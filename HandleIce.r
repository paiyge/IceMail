## "D:/safescripts/SendSeaIceData/rCode/HandleIce.r"
## Author: A.S. Fischbach (afischbach@usgs.gov)
## USGS Alaska Science Center
## version: 0.1.1
## R version 3.3.3 (2017-03-06) or higher, Copyright (C) 2016 The R Foundation for Statistical Computing.
## License: This code is dedicated to the public domain and all rights are waived to the work worldwide under copyright law, 
## including all related and neighboring rights, to the extent allowed by law.
## You can copy, modify, distribute and perform the work, even for commercial purposes, all without asking permission. 
###########################################################################################################################################
StartTime <- Sys.time()   
Today <- Sys.Date()
Yesterday <- Today - 1

## Contributed package Dependencies 
library(marmap)
library(terra)
library(sf) # To manipulate geographic (vector) data
library(stars) # To manipulate geographic (raster) data
library(dplyr)
library(terra)
library(ggplot2)
library(ptolemy)
require(sendmailR)	## Require the sendMailR package for access to mime e-mail procedures
pngPackageLoaded<-require(png)  ## Require for processing of logos in the plotting maps

### USER DEFINED SETTINGS
TrustMODIS = F								## Temporary flag to indicate if kml MODIS links may be trusted...
Testing=T									## Set to TRUE to send the e-mail only to the sender.
debugging = T								## Set to TRUE when debugging
ExtraNotice = T  ## Set to TRUE if there is an extra message to convey
Notice = paste('<p><strong>Please note: </strong>NASA MODIS imagery is no longer available for download due to a change in their server system. It may be accessed via <a href="https://go.nasa.gov/2JSxWcP"> the NASA web mapping application.</a></p>', sep='')  
NIC_MIZ.retry.interval = 300  				## Number of seconds between attempts to download the NIC MIZ product.  
NIC_MIZ.attempts = 16  					  
baseDir <- '/Users/paigenorris/Desktop/safescripts/SendSeaIceData/rCode'

### PATHWAYS
UserData <- suppressWarnings(read.csv(file = file.path(baseDir, 'UserData.csv'), as.is = T)[1,])	
Sender <- UserData$Sender  					
SenderAltEmail <- UserData$SenderAltEmail  		## Diagnostic messages will be sent here as well as to the primary Sender address.
eMailServer <-  UserData$eMailServer			## This address must be obtained from your email system administrator.
emailHTML <- file.path(baseDir, 'rCode', 'msgTxt.html')  							## Location of the HTML e-mail template for the full data mailing
emailHTML_plotsOnly <- file.path(baseDir, 'rCode', 'msgTxt_MapOnly.html')  		 	## Location of the HTML e-mail template for the maps only mailing
LogFile<-file.path(baseDir, 'rCode', 'HandleIce.r.Rout') 		## File to which the 'Rout' file will be written
GISPantryLocation <- UserData$GISPantryLocation					## Base directory where downloaded sea ice data is to be saved. 	
netWorkFlagBase <- UserData$netWorkFlagBase		 				## Base directory available on the network from multiple 

## Sub-Paths 
LocalWork <- file.path(baseDir, "Work")   						
dir.create(LocalWork, showWarnings = FALSE)                     
LocalTemp <- '/Users/paigenorris/Desktop/Temp'
dir.create(LocalTemp, showWarnings = FALSE)
lf <- list.files(LocalWork, full.names=TRUE)
file.remove(lf[lf!=file.path(LocalWork, "maps")])

if(!Testing){  ## If we are testing send to a the list of testing recipients, else send to everyone on the full list.
  recipients_df<-read.csv(file.path(baseDir, 'Recipients.csv'), 
                          header = TRUE, stringsAsFactors = F)
}else{
  recipients_df<-read.csv(file.path(baseDir, 'RecipientsTesting.csv'), 
                          header = TRUE, stringsAsFactors = F)
}
## Draw logo
logoFile <- file.path(baseDir, 'large_USGS_vector_green.png') ## Set to empty string if no logo to plot
if(nchar(logoFile)>0 & pngPackageLoaded){ ## prepare the logo for printing
  logo <- readPNG(source = logoFile, native=T)
  logo.ratio <- ncol(logo)/nrow(logo)
  draw.logo <- T
}else{draw.logo <- F}

#### Set Success flags  
gotNIC_MIZ = FALSE				
gotAMSR = FALSE					
madeMODIS_kmz = FALSE

#### Projections							
xLimits <- sort(c(150, -130)) ## define the study area polygon using geographic coordinates (eastings WGS84)
yLimits <- sort(c(49, 78))
pts <- expand.grid(xLimits, yLimits) %>% as_tibble()
names(pts) <- c('x','y')
prj.StudyArea <- sf::st_crs("+proj=laea +lat_0=70 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
prj.StudyAreaPositive <- sf::st_crs("+proj=laea +lat_0=70 +lon_0=190")
prj.geo <- sf::st_crs('+proj=longlat +datum=WGS84')

pts_bbox <- pts %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>%
  st_transform(crs = prj.StudyArea) %>%
  st_union() %>%
  st_convex_hull()

land_ptolemy <- pts_bbox %>%
  extract_gshhg(resolution = "i", epsg = 3571, buffer = 5000, simplify = FALSE, warn = FALSE) %>%
  st_transform(crs = prj.StudyArea)

land_ptolemy %>% ggplot() + geom_sf()

#### BATHYMETRY (depth of water)
BathymetryDataFile <- file.path(baseDir, 'rCode/ETOPO1.rData') 
if(!file.exists(BathymetryDataFile)){
  # Download bathy data. Warning: the downloaded area should be centered on the 
  # antimeridian. Otherwise, the left and right sides of the grid won't have the 
  # same  resolution. The difference is minor, but it is enough for st_as_stars()
  # to complain
  
  bath <- getNOAA.bathy(-120, 120, 47, 85, res = 10, antimeridian = TRUE, keep = TRUE)# Subset the bathy to get the desired area
  bath_ok <- subsetBathy(bath, x = c(160, 240), y = c(47, 85), locator = FALSE)# Transform to stars object and set crs
  bath_stars <- st_as_stars(as.xyz(bath_ok))
  st_crs(bath_stars) <- "EPSG:4326"
  
  # Extract isobath (vecto) lines form the stars (raster) object
  cont <- st_contour(bath_stars, contour_lines = TRUE, 
                     breaks = c(seq(-6000, -1000, 500), -200, 0))
  
  # Define projection and plot with ggplot2 and sf
  proj <- "+proj=laea +lat_0=70 +lon_0=190"
  pl <- ggplot() +
    geom_sf(data = cont, alpha = 0.2) +
    geom_sf(data = cont %>% filter(V3 == 0)) +
    geom_sf(data = cont %>% filter(V3 == -200), color = "red") +
    theme_minimal() 
  # Unprojected
  pl
  # Projected
  pl + coord_sf(crs = prj.StudyAreaPositive)
  
}else{load(BathymetryDataFile)}

### NIC MIZ 
DestFile.zip <- file.path(LocalTemp, format(Today, 'nic_miz%Y%jnc_pl_a.zip'))
DestFile.loc <- file.path(GISPantryLocation, format(Today, 'NIC_MIZ/%Y'))
DestFile <- file.path(DestFile.loc, format(Today, 'nic_miz%Y%jnc_pl_a.shp'))

if(!file.exists(DestFile) | file.info(DestFile)$size < 1000){
  base = 'https://www.natice.noaa.gov/pub/daily/arctic/'
  cat('\n-----------> Attempting to download', format(Yesterday, '%Y %B %d'), fill=T)
  URL <-  "https://usicecenter.gov/File/Download?fName=daily_miz_n.zip" 
  MoveOn = FALSE  ## flag to move on from trying the NICMIZ
  n <- 1 ## set counter
  n.try <- NIC_MIZ.attempts ## set number of attempts
  while(MoveOn==FALSE ){
    ds <- 1
    cat('... download attempt', n, 'of', n.try, fill=T)
    try(ds<-download.file(url=URL, 
                          destfile=DestFile.zip, 
                          method="libcurl", 
                          quiet = F, mode = "wb", cacheOK = F))
    if(ds==0 & file.size(DestFile.zip) < 1000) {ds<-1} ## flag ds to 1 if the file is too small
    if(ds==0) { ## If download successful handle the zip file
      uz <- unzip(zipfile=DestFile.zip, 
                  exdir=file.path(LocalTemp, format(Yesterday, '%j')), 
                  overwrite=T) ## unzip 
      for(z in uz) {
        cat('...unzipped', z, fill=T)
        fc <- file.copy(from=z, to=DestFile.loc, overwrite=T)
        if(fc){cat('--- copied to', DestFile.loc, fill = T)
        }else{ cat('!!! -> Copy of', z, 'to', DestFile.loc, 'FAILED', fill = T)
        }
      }
      MoveOn=TRUE
    }else{
      n = n+1
      if(n<n.try){
        cat('... pausing execution for', NIC_MIZ.retry.interval, 
            'seconds, awaiting availability of', URL, fill = T)
        Sys.sleep(NIC_MIZ.retry.interval)  ## wait NIC_MIZ.retry.interval seconds then try again
      }else{
        MoveOn=TRUE
      }
    }
  }
}else{
  cat('--->', DestFile, 'has already been downloaded and handled. Moving on.',
      fill=T)
}
#	End 				
if(file.exists(DestFile) & file.info(DestFile)$size > 1000){
  gotNIC_MIZ <- TRUE 												
  cat('... reading in the', format(Today, 'nic_miz%Y%jnc_pl_a'), 'shape file', fill = T)
  
  MIZ_d <- st_read(dsn=DestFile.loc, layer=format(Today, 'nic_miz%Y%jnc_pl_a')) %>% 
    st_make_valid() %>%
    st_transform(prj.StudyArea)
  MIZ_p.lines <- MIZ_d %>% 
    filter(ICECODE=='CT81') %>% st_cast("LINESTRING") %>% st_intersection(pts_bbox)
  MIZ_m.lines <- MIZ_d %>% 
    filter(ICECODE=='CT18') %>% st_cast("LINESTRING") %>% st_intersection(pts_bbox)
  
  st_simplify(land_ptolemy, preserveTopology = TRUE, dTolerance = 100)
  landAll.buffer<-st_buffer(land_ptolemy, 4000)
  
  MIZ_p.l.clip <- st_difference(MIZ_p.lines, landAll.buffer, byid=TRUE)
  MIZ_p.l.clip_simp <- st_simplify(MIZ_p.l.clip, preserveTopology = TRUE, dTolerance = 100)
  MIZ_m.l.c <- st_difference(MIZ_m.lines, landAll.buffer, byid=TRUE)
  MIZ_m.l.clip <- st_difference(MIZ_m.l.c, MIZ_p.l.clip_simp)
} 

##  AMSR2 - PASSIVE MICROWAVE
geoTiffFile<-file.path(GISPantryLocation, format(Yesterday,'AMSR2/%Y/asi-AMSR2-n6250-%Y%m%d-v5.4.tif'))
if(!file.exists(geoTiffFile) | file.info(geoTiffFile)$size < 100000){ 
  base = "https://seaice.uni-bremen.de/data/amsr2/asi_daygrid_swath/n6250"
  cat('\n-----------> Attempting to download', format(Yesterday, '%Y %B %d'), fill=T)
  cat(URL <- paste(base, tolower(format(Yesterday,'/%Y/%b/')), 
                   'Arctic/', format(Yesterday,'asi-AMSR2-n6250-%Y%m%d-v5.4.tif') , sep =''), fill =T)
  
  ## download file and tests to make sure its the right file size
  MoveOn=FALSE  	## flag to move on from trying the NICMIZ
  n <-1 			## set counter
  n.try <- 3 		## set number of attempts to be made
  while(MoveOn==FALSE ){
    ds <- 1
    cat('... download attempt', n, 'of', n.try, fill=T)
    try(ds<-download.file(url=URL, destfile=geoTiffFile, method="libcurl", quiet = F, mode = "wb", cacheOK = F))
    if(ds==0 & file.size(geoTiffFile)<1000) {ds<-1}
    if(ds==0) {
      MoveOn=TRUE
    }else{
      n=n+1
      if(n<n.try){
        cat('... pausing execution for', NIC_MIZ.retry.interval, 'seconds, awaiting availability of', URL, fill = T)
        Sys.sleep(NIC_MIZ.retry.interval)  ## wait NIC_MIZ.retry.interval seconds then try again
      }else{MoveOn=TRUE}
    }
  }
}else{cat('--- >', geoTiffFile, 'has already been downloaded.  Moving on.', fill=T)}

if(file.exists(geoTiffFile) & file.info(geoTiffFile)$size > 90000){ 
  gotAMSR <- TRUE					## Flag for existence of the AMSR data
  AMSR2gTif <- rast(geoTiffFile)
  x<-values(AMSR2gTif)			## Pull the values from the raster so that it may be tidied up.
  x[x==120] <- NA 							## set all non-ice to NA
  x[x==0] <- NA    							## set all 0 ice to NA
  
  values(AMSR2gTif) <- x						## Place the tidy raster data back into the AMSR raster.
  rm(x)										## Remove the temporary matrix of raster data used in the tidying process.
  
  ## project the clip polygon to this raster projection
  ClipStudyAreap2<-st_transform(pts_bbox, st_crs(AMSR2gTif))
  study_area_subset <- crop(AMSR2gTif, ClipStudyAreap2)
  
  ## Clip and project the AMSR raster to an all positive projected Cartesian coordinate system.
  rr2 <- terra::project(study_area_subset, "+proj=laea +lat_0=70 +lon_0=190", method="bilinear", res=6250)
  rr.geo <- terra::project(rr2, "+proj=longlat +datum=WGS84", method='near', over=TRUE) ##Project to lat lon
  
  ## Prepare for plotting
  study_area_buf <-crop(AMSR2gTif, st_buffer(ClipStudyAreap2, 250*1000))
  AMSR <- project(study_area_buf,
                  "+proj=laea +lat_0=70 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", method="near", res=6500)
} else {cat('\nNo success getting AMSR', geoTiffFile, 'file not found.', fill = T)}

## BUILD Browse Map based on AMSR2 6.5 KM data
e <- terra::ext(land_ptolemy)  														## Cast as a raster extent object
BrowseMap.file<-format(Yesterday, 'BrowseMap_%Y_%m_%d.png')  ## Declare the name of the browse map
dir.create(file.path(LocalWork, 'maps'), showWarnings=F)
BrowseMap.filePath<-file.path(LocalWork, 'maps', BrowseMap.file)							## Declare the browse map file name
if(gotAMSR){
  study_area_buf <- crop(AMSR2gTif, st_buffer(ClipStudyAreap2, 350*1000))
  AMSR.r <-project(x=study_area_buf,
                  y="+proj=laea +lat_0=70 +lon_0=-170 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", 
                  method="near", res=6500)
  r<-AMSR.r
  r[r<0.1499] <- NA															## Set all values less than 15% to NA
}
cont.sa <- terra::crop(cont, e)
MIZ_p.l.clip <- terra::crop(MIZ_p.l.clip, e)
MIZ_m.l.clip <- terra::crop(MIZ_m.l.clip, e)

r.sa <- crop(bath.r, e)
values(r.sa) <- NA
r.saPositive <- project(r.sa, crs=prj.StudyAreaPositive)

## Skip MODIS download until new method for accessing MODIS is devised.  <- luna package?
getMODIS <- F
Aqua <- F
Terra <- F

## plot maps
h <- 620  
w <- 1240 

widthFullMap <- w*(1 + (Aqua + Terra))/2
if(!Aqua & !Terra){
  cexScale <- 5/8
}else{cexScale <- 9/8}

## Build scale bar
xStart <- xmax(e) - 650*1000
yStart <- ymin(e) + 50*1000
scaleBar <- data.frame(cbind(x=c(rep(xStart+ 100*1852, 2), rep(xStart, 2), xStart+ 100*1852), y = c(yStart, rep(yStart+50*1000, 2), rep(yStart, 2))))
scaleBar100.sp = SpatialPolygons(list(Polygons(list(Polygon(scaleBar)),1)))

scaleBar<-data.frame(cbind(x=c(rep(xStart+ 300*1852, 2), rep(xStart, 2), xStart+ 300*1852), y = c(yStart, rep(yStart+50*1000, 2), rep(yStart, 2))))
scaleBar300.sp = SpatialPolygons(list(Polygons(list(Polygon(scaleBar)),1)))
rm(scaleBar)
#
if(gotAMSR) {
  AMSR2text<- paste( 'AMSR2 image\n',format(Yesterday, '%Y %b %d'),  sep='')
  labelText<-paste('Gray scale pixels indicate sea ice', '\r\n', 'revealed by a microwave reflectance', 
                   '\r\nsensor that can "see" through', '\r\nclouds with a resolution of 6.25 Km.', sep='')
}else{
  AMSR2text<-paste( 'NO AMSR2 image\n',format(Yesterday, '%Y %b %d'),  sep='')
  labelText<-''
}
if(gotNIC_MIZ) {labelText<-paste(labelText, '\r\n', '\r\n', 'National Ice Center Chart\r\n', 'Yellow lines = Marginal Ice Zone', '\r\n', sep='')}
(labelText<-paste(labelText, 'Red lines = Pack Ice Zone', '\r\n\r\n', 'Blue water and thin blue line \nindicate continental shelf (<200m)', sep=''))
shelfColor<-'cadetblue4' ## #9190FF 
landColor<-'#574835'#'burlywood4'  ## burlywood4

## Plot 1
plot(bath.r, ext=e, col=c("black", shelfColor, landColor), 
     breaks=c(-6000, -200, 0, 6000),  	## Plot the ocean depth
     legend=FALSE,  ann=FALSE, 
     xaxt='n', yaxt='n', asp = 1, bty='n')
if(gotAMSR) {
  plot(r, ext=e, col=gray.colors(85, start = 0.15, end = 1, gamma = 2.2), 
       bty='n', legend=FALSE,  add=T)}		## Plot the AMSR raster
if(gotNIC_MIZ) lines(MIZ_p.l.clip,  col='#ff2222f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines spLPIZ 
if(gotNIC_MIZ) lines(MIZ_m.l.clip,  col='#ffff00f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines  spLMIZ
lines(bath200.sa, col='cadetblue4', lwd=3, bty='n')			## Continental shelf
text(x=580000, y=60000, labels=AMSR2text, col='#ffff00f0', cex= 1.85*cexScale,  adj=0) ## original cex= 1.85
text(x=580000, y=-385000, labels=labelText, col='snow1', adj=0, cex=1.25*cexScale) ## original cex= 1.25
plot(scaleBar100.sp, border='cadetblue4', add = T)
plot(scaleBar300.sp, border='cadetblue4', add = T)
text(x = xStart+100*1852, y=yStart + 79*1000, adj=0, label='100', cex = 1.05*cexScale, col=shelfColor)
text(x = xStart+300*1852, y=yStart + 79*1000, adj=0, label='300', cex = 1.05*cexScale, col=shelfColor)
text(x = xStart+105*1852, y=yStart + 30*1000, adj=0, label='Nautical Miles', cex = 0.9*cexScale, col=shelfColor)
rasterImage(image=logo,
            xleft=xmin(e), ybottom=ymax(e)-15000/logo.ratio, xright=xmin(e) + 150000, ytop=ymax(e))
box(col='black', lwd=1.5)
## Plot 2
if(Aqua){
  plot(bath.r, ext=e, col=c("black", 'cadetblue4', 'burlywood4'), 
       breaks=c(-6000, -200, 0, 6000),  	## Plot the ocean depth
       legend=FALSE,  ann=FALSE, 
       xaxt='n', yaxt='n', asp = 1, bty='n')
  if(download.statusABS==0) plotRGB(BS.aqua.sa, add = T, bgalpha = 0) ##
  if(download.statusACh==0) plotRGB(BSW.aqua.sa, add = T, bgalpha = 0) ##
  if(download.statusABeaSE==0) plotRGB(BSE.aqua.sa, add = T, bgalpha = 0) ##
  if(download.statusAAI==0) plotRGB(AI.aqua.sa, add = T, bgalpha = 0) ##
  if(download.statusAGA==0) plotRGB(GA.aqua.sa, add = T, bgalpha = 0) ##
  if(gotNIC_MIZ) lines(MIZ_p.l.clip,  col='#ff2222f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines spLPIZ 
  if(gotNIC_MIZ) lines(MIZ_m.l.clip,  col='#ffff00f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines  spLMIZ
  plot(bath.r, ext=e, col=landColor, #rgb(222/255, 184/255, 135/255, alpha=.4)
       breaks=c(0, 6000),  legend=FALSE, add = T)	## Plot the ocean depth
  lines(bath200.sa, col=shelfColor, lwd=2, bty='n')			## Continental shelf
  text(x=580000, y=60000-200000, adj=c(0,0), ##x = -650000
       labels=paste('NASA MODIS image\n', format(Yesterday, '%Y %b %d'), ' from the\n', ifelse(Aqua, 'Aqua', 'Terra'), ' satellite', sep=''),  col='#ffff00f0', cex=1.85)
  text(x=580000, y=-350000, adj=0, labels='In this false color image,\nsea ice appears blueish,\nwhile clouds appear white\nand open water appears black.'  , col='snow1', cex=1.25)
  box(col='black', lwd=1.5)
}	
## Plot 3
if(Terra){
  plot(bath.r, ext=e, col=c("black", shelfColor, landColor), 
       breaks=c(-6000, -200, 0, 6000),  	## Plot the ocean depth
       legend=FALSE,  ann=FALSE, 
       xaxt='n', yaxt='n', asp = 1, bty='n')
  if(download.statusTBS==0) plotRGB(BS.terra.sa, add = T, bgalpha = 0) ##
  if(download.statusTCh==0) plotRGB(BSW.terra.sa, add = T, bgalpha = 0) ##
  if(download.statusTBeaSE==0) plotRGB(BSE.terra.sa, add = T, bgalpha = 0) ##
  if(download.statusTAI==0) plotRGB(AI.terra.sa, add = T, bgalpha = 0) ##
  if(download.statusTGA==0) plotRGB(GA.terra.sa, add = T, bgalpha = 0) ##
  if(gotNIC_MIZ) lines(MIZ_p.l.clip,  col='#ff2222f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines spLPIZ 
  if(gotNIC_MIZ) lines(MIZ_m.l.clip,  col='#ffff00f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines  spLMIZ
  plot(bath.r, ext=e, col=landColor, breaks=c(0, 6000),  legend=FALSE, add = T)			    ## Plot the land
  lines(bath200.sa, col=shelfColor, lwd=3, bty='n')			## Continental shelf
  text(x=580000, y=60000-200000, adj=c(0,0), 
       labels=paste('NASA MODIS image\n', format(Yesterday, '%Y %b %d'), ' from the\n', 'Terra satellite', sep=''),  col='#ffff00f0', cex=1.85)
  text(x=580000, y=-350000, adj=0, labels='Both MODIS images are provided\nhere, so that you may choose which\none to open from the kmz data file.'  , col='snow1', cex=1.25)
  if(draw.logo){
    rect(xleft=xmin(e), ybottom=(ymax(e)-570000/logo.ratio), xright=(xmin(e) + 570000), ytop=ymax(e), 
         density = -1, col = 'white', border = 'white', lwd=1.5)}
  if(draw.logo){
    rasterImage(image=logo, xleft=xmin(e), ybottom=(ymax(e)-570000/logo.ratio), xright=(xmin(e) + 570000), ytop=ymax(e))}
  box(col='black', lwd=1.5)
}
dev.off()
MODISBrowseSingle.file<-format(Yesterday,'IceMap_%Y_%m_%d.jpg'   )
MODISBrowseSingle.filePath<-file.path(baseDir, 'MODIS/Browse', MODISBrowseSingle.file) 

w <- ifelse( Aqua | Terra, w, w/2)
jpeg(filename = MODISBrowseSingle.filePath, width = w, height = h, quality = 90)
if(Aqua | Terra){
  par(mfrow=c(1,2)) 
}else{
  par(mfrow=c(1,1))
}
par(mar=c(0,0,0,0))
par(bg='black')

## Plot 1
plot(bath.r, ext=e, col=c("black", shelfColor, landColor), 
     breaks=c(-6000, -200, 0, 6000),  	## Plot the ocean depth
     legend=FALSE,  ann=FALSE, 
     xaxt='n', yaxt='n', asp = 1, bty='n')
if(gotAMSR) {
  plot(r, ext=e, col=gray.colors(85, start = 0.15, end = 1, gamma = 2.2), 
       bty='n', legend=FALSE,  add=T)}		## Plot the AMSR raster
if(gotNIC_MIZ) lines(MIZ_p.l.clip,  col='#ff2222f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines spLPIZ 
if(gotNIC_MIZ) lines(MIZ_m.l.clip,  col='#ffff00f0', lwd=2, bty='n')						## If we have NIC MIZ, plot the lines  spLMIZ
lines(bath200.sa, col='cadetblue4', lwd=2, bty='n')			## Continental shelf
text(x=580000, y=60000, labels=AMSR2text, col='#ffff00f0', cex=1.35, adj=0)
text(x=580000, y=-385000, labels=labelText, col='snow1', adj=0, cex=0.9)	
plot(scaleBar100.sp, border=shelfColor, add = T)
plot(scaleBar300.sp, border=shelfColor, add = T)
text(x = xStart+100*1852, y=yStart + 79*1000, adj=0, label='100', cex = .9, col=shelfColor)
text(x = xStart+300*1852, y=yStart + 79*1000, adj=0, label='300', cex = .9, col=shelfColor)
text(x = xStart+105*1852, y=yStart + 30*1000, adj=0, label='Nautical Miles', cex = .8, col=shelfColor)
box(col='black', lwd=1.5)
## Plot 2
if(Aqua | Terra){
  plot(bath.r, ext=e, col=c("black", shelfColor, landColor), 
       breaks=c(-6000, -200, 0, 6000),  	## Plot the ocean depth
       legend=FALSE,  ann=FALSE, 
       xaxt='n', yaxt='n', asp = 1, bty='n')
  if(Aqua){
    if(download.statusABS==0) plotRGB(BS.aqua.sa, add = T, bgalpha = 0) ##
    if(download.statusACh==0) plotRGB(BSW.aqua.sa, add = T, bgalpha = 0) ##
    if(download.statusABeaSE==0) plotRGB(BSE.aqua.sa, add = T, bgalpha = 0) ##
    if(download.statusAAI==0) plotRGB(AI.aqua.sa, add = T, bgalpha = 0) ##
    if(download.statusAGA==0) plotRGB(GA.aqua.sa, add = T, bgalpha = 0) ##
  }else{
    if(Terra){
      if(download.statusTBS==0) plotRGB(BS.terra.sa, add = T, bgalpha = 0) ##
      if(download.statusTCh==0) plotRGB(BSW.terra.sa, add = T, bgalpha = 0) ##, addfun=plot(land.StudyArea, lwd=2, border='green', add = T)
      if(download.statusTBeaSE==0) plotRGB(BSE.terra.sa, add = T, bgalpha = 0) ##
      if(download.statusTAI==0) plotRGB(AI.terra.sa, add = T, bgalpha = 0) ##
      if(download.statusTGA==0) plotRGB(GA.terra.sa, add = T, bgalpha = 0) ##
    }
  }
  if(gotNIC_MIZ) lines(MIZ_p.l.clip,  col='#ff2222f0', lwd=2.1, bty='n')						## If we have NIC MIZ, plot the lines spLPIZ 
  if(gotNIC_MIZ) lines(MIZ_m.l.clip,  col='#ffff00f0', lwd=2.1, bty='n')						## If we have NIC MIZ, plot the lines  spLMIZ
  plot(bath.r, ext=e, col=landColor, breaks=c(0, 6000),  legend=FALSE, add = T)				## Plot the land
  lines(bath200.sa, col=shelfColor, lwd=3, bty='n')											## Continental shelf
  text(x=580000, y=60000-200000, adj=c(0,0), ##x = -650000
       labels=paste('NASA MODIS image\n', format(Yesterday, '%Y %b %d'), ' from the\n', ifelse(Aqua, 'Aqua', 'Terra'), ' satellite', sep=''),  col='#ffff00f0', cex=1.35)
  text(x=580000, y=-350000, adj=0, labels='In this false color image,\nsea ice appears blueish,\nwhile clouds appear white\nand open water appears black.'  , col='snow1', cex=0.9)	
  box(col='black', lwd=1.5)
  if(draw.logo) {
    rect(xleft=xmin(e), ybottom=(ymax(e)-570000/logo.ratio), xright=(xmin(e) + 570000), ytop=ymax(e), 
         density = -1, col = 'white', border = 'white', lwd=1.5)}
  if(draw.logo) {
    rasterImage(image=logo, xleft=xmin(e), ybottom=(ymax(e)-570000/logo.ratio), xright=(xmin(e) + 570000), ytop=ymax(e))}
}
dev.off()

## Prepare e-mail attachments
i = 1  ## Increment for the mime_part attachment list
iD = 1
attachments<-list()  ## mime_part attachment list
attachmentsD<-list()  ## mime_part attachment list

## NIC MIZ Maps & Data
if(gotNIC_MIZ){
  cat('attaching', MIZkmz.filePath, 'as', MIZkmz.file, 'in position', iD, 'of attachmentsD', fill=T)
  attachmentsD[[iD]]=list(mime_part(x=MIZkmz.filePath, name=MIZkmz.file))  										## attach data
  iD = iD + 1  ## Increment the mime_part list counter
  ## } else {
  ## Adjust body of NIC MIZ e-mail body
}

## AMSR Data
if(gotAMSR){
  AMSRFile<-format(Yesterday,'asi-AMSR2-n6250-%Y%m%d-v5.kmz')	
  cat('attaching', AMSRkmzFile, 'as', 	AMSRFile, 'in position', iD, 'of attachmentsD',fill=T)						
  attachmentsD[[iD]]=list(mime_part(x=AMSRkmzFile, name=AMSRFile))	## attach map
  iD = iD + 1  ## Increment the mime_part list counter
}

## Prepare to send the mail				
sendIceMailHTML<-function(From=Sender, 
                          To=Sender, 
                          Subject = 'IceMail', 
                          BodyHTMLfile = emailHTML, 
                          Attachments=NULL, SingleMap=F, gotNIC_MIZFlag=gotNIC_MIZ, gotAMSRFlag=gotAMSR,
                          smtpS = eMailServer
){
  tt<-readChar(BodyHTMLfile, file.info(BodyHTMLfile)$size)
  tt<-sub(pattern=' today', replacement=format(Today, ' %Y %B %d'), x=tt)
  if(SingleMap==T){
    tt<-sub(pattern='IceMap_YYYY_MM_DD.jpg', replacement=MODISBrowseSingle.file, x=tt)
  }else{
    tt<-sub(pattern='IceMap_YYYY_MM_DD.jpg', replacement=MODISBrowse.file, x=tt)
  }
  ## Replace links to the NASA MODIS kmz files.
  tt<-gsub (pattern='Today_YYYYjjj', replacement=format(Today, '%Y%j'), x=tt)
  tt<-gsub (pattern='Today_YYYYMMDD', replacement=format(Today, '%Y%m%d'), x=tt)
  tt<-gsub (pattern='Yesterday_YYYYjjj', replacement=format(Yesterday, '%Y%j'), x=tt)
  tt<-sub(pattern='Today_asDateFormat', replacement=format(Today, '%B %d, %Y'), x=tt)
  tt<-sub(pattern='Yesterday_asDateFormat', replacement=format(Yesterday, '%B %d, %Y'), x=tt)
  
  if(gotNIC_MIZFlag) 
  {tt<-sub(pattern='NIC_MIZ_LineYYYYJJJ.kmz', replacement=MIZkmz.file, x=tt)
  }else{tt<-sub(pattern='NIC_MIZ_LineYYYYJJJ.kmz', replacement='No NIC/MIZ data today', x=tt)}
  
  if(gotAMSRFlag){
    tt<-sub(pattern='asi-AMSR2-n6250-YYYYMMDDv5.kmz', replacement=AMSRFile, x=tt)
  }else{tt<-sub(pattern='asi-AMSR2-n6250-YYYYMMDDv5.kmz', replacement='No AMSR data today', x=tt)}
  
  if(ExtraNotice){
    tt<-sub(pattern='<!-- Notice here -->', replacement=Notice, x=tt)
  }
  BodyHTML <- mime_part(tt)
  BodyHTML[["headers"]][["Content-Type"]] <- "text/html" ##Override content type.
  bodyWithAttachment <- list(BodyHTML)  
  for(a in Attachments){bodyWithAttachment<-c(bodyWithAttachment, a)}
  #bodyWithAttachment
  cat('==> Sending mail to', length(To), 'recipients', fill=T)
  for(T in 1:length(To)){
    TO<-To[[T]]
    cat(TO, fill = T)
    sendmail(from=From,to=TO,subject=Subject,msg=bodyWithAttachment,control=list(smtpServer=smtpS,  verboseShow=TRUE))
  }
}
#
## Test for existence of netWorkFlagForSuccess	
if(file.exists(netWorkFlagForSuccess)) {
  sucessSendingEmails <- any(grepl(readLines(netWorkFlagForSuccess), pattern = "E-mails sent"))
} else {
  sucessSendingEmails <- FALSE
}

if(!sucessSendingEmails){
  ## Maps Only
  Recipients.map<-recipients_df[recipients_df$getData==0 & !(recipients_df$Monday==1),]$Recipients
  if(length(Recipients.map)>0){ 	
    cat('\n\nSending Maps', fill=T)
    sendIceMailHTML(From=Sender, To=Recipients.map, Subject= 'IceMail: maps only', BodyHTMLfile = emailHTML_plotsOnly, SingleMap=T, Attachments=attachments)		
    cat(SinkMsg<-paste('\n------------------------------------------------------------------------------------------------------------\n',
                       '==> E-mails sent.  <==\n', 'Map only e-mails sent to', length(Recipients.map), 'recipients\n'))
  }
  ## Monday Maps Only
  if(format(Today, '%a')=='Mon'){## it is Monday
    Recipients.Monday<-recipients_df[recipients_df$getData==0 & recipients_df$Monday==1,]$Recipients
    if(length(Recipients.Monday)>0){ ## There are Recipients with a request for data
      cat('\n\nSending Monday Maps', fill=T)
      sendIceMailHTML(From=Sender, 
                      To=Recipients.Monday,
                      Subject= 'IceMail: Monday maps only', 
                      BodyHTMLfile = emailHTML_plotsOnly, SingleMap=T, Attachments=attachments)
      cat(SinkMsg<-paste(SinkMsg, 'Monday Map only e-mails sent to', length(Recipients.Monday), 'recipients\n'))
    }
  }
  ## Maps & Data
  Recipients.Data <- recipients_df[recipients_df$getData==1 & recipients_df$Monday==0,]$Recipients
  if(length(Recipients.Data)>0){ ## There are Recipients with a request for data
    cat('\n\nSending Data', fill=T)
    sendIceMailHTML(From=Sender, To=Recipients.Data, Subject= 'IceMail: maps & data', BodyHTMLfile = emailHTML, Attachments=attachmentsD)		
    cat(SinkMsg<-paste(SinkMsg, 'Full data e-mails sent to', length(Recipients.Data), 'recipients\n'))
  }
  cat('Sending emails completed at', format(Sys.time(), '%Y%B%d %H:%M'), fill=T)
  SentEmails <- T
}else{
  cat(SinkMsg<-paste('\n--------------------------------------------------------------------------------------\n',
                     'No e-mails sent.', '\nThe flag file', '\n', netWorkFlagForSuccess, '\r\n', 'had already been written today, indicating success sending emails.', sep=''), fill=T)
  SentEmails <- F
} ## end If check for email success.
## Send diagnostic e-mails
si<-sessionInfo()
cat(SinkMsg<-paste(SinkMsg, '\n',
                   'Started at', format(StartTime, '%Y%b%d %H:%M'), '\n',
                   'Completed at:', format(Sys.time(), '%Y%b%d %H:%M'), '\n',
                   '---> Got NIC MIZ:', gotNIC_MIZ, '\n',
                   '---> Got AMSR:', gotAMSR, '\n',
                   '---> Testing:', Testing, '\n',
                   '---> Sent from the machine ', Sys.info()[4], '\n     running ', Sys.info()[1], Sys.info()[2], Sys.info()[3], 
                   '\n     as a', Sys.info()[5], 'machine logged in as ', Sys.info()[6],
                   '\n    ', si$R.version$version.string, 
                   '\n    rgdal:', si$otherPkg$rgdal$Version, 
                   '\n    sp:', si$otherPkg$sp$Version,
                   '\n--------------------------------------------------------------------------------------\n'))
sink(netWorkFlagForSuccess, append = T)
cat(SinkMsg)
sink() ## end logging to sink		
if(file.exists(LogFile)){
  LogFileTxt<-paste(LogFile, '.txt', sep='')
  file.copy(from=LogFile, to=LogFileTxt, overwrite=T)
  ## add to the attachments
  attachments[[i]]=list(mime_part(x=LogFileTxt, 'HandleIce.r.Rout.txt'))
  i = i + 1
  attachmentsD[[iD]]=list(mime_part(x=LogFileTxt, 'HandleIce.r.Rout.txt'))
  iD = iD + 1  ## Increment the mime_part list counter
}
attachmentsD[[iD]]=list(mime_part(x=netWorkFlagForSuccess, format(Today, '%Y%b%d.txt')))
attachments[[i]]=list(mime_part(x=netWorkFlagForSuccess, format(Today, '%Y%b%d.txt')))
## Complete by sending a copy to the sender and SenderAltEmail
sendIceMailHTML(From=Sender, Subject= 'IceMail: maps, data & diagnostics',
                To = Sender, Attachments=attachmentsD)
if(SentEmails){
  sendIceMailHTML(From=Sender, 
                  To=c(Sender, SenderAltEmail), Subject= 'IceMail: maps & diagnostics',BodyHTMLfile = emailHTML_plotsOnly,
                  SingleMap=T, Attachments=attachments)
}else{
  sendmail(from=Sender,to=SenderAltEmail,subject='IceMail: diagnostics', msg=SinkMsg, control=list(smtpServer=eMailServer,  verboseShow=FALSE))
}
### END
