## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# Adding a minor


## ----Library, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------
# Remove objects from memory
rm(list=ls())

# Load required libraries
library(raster)
library(rgdal)
library(sp)


## ----Spatial Data, warning = F, eval=T---------------------------------------------------------------------------------------------------------------
# Load the point data and project to UTM32N
Addax <- readOGR(dsn="Data", layer="Occurrence_dd")
Addax

# Project to UTM32N, WGS84
# See: https://spatialreference.org/
UTM32N.proj <- "+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs"
Addax <- spTransform(Addax, CRS = UTM32N.proj) # Note that I am overwriting the original Lat/Long object

# Look at the file
head(Addax)

# Is the file projected?
projection(Addax)

# Plot - Not much context, but we at least can verify that the file loaded correctly
# We'll overlay on some base layers
#plot(Addax)


## ----Raster Data, eval=T-----------------------------------------------------------------------------------------------------------------------------
# NDVI - Normalized Difference Vegetation Index (250-m)
# ********************************************
# ********************************************
# Note the file is already projected to UTM32N WGS84
ndvi <- raster("Data/MOD13Q1_Nov2017.tif")
ndvi <- ndvi * 0.0001 # Convert to decimal values (This is done to reduce the file size)
ndvi

# SRTM - Elevation data from the Shuttle Radar Topography Mission (90-m)
# ********************************************
# ********************************************
srtm <- getData('SRTM', lon=12, lat=16.5) # Inputting general coordinates to grab tile
srtm

# Clip (crop) the raster to the study area extent. This will help with processing the image
# Read in a study area boundary (also in Lat/Long).
SA <- readOGR(dsn="Data", layer="StudyArea")
plot(srtm)
plot(SA, add=T)

# Crop the raster to the study area extent
# You could also manually set the extent, if inclined to do so (SA <- extent(11,13,16,17))
srtm <- crop(srtm,SA)

# Note: In many cases, you might need to download multiple tiles to cover your study area.  To put them together, you'd need to use the mosaic function.  Be careful, rasters can get large.
#srtm1 <- getData('SRTM', lon = 12, lat = 16.5)
#srtm2 <- getData('SRTM', lon = 15, lat = 16.5)
#srtm.mosaic <- mosaic(srtm1,srtm2, fun=mean) # Using mean for overlapping region

# Multiple other options are available with the getData command (Worldclim, GADM, and future climate data - CMIP5)
# Worldclim:
  # Ramiro will be going over the bioclim variables in the next few weeks

# Global Administrative Boundaries (GADM):
# Sometimes very useful to verify you are working where you think you are
Niger <- getData('GADM', country = "Niger", level = 0) 

# Plot together - Overlapping where I think they should!
plot(Niger)
plot(srtm, add=T)


## ----Project Raster, warning=FALSE-------------------------------------------------------------------------------------------------------------------
# Project the srtm image, setting the resolution
srtm.utm <- projectRaster(srtm, res = 90, crs = UTM32N.proj, method = 'bilinear') # This can be a slow process, depending on the size of the raster

# Resample the srtm image to the ndvi image
# The native resolution of our analysis will then be 250m - This will help to speed up the processing, but means we are losing some of the fine-scale information contained in the 90m elevation model which might be important
srtm.250m <- resample(srtm.utm, ndvi, "bilinear")

# Evaluate if both rasters have the same extent, rows/columns, projection, and resolution
compareRaster(srtm.250m, ndvi)

# You might need to `crop` the image, if the extents don't match
#ext <- extent(ndvi) # Smaller file
#example.crop <- crop(srtm.250m, ext)


## ----Questions, eval=F, echo=F-----------------------------------------------------------------------------------------------------------------------
## # Answers to questions:
## # 1) Print the name of the file to the R console.  This will provide information on the attributes of the file.  If projected, these details will appear in the output.  Use compareCRS() to determine if the projections match
## srtm.250m
## Addax
## compareCRS(srtm.250m,Addax)
## compareCRS(srtm,ndvi)
## 
## # 2) For raster and vector files, the function differs
## help(spTransform)
## help(projectRaster)
## 
## # 3) Plot your data.  R does not have projection on the fly capabilities, so if your data do not overlay, there is a problem.  See below for plotting of data.


## ----Plot Data---------------------------------------------------------------------------------------------------------------------------------------
# Plot the result (order matters)
plot(srtm.250m)
plot(Addax,pch=15,cex=0.7,add=TRUE)


## ----Roughness, eval = T-----------------------------------------------------------------------------------------------------------------------------
rough <- terrain(srtm.250m,opt='roughness')
plot(rough)
plot(Addax,pch=15,cex=0.7,add=TRUE)


## ----Extract, eval=T---------------------------------------------------------------------------------------------------------------------------------
r.stack <- stack(ndvi,rough)
#plot(r.stack)

# Extract NDVI and rough  
Extract <- extract(r.stack,Addax)
colnames(Extract) <- c("ndvi","ROUGH")

# Append to spatialobject
Addax <- cbind(Addax,Extract) 

# Look at the data
head(Addax)

