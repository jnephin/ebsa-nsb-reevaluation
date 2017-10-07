###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview:
# Exports maps of mean chla and bloom freqency with EBSA boundaries
#
###############################################################################

# Load packages
library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(RColorBrewer)

# Go to parent directory
setwd('..')


# -------------------------------------------------#
# load boundary polygon
nsb <- readOGR(dsn="Boundary", layer="NSB")
# Convert to spatial polygons (i.e., drop the data)
nsb <- as( nsb, "SpatialPolygons" )

# Load coastline shapefile
bcPoly <- readOGR( dsn="Boundary", layer="coast_sim" )
# Convert to spatial polygons (i.e., drop the data)
bcPoly <- as( bcPoly, "SpatialPolygons" )
# Project
bcPoly <- spTransform( bcPoly, proj4string(nsb))

# -------------------------------------------------#




# -------------------------------------------------#
# Load rasters
chla <- raster("Data/Productivity/Chla_mean_nsb.tif")
bloom <- raster("Data/Productivity/Bloom_freq_nsb.tif")

# -------------------------------------------------#

# reclass uncert raster
# all values >= 0 and <= 0.25 become 1, etc.
uncert <- raster("Data/Productivity/Uncertainty_straylight.tif")
m <- c(-1, 15, NA,  15, 16, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
interp_chla <- reclassify(uncert, rclmat)
#writeRaster(interp_chla, "Data/Productivity/Interp_chla.tif", 
#            datatype="INT2S",overwrite=TRUE)

interp_chla <- readOGR( dsn="Data/Productivity", layer="interpchla_100m" )
r <- raster(chla)
rp <- rasterize(interp_chla, r, 'ID')

# -------------------------------------------------#
# MapRaster function
MapRaster <- function( layer, filename, legend ){
  
  # Get the vertical and horizontal limits
  ext <- extent( layer )
  # Get x and y limits
  lims <- list( x=c(ext@xmin*1.1, ext@xmax*.98), y=c(ext@ymin*1.05, ext@ymax*.98) )
  
  #colour
  pal <- rev(brewer.pal( 8, "Spectral" ))
  
  # Map (up to 500,000 pixels)
  tiff( file=paste0("Output/Maps/Productivity/",filename,"_1km.tif"), 
        height=5.25, width=4.8,
        units = "in", res = 300,
        compression = "lzw")
  par(mar=c(.5,.5,.5,.5))
  plot( bcPoly, col = "grey60", border = NA, xlim = lims$x , ylim = lims$y)
  plot( layer, maxpixels=500000, add=TRUE, col=pal, legend=FALSE)
  plot( rp, add=TRUE, col="#00000075", legend=FALSE)
  plot( bcPoly, col = "grey60", add=TRUE,border = NA, xlim = lims$x , ylim = lims$y)
  plot( layer, legend.only=TRUE, col=pal, smallplot= c(.7,.73,0.65,.9),
        legend.args=list(text=legend, side=4, font=1, line=3.5, cex=1) )
  box( lty = 'solid', col = 'black')
  dev.off()
  
} # End MapRaster function
# -------------------------------------------------#


# Map
MapRaster( chla, filename="MeanChla", 
           legend = "Mean Chlorophyll a\n (mg m-3)" )
MapRaster( bloom, filename="BloomFreq", 
           legend = "Frequency of monthly\n plankton blooms" )


