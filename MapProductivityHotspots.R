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
# List EBSA polygons
gdb <- "EBSA_Polygons/EBSAs.gdb"
ebsalist <- ogrListLayers(gdb)

# loop through each ebsa polygon
# merge into one spatial polygon data frame
spdf <- readOGR(dsn=gdb, layer=ebsalist[1])
spdf <- as( spdf, "SpatialPolygons" )
spdf$EBSA <- rep(ebsalist[1],length(spdf))
for(i in ebsalist[2:length(ebsalist)]){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly <- as( poly, "SpatialPolygons" )
  poly$EBSA <- rep(i,length(poly))
  spdf <- rbind(spdf,poly)
}

# Remove extra attributes
spdf <- spdf[,names(spdf) %in% "EBSA"]


# -------------------------------------------------#
# load rasters
chla <- raster( "Data/Productivity/Chla_mean_nsb_hotspots.tif")
bloom <- raster("Data/Productivity/Bloom_freq_nsb_hotspot.tif")


# -------------------------------------------------#
# MapRaster function
MapRaster <- function( layer, filename, legend ){
  
  # Get the vertical and horizontal limits
  ext <- extent( layer )
  # Get x and y limits
  lims <- list( x=c(ext@xmin*1.1, ext@xmax*.98), y=c(ext@ymin*1.05, ext@ymax*.98) )
  
  #colour
  pal <- c("lightblue","goldenrod3")
  
  # Map (up to 500,000 pixels)
  tiff( file=paste0("Output/Maps/Productivity/",filename,"_1km.tif"), 
        height=5.25, width=4.8,
        units = "in", res = 300,
        compression = "lzw")
  par(mar=c(.5,.5,.5,.5))
  plot( bcPoly, col = "grey60", border = NA, xlim = lims$x , ylim = lims$y)
  plot( layer, maxpixels=500000, add=TRUE, col=pal, legend=FALSE, useRaster=FALSE)
  plot( bcPoly, col = "grey60", add=TRUE, border = NA)
  plot( nsb, border = "black", add=TRUE, col = NA, lwd=.5 )
  plot( spdf, add=T, lwd=.8, lty=3 )
  legend("topright",  col=pal, pch=15, bty="n", bg=NA, 
         legend = c("Data extent", "Hotspot"),
         title=legend, cex=.8)
  box( lty = 'solid', col = 'black')
  dev.off()
  
} # End MapRaster function
# -------------------------------------------------#


# Map
MapRaster( chla, filename="MeanChla", 
           legend = "Mean Chlorophyll Hotspots" )
MapRaster( bloom, filename="BloomFreq", 
           legend = "Bloom Frequency Hotspots" )




