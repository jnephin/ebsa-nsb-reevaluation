# Load packages
library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(RColorBrewer)


# -------------------------------------------------#
# Load EBSA in and outside polygons
gdb <- "EBSA_Polygons/EBSAs_Overlay.gdb"
ebsas <- ogrListLayers(gdb)

# within only
ebsas <- ebsas[-grep("^nsb_",ebsas)]

# loop through each ebsa polygon
# merge into one spatial polygon data frame
spdf <- readOGR(dsn=gdb, layer=ebsas[1]) 
for(i in ebsas[2:length(ebsas)]){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i) 
  spdf <- rbind(spdf,poly)
}

# Convert to spatial polygons (i.e., drop the data)
spdf <- as( spdf, "SpatialPolygons" )


# -------------------------------------------------#



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



# -------------------------------------------------#
# MapRaster function
MapRaster <- function( layer ){
  
  # name
  name <- names(layer)
  name <- sub("_nsb","", name)
  
  # Get the vertical and horizontal limits
  ext <- extent( layer )
  # Get x and y limits
  lims <- list( x=c(ext@xmin, ext@xmax), y=c(ext@ymin, ext@ymax) )
  
  #colour
  pal <- rev(brewer.pal( 8, "Spectral" ))
  
  # Map (up to 500,000 pixels)
  pdf( file=file.path("Output/Maps",  paste0(name,".pdf")),
       height=6, width=6*diff(lims$x)/diff(lims$y)+1 )
  par( mar=c(1,1,1,5) )
  plot( bcPoly, col = "grey40", border = NA, xlim = lims$x*.98 , ylim = lims$y*.98 )
  box( lty = 'solid', col = 'black')
  plot( layer, maxpixels=500000, add=TRUE, col=pal,
        legend.args=list(text=name, side=4, font=2, line=2.5, cex=0.8) )
  plot( spdf, add=T )
  dev.off()
  
} # End MapRaster function  
# -------------------------------------------------#


# Map
MapRaster( chla )
MapRaster( bloom )



