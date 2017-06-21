# Load packages
library(rgdal)
library(raster)
library(rgeos)

# Go to parent directory
setwd('..')


# -------------------------------------------------#
# Load boundary shapefile
nsb <- readOGR(dsn="Boundary", layer="NSB")

# Convert to spatial polygons (i.e., drop the data)
nsb <- as( nsb, "SpatialPolygons" )


# -------------------------------------------------#
# Load IPHC shapefile
iphc <- readOGR(dsn="FishSurveys", layer="IPHC_albers")

# Convert to spatial polygons (i.e., drop the data)
iphc <- as( iphc, "SpatialPoints" )


# -------------------------------------------------#
# Load GF Synoptic
gf <- readOGR(dsn="FishSurveys/GFSynoptic_Weights.gdb", layer="All_proj")

# Convert to spatial polygons (i.e., drop the data)
gf <- as( gf, "SpatialPoints" )


# -------------------------------------------------#
# Load PHMA
phma <- readOGR(dsn="FishSurveys/PHMA.gdb", layer="All_proj")

# Convert to spatial polygons (i.e., drop the data)
phma <- as( phma, "SpatialPoints" )



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
# Load EBSA in and outside polygons
gdb <- "EBSA_Polygons/EBSAs.gdb"
ebsas <- ogrListLayers(gdb)

# within only ebsas important for halibut
#ebsas <- c("CapeStJames","McIntyreBay")
# ebsas
ebsas <- c("HecateStraitFront","BellaBellaNearshore","BrooksPeninsula","CapeStJames",
           "CentralMainland","ChathamSound","DogfishBank","HaidaGwaiiNearshore",
           "LearmonthBank","McIntyreBay","NorthIslandsStraits","ScottIslands",
           "ShelfBreak","SpongeReefs")

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
# map IPHC

# Get the vertical and horizontal limits
ext <- extent( iphc )
# Get x and y limits
lims <- list( x=c(ext@xmin, ext@xmax), y=c(ext@ymin, ext@ymax) )

# Map
pdf( file=file.path("Output/Maps/Surveys","Map_GroundfishSurveys.pdf"),
     height=6, width=5.25 )
par(mar=c(1,1,1,1))
plot( bcPoly, col = "grey40", border = NA, xlim = lims$x , ylim = lims$y )
box( lty = 'solid', col = 'black')
plot( gf, border=NA, add = T , pch = 1, cex = .5, col="#7fc97f")
plot( phma, border=NA, add = T, pch = 2, cex = .5, col="#386cb0" )
plot( iphc, border=NA, add = T, pch = 3, cex = .5, col="#f0027f" )
plot( spdf, add=T )
legend( "bottomleft", legend = c("GF Synoptic", "PHMA", "IPHC"), pch = c(1,2,3),
        col= c("#7fc97f","#386cb0","#f0027f"), bg = NA, box.col = NA)
dev.off()
