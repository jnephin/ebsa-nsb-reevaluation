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
# Performs spatial overlay for each chlorophyll layer. Adds an inside or outside 
# attribute for each raster cell for each EBSAs.
#
###############################################################################

# Load packages
library(rgdal)
library(raster)
library(rgeos)

# Go to parent directory
setwd('..')


# Load boundary shapefile
nsb <- readOGR(dsn="Boundary", layer="NSB")

# load aggregated layer
load("Aggregated/Grid_ProductivityData.Rdata") #prod

# Convert to points for overlay
# that way the grid cell will be marked as inside only if the centroid is within the ebsa
proj <- proj4string(prod)
ptprod <- gCentroid(prod, byid=TRUE)
prod <- SpatialPointsDataFrame(coordinates(ptprod), prod@data, proj4string = CRS(proj))


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

# match ebsa CRS (both albers but slight difference in no defs notation)
proj4string(spdf) <- proj4string(prod)



# -------------------------------------------------#
# Overlay by ebsa

# empty list for loop
prodEBSA <- list()
# loop through each species in presence data
for(s in names(prod)){
  # get gridded data for s only
  sp <- prod[s]
  # empty list
  overlay <- list()
  # ebsas where species was listed as important
  for( e in ebsalist ){
    # get single ebsa 
    ebsa <- spdf[spdf$EBSA %in% e,]
    # overlay grid with relevant ebsa polygon(s)
    tmp <- over(sp, ebsa)
    tmp$ID <- 1
    tmp$ID[is.na(tmp$EBSA)] <- 0
    overlay[[e]] <- tmp[,2]
  }
  # convert list to dataframe
  df <- as.data.frame(overlay)
  # add overlay to species data
  sp@data <- cbind(sp@data, df)
  # return
  prodEBSA[[s]] <- sp
}

# Save
save(prodEBSA, file="Aggregated/EBSA_Productivity_Overlay.Rdata")


