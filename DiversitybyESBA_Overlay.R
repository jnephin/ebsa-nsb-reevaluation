#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview:
# 
#
###############################################################################


# Load packages
library(rgdal)
library(raster)
library(rgeos)

# Go to parent directory
setwd('..')


# -------------------------------------------------#
# Load gridded data
load(file="Aggregated/Survey_Mean.Rdata") #smean

# get richness and diversity variables
div <- c("Div_Fish","Div_Invert","nSp_Fish","nSp_Invert")
sdiv <- smean[names(smean) %in% div,]


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
# Overlay

# empty list for loop
divEBSA <- list()
# loop through each species in presence data
for(s in div){
    # get gridded data for s only
  spdiv <- smean[s]
    # empty list
    overlay <- list()
    # ebsas where species was listed as important
    for( e in ebsalist ){
      # get single ebsa 
      ebsa <- spdf[spdf$EBSA %in% e,]
      # overlay grid with relevant ebsa polygon(s)
      tmp <- over(spdiv, ebsa)
      tmp$ID <- 1
      tmp$ID[is.na(tmp$EBSA)] <- 0
      overlay[[e]] <- tmp[,2]
    }
    # convert list to dataframe
    df <- as.data.frame(overlay)
    # add overlay to species data
    spdiv@data <- cbind(spdiv@data, df)
    # return
    divEBSA[[s]] <- spdiv
}

# Save
save(divEBSA, file="Aggregated/EBSA_Diversity_Overlay.Rdata")


