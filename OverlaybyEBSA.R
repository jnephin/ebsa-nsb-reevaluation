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
# Adds an attribute to the gridded data that describes its position
# inside or outside each EBSA
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
load(file="Aggregated/Survey_Mean.Rdata") #survey_grid
smean <- survey_grid
load(file="Aggregated/Survey_SD.Rdata") #survey_grid
ssd <- survey_grid
load(file="Aggregated/Presence.Rdata") #presence_grid

# Load boundary shapefile
nsb <- readOGR(dsn="Boundary", layer="NSB")



# -------------------------------------------------#
# Load EBSA in and outside polygons
gdb <- "EBSA_Polygons/EBSAs_Overlay.gdb"
ebsas <- ogrListLayers(gdb)

# within only
ebsas <- ebsas[-grep("^nsb_",ebsas)]

# loop through each ebsa polygon
# merge into one spatial polygon data frame
spdf <- readOGR(dsn=gdb, layer=ebsas[1])
spdf <- as( spdf, "SpatialPolygons" )
spdf$EBSA <- rep(ebsas[1],length(spdf))
for(i in ebsas[2:length(ebsas)]){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly <- as( poly, "SpatialPolygons" )
  poly$EBSA <- rep(i,length(poly))
  spdf <- rbind(spdf,poly)
}

# Remove extra attributes
spdf <- spdf[,names(spdf) %in% "EBSA"]




# -------------------------------------------------#
# Overlay survey grid data with EBSAs


# empty list for loop
dat <- list()

# loop through each ebsa polygon for survey data
for(i in ebsas){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly$EBSA <- rep(i,length(poly))
  # overlay grid on ebsa
  overlay <- over(smean, poly)
  # bind ebsa ID to grid data
  tmp <- cbind(smean@data, EBSA=overlay$EBSA)
  # inside ebsa
  tmp$Area <- "in"
  # outside ebsa
  tmp$Area[is.na(tmp$EBSA)] <- "out"
  # return
  dat[[i]] <- tmp
}

# Save
save(dat, file="Aggregated/EBSA_Mean_Overlay.Rdata")


# empty list for loop
dat <- list()

# loop through each ebsa polygon for survey data
for(i in ebsas){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly$EBSA <- rep(i,length(poly))
  # overlay grid on ebsa
  overlay <- over(ssd, poly)
  # bind ebsa ID to grid data
  tmp <- cbind(ssd@data, EBSA=overlay$EBSA)
  # inside ebsa
  tmp$Area <- "in"
  # outside ebsa
  tmp$Area[is.na(tmp$EBSA)] <- "out"
  # return
  dat[[i]] <- tmp
}

# Save
save(dat, file="Aggregated/EBSA_SD_Overlay.Rdata")





# -------------------------------------------------#
# Overlay presence grid data with EBSAs


# empty list for loop
dat <- list()

# loop through each ebsa polygon for survey data
for(i in ebsas){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly$EBSA <- rep(i,length(poly))
  # overlay grid on ebsa
  overlay <- over(survey_grid, poly)
  # bind ebsa ID to grid data
  tmp <- cbind(presence_grid@data, EBSA=overlay$EBSA)
  # inside ebsa
  tmp$Area <- "in"
  # outside ebsa
  tmp$Area[is.na(tmp$EBSA)] <- "out"
  # return
  dat[[i]] <- tmp
}

# Save
save(dat, file="Aggregated/EBSA_Presence_Overlay.Rdata")
