# Load packages
library(rgdal)
library(raster)
library(rgeos)


# -------------------------------------------------#
# Load gridded data
load(file="Aggregated/Mean.Rdata") #survey_grid
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

  # overlay points on ebsa
  overlay <- over(survey_grid, poly)

  # bind ebsa ID to point data
  tmp <- cbind(survey_grid@data, EBSA=overlay$EBSA)

  # inside ebsa
  tmp$Area <- "in"
  
  # outside ebsa
  tmp$Area[is.na(tmp$EBSA)] <- "out"

  # return
  dat[[i]] <- tmp

}

# Save
save(dat, file="Aggregated/EBSA_Survey_Overlay.Rdata")



# -------------------------------------------------#
# Overlay presence grid data with EBSAs


# empty list for loop
dat <- list()

# loop through each ebsa polygon for survey data
for(i in ebsas){
  
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly$EBSA <- rep(i,length(poly))
  
  # overlay points on ebsa
  overlay <- over(survey_grid, poly)
  
  # bind ebsa ID to point data
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
