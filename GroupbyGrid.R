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
# Aggregates by grid cell
#  * Calculates mean and sd per grid cell for survey data
#  * Calculates presence of points within grid cell for presence and polygon data
#
###############################################################################

# Load packages
library(rgdal)
library(raster)
library(rgeos)

# Go to parent directory
setwd('..')

# Load grid
grid <- readOGR(dsn="Grid", layer="NSB_5km_buffered")


## Survey Data - Mean ##

# load gdb
gdb="Data/SurveyData.gdb"
fc_list = ogrListLayers(gdb)

# Empty dataframe for loop
df <- 1:nrow(grid)

for(i in fc_list){
  # load feature class
  dat <- readOGR(dsn=gdb, layer=i)
  # assign matching proj4 string
  if( !grepl("proj=aea", proj4string(dat)) ) stop( "Data not projected in BC Albers")
  proj4string(grid) <-   proj4string(dat)
  # aggregate points using grid
  aggr <- aggregate(dat, grid, FUN="mean", na.rm=T)
  # add to data.frame
  df <- cbind(df, aggr@data)
}

# Remove SourceKey, x and y columns
df <- df[,!names(df) %in% c("df", "SourceKey","x","y")]
# add dataframe back onto grid
smean <- SpatialPolygonsDataFrame(grid, df)
# Save
save(smean, file="Aggregated/Grid_Survey_Mean.Rdata")


## Survey Data - Standard Deviation ##

# Empty dataframe for loop
df <- 1:nrow(grid)

for(i in fc_list){
  # load feature class
  dat <- readOGR(dsn=gdb, layer=i)
  # assign matching proj4 string
  if( !grepl("proj=aea", proj4string(dat)) ) stop( "Data not projected in BC Albers")
  proj4string(grid) <-   proj4string(dat)
  # aggregate points using grid
  aggr <- aggregate(dat, grid, FUN="sd", na.rm=T)
  # add to data.frame
  df <- cbind(df, aggr@data)
}

# Remove SourceKey, x and y columns
df <- df[,!names(df) %in% c("df", "SourceKey","x","y")]
# add dataframe back onto grid
ssd <- SpatialPolygonsDataFrame(grid, df)
# Save
save(ssd, file="Aggregated/Grid_Survey_SD.Rdata")




## Presence Data ##

# load gdb
gdb="Data/PresenceData.gdb"
fc_list = ogrListLayers(gdb)

# Empty dataframe for loop
df <- 1:nrow(grid)

for(i in fc_list){

  # load feature class
  dat <- readOGR(dsn=gdb, layer=i)
  # assign matching proj4 string
  if( !grepl("proj=aea", proj4string(dat)) ) stop( "Data not projected in BC Albers")
  proj4string(grid) <-   proj4string(dat)
  # overlay points on grid
  overlay <- over(grid, dat)
  # present = 1
  overlay$presence <- 1
  overlay$presence[is.na(overlay[,1])] <- 0
  # add to data.frame
  df <- cbind(df, overlay["presence"] )
  # rename column
  ind <- which(names(df) %in%"presence")
  colnames(df)[ind] <- i
}

# rename dataframe
presence_df <- df



## Polygon Data ##

# load gdb
gdb="Data/PolygonData.gdb"
fc_list = ogrListLayers(gdb)

# Empty dataframe for loop
df <- 1:nrow(grid)

for(i in fc_list){
  # load feature class
  dat <- readOGR(dsn=gdb, layer=i)
  # assign matching proj4 string
  if( !grepl("proj=aea", proj4string(dat)) ) stop( "Data not projected in BC Albers")
  proj4string(grid) <-   proj4string(dat)
  # overlay points on grid
  overlay <- over(grid, dat)
  # present = 1
  overlay$presence <- 1
  overlay$presence[is.na(overlay[,1])] <- 0
  # add to data.frame
  df <- cbind(df, overlay["presence"] )
  # rename column
  ind <- which(names(df) %in%"presence")
  colnames(df)[ind] <- i
}

# rename dataframe
polygon_df <- df



## combine all binary grid data ##

# add all presence gridded data into one spdf
df <- cbind(presence_df, polygon_df)
df <- df[,!names(df) %in% "df"]
pres <- SpatialPolygonsDataFrame(grid, df)

# Save
save(pres, file="Aggregated/Grid_Presence.Rdata")
