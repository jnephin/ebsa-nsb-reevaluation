# Load packages
library(rgdal)
library(raster)
library(rgeos)

# Go to parent directory
setwd('..')

# Load grid
grid <- readOGR(dsn="Grid", layer="NSB_5km")


## Survey Data ##

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
  aggr <- aggregate(dat, grid, FUN="mean")

  # add to data.frame
  df <- cbind(df, aggr@data)

}

# Remove SourceKey, x and y columns
df <- df[,!names(df) %in% c("df", "SourceKey","x","y")]

# add dataframe back onto grid
survey_grid <- SpatialPolygonsDataFrame(grid, df)

# Save
save(survey_grid, file="Aggregated/Mean.Rdata")

# Export
writeOGR(survey_grid, dsn="Aggregated/Mean", layer="Mean",
         driver="ESRI Shapefile", overwrite=TRUE)





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



## Fishery Data ##

# load gdb
gdb="Data/FisheryData.gdb"
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
fishery_df <- df




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
df <- cbind(presence_df,fishery_df,polygon_df)
df <- df[,!names(df) %in% "df"]
presence_grid <- SpatialPolygonsDataFrame(grid, df)

# Save
save(presence_grid, file="Aggregated/Presence.Rdata")

# Export
writeOGR(presence_grid, dsn="Aggregated/Presence", layer="Presence",
         driver="ESRI Shapefile", overwrite=TRUE)
