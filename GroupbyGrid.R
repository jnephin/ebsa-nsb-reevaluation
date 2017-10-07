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
grid <- readOGR(dsn="Grid", layer="NSB_5km")


## Fish, Marine Mammal and Marine Birds ##
# load gdb
gdbs=c("Fish","MarineMammals","MarineBirds")
for(gdb in gdbs){
  fc_list = ogrListLayers(paste0("Data/",gdb,".gdb"))
  # Empty dataframe for loop
  df <- 1:nrow(grid)
  for(i in fc_list){
    # load feature class
    dat <- readOGR(dsn=paste0("Data/",gdb,".gdb"), layer=i)
    # Only retain column of interest
    dat <- dat[i]
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
  assign(gdb, SpatialPolygonsDataFrame(grid, df))
}
# Combine
dens <- SpatialPolygonsDataFrame(grid, cbind(Fish@data,MarineMammals@data, MarineBirds@data))
proj4string(dens) <- proj4string(Fish)
# Save
save(dens, file="Aggregated/Grid_DensityData.Rdata")



## Diversity Data ##
# load gdb
fc_list = ogrListLayers("Data/Diversity.gdb")
# Empty dataframe for loop
df <- 1:nrow(grid)
for(i in fc_list){
  # load feature class
  dat <- readOGR(dsn="Data/Diversity.gdb", layer=i)
  # Only retain column of interest
  dat <- dat[i]
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
div <- SpatialPolygonsDataFrame(grid, df)


# Save
save(div, file="Aggregated/Grid_DiversityData.Rdata")



## Invert Data ##
# load gdb
gdb="Data/Invert.gdb"
fc_list = ogrListLayers(gdb)
# Empty dataframe for loop
presence_df <- 1:nrow(grid)
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
  presence_df <- cbind(presence_df, overlay["presence"] )
  # rename column
  ind <- which(names(presence_df) %in%"presence")
  colnames(presence_df)[ind] <- i
}

## Polygon Data ##
# load gdb
gdb="Data/PolygonRanges.gdb"
fc_list = ogrListLayers(gdb)
# Empty dataframe for loop
polygon_df <- 1:nrow(grid)
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
  polygon_df <- cbind(polygon_df, overlay["presence"] )
  # rename column
  ind <- which(names(polygon_df) %in%"presence")
  colnames(polygon_df)[ind] <- i
}


## combine all binary grid data ##
# add all presence gridded data into one spdf
df <- cbind(presence_df[-1], polygon_df[-1])
pres <- SpatialPolygonsDataFrame(grid, df)

# Save
save(pres, file="Aggregated/Grid_PresenceData.Rdata")




## Productivity Data ##
# load chla layer (includes straylight)
chla <- raster("Data/Productivity/Chla_mean_straylight.tif")
bloom <- raster("Data/Productivity/Bloom_freq_straylight.tif")

# Conver to spatial points
spchla <- rasterToPoints(chla, spatial=TRUE)
spbloom <- rasterToPoints(bloom, spatial=TRUE)

# load gdb
layers= c("spchla","spbloom")
# Empty dataframe for loop
prod_df <- 1:nrow(grid)
for(i in layers){
  # load feature class
  dat <- get(i)
  # assign matching proj4 string
  if( !grepl("proj=aea", proj4string(dat)) ) stop( "Data not projected in BC Albers")
  proj4string(grid) <-   proj4string(dat)
  # aggregate points using grid
  aggr <- aggregate(dat, grid, FUN="mean", na.rm=T)
  # add to data.frame
  prod_df <- cbind(prod_df, aggr@data)
}
# add back to grid
prod <- SpatialPolygonsDataFrame(grid, prod_df[-1])

# Save
names(prod) <- c("Chla_mean_nsb","Bloom_freq_nsb")
save(prod, file="Aggregated/Grid_ProductivityData.Rdata")




#-----------------------------------------------------------------------------------------#
# Variance


## Fish, Marine Mammal and Marine Birds ##
# load gdb
gdbs=c("Fish","MarineMammals","MarineBirds")
for(gdb in gdbs){
  fc_list = ogrListLayers(paste0("Data/",gdb,".gdb"))
  # Empty dataframe for loop
  df <- 1:nrow(grid)
  for(i in fc_list){
    # load feature class
    dat <- readOGR(dsn=paste0("Data/",gdb,".gdb"), layer=i)
    # Only retain column of interest
    dat <- dat[i]
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
  assign(gdb, SpatialPolygonsDataFrame(grid, df))
}
# Combine
dens <- SpatialPolygonsDataFrame(grid, cbind(Fish@data,MarineMammals@data, MarineBirds@data))
proj4string(dens) <- proj4string(Fish)
# Save
save(dens, file="Aggregated/Grid_DensityData_var.Rdata")



## Diversity Data ##
# load gdb
fc_list = ogrListLayers("Data/Diversity.gdb")
# Empty dataframe for loop
df <- 1:nrow(grid)
for(i in fc_list){
  # load feature class
  dat <- readOGR(dsn="Data/Diversity.gdb", layer=i)
  # Only retain column of interest
  dat <- dat[i]
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
div <- SpatialPolygonsDataFrame(grid, df)


# Save
save(div, file="Aggregated/Grid_DiversityData_var.Rdata")



## Productivity Data ##
# load chla layer (includes straylight)
chla <- raster("Data/Productivity/Chla_mean_straylight.tif")
bloom <- raster("Data/Productivity/Bloom_freq_straylight.tif")

# Conver to spatial points
spchla <- rasterToPoints(chla, spatial=TRUE)
spbloom <- rasterToPoints(bloom, spatial=TRUE)

# load gdb
layers= c("spchla","spbloom")
# Empty dataframe for loop
prod_df <- 1:nrow(grid)
for(i in layers){
  # load feature class
  dat <- get(i)
  # assign matching proj4 string
  if( !grepl("proj=aea", proj4string(dat)) ) stop( "Data not projected in BC Albers")
  proj4string(grid) <-   proj4string(dat)
  # aggregate points using grid
  aggr <- aggregate(dat, grid, FUN="sd", na.rm=T)
  # add to data.frame
  prod_df <- cbind(prod_df, aggr@data)
}
# add back to grid
prod <- SpatialPolygonsDataFrame(grid, prod_df[-1])
# Save
names(prod) <- c("Chla_mean_nsb","Bloom_freq_nsb")
save(prod, file="Aggregated/Grid_ProductivityData_var.Rdata")


