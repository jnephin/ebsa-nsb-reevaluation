# Load packages
library(dplyr)
library(reshape2)
library(rgdal)
library(raster)
library(rgeos)

# Go to parent directory
setwd('..')


# Speed up raster processes
rasterOptions(chunksize = 1e+08, maxmemory = 1e+09)



# Load boundary shapefile
nsb <- readOGR(dsn="Boundary", layer="NSB")

# load chla layer
chla <- raster("Data/Productivity/Chla_mean_nsb.tif")
bloom <- raster("Data/Productivity/Bloom_freq_nsb.tif")



# -------------------------------------------------#
# Load EBSA in and outside polygons
gdb <- "EBSA_Polygons/EBSAs_Overlay.gdb"
ebsas <- ogrListLayers(gdb)

# within only
#ebsas <- ebsas[-grep("^nsb_",ebsas)]

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


# -------------------------------------------------#
# Overlay survey grid data with EBSAs


# empty dataframe for loop
dat <- NULL

# loop through each ebsa polygon for survey data
for(i in ebsas){

  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly$EBSA <- rep(i,length(poly))

  # overlay points on ebsa
  chla.over <- extract(chla, poly)
  bloom.over <- extract(bloom, poly)

  # remove NA
  chla.over <- chla.over[[1]]
  chla.over <- chla.over[!is.na(chla.over)]
  bloom.over <- bloom.over[[1]]
  bloom.over <- bloom.over[!is.na(bloom.over)]

  # bind ebsa ID to point data
  tmp <- data.frame( chla = chla.over, bloom = bloom.over,
                     EBSA = rep(i, length(overlay)) )

  # return
  dat <- rbind(dat, tmp)

}


# Inside / Outside
dat$Area <- "in"
dat$Area[grep("^nsb_",dat$EBSA)] <- "out"

# EBSA
dat$EBSA <- sub("nsb_","",dat$EBSA)


# Save
save(dat, file="Aggregated/EBSA_Productivity_Overlay.Rdata")




# aggregating functions
MEAN <- function(x){
  mean(x, na.rm=TRUE)
}
STDEV <- function(x){
  sd(x, na.rm=TRUE)
}



#---------------------------------------------------------#
# aggregate

aggr <- dat %>%
  group_by(EBSA,Area) %>%
  summarise(
    chla.mean = MEAN(chla),
    chla.sd = STDEV(chla),
    bloom.mean = MEAN(bloom),
    bloom.sd = STDEV(bloom)) %>%
  as.data.frame()


# save data
save(aggr, file="Aggregated/EBSA_Productivity.Rdata")
