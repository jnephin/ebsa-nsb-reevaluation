# Load packages
library(dplyr)
library(reshape2)
library(rgdal)
library(raster)
library(rgeos)



# Load boundary shapefile
nsb <- readOGR(dsn="Boundary", layer="NSB")

# load chla layer
chla <- readOGR(dsn="Data/Productivity.gdb", layer="Chla_mean_nsb")

# get centroid of chla polygons 1x1 km cells
chlaPts <- gCentroid(chla, byid=TRUE)
chlaPts <- SpatialPointsDataFrame(chlaPts, chla@data)



# -------------------------------------------------#
# Load EBSA in and outside polygons
gdb <- "EBSA_Polygons/EBSAs_Overlay.gdb"
ebsas <- ogrListLayers(gdb)

# within only
ebsas <- ebsas[-grep("^nsb_",ebsas)]

# loop through each ebsa polygon
# merge into one spatial polygon data frame
spdf <- readOGR(dsn=gdb, layer=ebsas[1])
spdf$EBSA <- rep(ebsas[1],length(spdf))
for(i in ebsas[2:length(ebsas)]){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i) 
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
  overlay <- over(chlaPts, poly)
  
  # bind ebsa ID to point data
  tmp <- cbind(chlaPts@data, EBSA=overlay$EBSA)
  
  # inside ebsa
  tmp$Area <- "in"
  
  # outside ebsa
  tmp$Area[is.na(tmp$EBSA)] <- "out"
  
  tmp <- tmp[c("Chla_mean","EBSA","Area")]
  
  # return
  dat[[i]] <- tmp
  
}

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


# Convert list to data.frame
df <- melt(dat)

# EBSA column
df$EBSA <- df$L1

# Remove L1 column
df <- df[!names(df) %in% c("L1","variable")]



#---------------------------------------------------------#
# aggregate

aggr <- df %>%
  group_by(EBSA,Area) %>%
  summarise(
    mean = MEAN(value),
    sd = STDEV(value)) %>%
  as.data.frame()


# save data
save(df, file="Aggregated/EBSA_Productivity.Rdata")



