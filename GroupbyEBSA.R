# Load packages
library(rgdal)
library(raster)
library(rgeos)
library(reshape2)

options(scipen = 999)

# aggregating functions
pPres <- function(x){
  length(x[x>0])/length(x[x>=0])
}
pNA <- function(x){
  length(x[is.na(x)])/length(x)
}
MEAN <- function(x){
  mean(x, na.rm=TRUE)
}
STDEV <- function(x){
  sd(x, na.rm=TRUE)
}
SUM <- function(x){
  sum(x, na.rm=TRUE)
}


#---------------------------------------------------------#

# Load gridded data
load(file="Aggregated/Mean.Rdata") #survey_grid
load(file="Aggregated/Presence.Rdata") #presence_grid


# Load EBSA in and outside polygons
gdb="EBSA_Polygons/EBSAs_Overlay.gdb"
ebsas = ogrListLayers(gdb)


#---------------------------------------------------------#

# empty list for loop
dat <- list()


# loop through each ebsa polygon
for(i in ebsas){
  
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i) 
  
  #aggregate survey data on ebsa polygons
  survey_mean <- aggregate(survey_grid, poly, FUN=MEAN)
  survey_sd <- aggregate(survey_grid, poly, FUN=STDEV)
  survey_pPres <- aggregate(survey_grid, poly, FUN=pPres)
  survey_pNA <- aggregate(survey_grid, poly, FUN=pNA)
  
  #aggregate presence data on ebsa polygons
  presence_sum <- aggregate(presence_grid, poly, FUN=SUM)
  presence_pPres <- aggregate(presence_grid, poly, FUN=pPres)
  
  # convert to dataframes
  survey <- data.frame(survey_mean=t(survey_mean@data),
                       survey_sd=t(survey_sd@data),
                       survey_pPres=t(survey_pPres@data),
                       survey_pNA=t(survey_pNA@data))
  survey$species <- row.names(survey)
  survey <- melt(survey, variable.name = "metric")
  
  presence <- data.frame(presence_sum=t(presence_sum@data),
                         presence_pPres=t(presence_pPres@data))
  presence$species <- row.names(presence)
  presence <- melt(presence, variable.name = "metric")
  
  # combine presence and survey data frame
  tmp <- rbind(survey, presence)
  
  # separate metric column
  tmp$type <- sub("_.*","",tmp$metric)
  tmp$metric <- sub(".*_","",tmp$metric)
  
  # order columns
  tmp <- tmp[,c("species","type","metric","value")]
  
  # return
  dat[[i]] <- tmp
    
}

## overwrite SpongeReefs (to fix multi polygon issue)
tmp <- dat[["SpongeReefs"]]
tmp <- tmp[tmp$species == "SpongeReefs",]
tmp$metric <- sub("[.].*","",tmp$metric)
tmpsum <- aggregate(value ~ species+type+metric, sum, data = tmp[tmp$metric =="sum",])
tmppPres <- aggregate(value ~ species+type+metric, mean, data = tmp[tmp$metric =="pPres",])
tmp <- rbind(tmpsum,tmppPres)
dat[["SpongeReefs"]] <- tmp

# Save
save(dat, file="Aggregated/EBSA_Aggregation.Rdata")



