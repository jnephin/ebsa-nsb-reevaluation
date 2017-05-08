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
# 
#
#
###############################################################################


# Load packages
library(dplyr)
library(reshape2)
library(sp)
library(lazyeval)

# Go to parent directory
setwd('..')

# global options
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
count <- function(x){
  length(x[x>0])
}

# load data
load(file="Aggregated/EBSA_Mean_Overlay.Rdata") #meanEBSA
load(file="Aggregated/EBSA_Presence_Overlay.Rdata") #presEBSA


#---------------------------------------------------------#

# all important species
species <- c("Abalone","Alcids","Ancient_Murrelet","Black_legged_Kittiwake","BlueWhale",
             "Brandts_Cormorant","ButterSole","Cassins_Auklet","Common_Murre","DoverSole",
             "DungenessCrab","EnglishSole","Eulachon","FinWhale","Fork_tailed_Storm_petrels",
             "Geoduck","Glaucous_winged_Gull","GreenSturgeon","GreenUrchin","GreyWhale","Hake", 
             "Halibut","Halibut_phma","Herring","Herring_Gulls","HerringSpawn","Humpback",
             "KillerWhale","Leachs_Storm_petrels","Lingcod","Lingcod_phma","PacificCod",
             "PacificOceanPerch","Pelagic_Cormorant","Phalaropes","Pigeon_Guillemot",
             "Prawn","RedSeaCucumber","RedUrchin","Rhinoceros_Auklet","RockSole","Sablefish",
             "Sablefish_phma","SandSole","Scoters","SeaOtterRange","SeiWhale","Shearwaters",
             "Shrimp","Sooty_Shearwaters","SpermWhale","SpongeReef","StellarSeaLionRookeries",
             "TannerCrab","Tufted_Puffin","WidowRockfish","YellowmouthRockfish","YellowtailRockfish",
             "BlackRockfish","ChinaRockfish","CopperRockfish","QuillbackRockfish","TigerRockfish",
             "YelloweyeRockfish")



#---------------------------------------------------------#
# loop through species
# calculate metrics for inside each ebsa
# calculate metrics for cells outside of all inside ebsas using bootstrapping method

# empty list
surveydat <- list()

# loop through each species in survey data
for(s in species){
  # is the species present in the dataset
  if( s %in% names(meanEBSA) ){
    # gridded data for species s
    spdf <- meanEBSA[[s]]
    # esba(s)
    ebsa <- names(spdf)[-1]
    
    # empty
    inebsa <- NULL
    # loop through ebsas
    for(e in ebsa){
      # summarise inside each ebsa
      sum_inside <- spdf@data %>%
        group_by_(e) %>% 
        summarise_(mean = interp(~MEAN(var), var = as.name(s)),
                   sd = interp(~STDEV(var), var = as.name(s)),
                   pPres = interp(~pPres(var)*100, var = as.name(s)),
                   pNA = interp(~pNA(var)*100, var = as.name(s)),
                   count = interp(~count(var), var = as.name(e)))  %>%
        as.data.frame()
      # just inside values
      inside <- sum_inside[sum_inside[,1] == 1,]
      # error for inside ebsa measured by sd
      inside$mean_error <- inside$sd
      inside$pPres_error <- NA
      inside$pNA_error <- NA
      # bind together
      inside[1,1] <- e
      colnames(inside)[1] <- "EBSA"
      inebsa <- rbind(inebsa, inside)
    }
    
    # get outside grid cells
    out <- spdf@data[-1]
    outindex <- which(apply(out, 1, sum) == 0)
    out <- spdf@data[outindex,1]
    # number of cells for bootstrapping
    cellnum <- sum(inebsa$count)
    # empty 
    boot <- NULL
    # loop through 100 times for bootstrapping
    resample <- 100
    for(i in 1:resample){
      # generate random numbers
      rn <- sample(1:length(out), cellnum)
      # get sample data
      sampledata <- out[rn]
      # summarise outside grid cells
      sum_out <- data.frame(mean = MEAN(sampledata),
                            pPres = pPres(sampledata)*100,
                            pNA = pNA(sampledata)*100)
      # return
      boot <- rbind(boot, sum_out)
    }
    # Calculate mean and sd
    mean_boot <- apply(boot, 2, mean)
    sd_boot <- apply(boot, 2, sd)
    # 95% confidence interval estimated with t-distribution
    conferror <- function(x){ qt(0.975,df=resample-1) * x / sqrt(resample) }
    error_boot <- conferror(sd_boot)
    # create table to match inside output  
    outebsa <- data.frame(EBSA= "Outside", t(mean_boot), count = cellnum)
    outebsa$mean_error <- error_boot["mean"]
    outebsa$pPres_error <- error_boot["pPres"]
    outebsa$pNA_error <- error_boot["pNA"]
      
    # create to summary table
    sumtable <- rbind(inebsa[names(outebsa)],outebsa)
    # add to list
    surveydat[[s]] <- sumtable
  }
}
# save survey data
save(surveydat, file="Aggregated/EBSA_Mean_InOut.Rdata")




# empty list
presdat <- list()

# loop through each species in presence data
for(s in species){
  # is the species present in the dataset
  if( s %in% names(presEBSA) ){
    # gridded data for species s
    spdf <- presEBSA[[s]]
    # esba(s)
    ebsa <- names(spdf)[-1]
    
    # empty
    inebsa <- NULL
    # loop through ebsas
    for(e in ebsa){
      # summarise inside each ebsa
      sum_inside <- spdf@data %>%
        group_by_(e) %>% 
        summarise_(sum = interp(~SUM(var), var = as.name(s)),
                   pPres = interp(~pPres(var)*100, var = as.name(s)),
                   count = interp(~count(var), var = as.name(e)))  %>%
        as.data.frame()
      # just inside values
      inside <- sum_inside[sum_inside[,1] == 1,]
      # error for inside ebsa cannot be measured
      inside$sum_error <- NA
      inside$pPres_error <- NA
      # bind together
      inside[1,1] <- e
      colnames(inside)[1] <- "EBSA"
      inebsa <- rbind(inebsa, inside)
    }
    
    # get outside grid cells
    out <- spdf@data[-1]
    outindex <- which(apply(out, 1, sum) == 0)
    out <- spdf@data[outindex,1]
    # number of cells for bootstrapping
    cellnum <- sum(inebsa$count)
    # empty 
    boot <- NULL
    # loop through 100 times for bootstrapping
    resample <- 100
    for(i in 1:resample){
      # generate random numbers
      rn <- sample(1:length(out), cellnum)
      # get sample data
      sampledata <- out[rn]
      # summarise outside grid cells
      sum_out <- data.frame(sum = SUM(sampledata),
                            pPres = pPres(sampledata)*100)
      # return
      boot <- rbind(boot, sum_out)
    }
    # Calculate mean and sd
    sum_boot <- apply(boot, 2, mean)
    sd_boot <- apply(boot, 2, sd)
    # 95% confidence interval estimated with t-distribution
    conferror <- function(x){ qt(0.975,df=resample-1) * x / sqrt(resample) }
    error_boot <- conferror(sd_boot)
    # create table to match inside output  
    outebsa <- data.frame(EBSA= "Outside", t(sum_boot), count = cellnum)
    outebsa$sum_error <- error_boot["sum"]
    outebsa$pPres_error <- error_boot["pPres"]

    # create to summary table
    sumtable <- rbind(inebsa[names(outebsa)],outebsa)
    # add to list
    presdat[[s]] <- sumtable
  }
}

# save presence data
save(presdat, file="Aggregated/EBSA_Presence_InOut.Rdata")



