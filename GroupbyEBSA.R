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
# Aggregates by in and outside each EBSA polgyon
#  * Calculates the mean, sd, percent of occurence and percent of missing
#    data for survey data
#  * Caluclates the sum and percent of occurence for presence data
#
###############################################################################

# Load packages
library(dplyr)
library(reshape2)

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


#---------------------------------------------------------#


# load data
load(file="Aggregated/EBSA_Mean_Overlay.Rdata") #dat
mean_dat <- dat
load(file="Aggregated/EBSA_SD_Overlay.Rdata") #dat
sd_dat <- dat
load(file="Aggregated/EBSA_Presence_Overlay.Rdata") #dat
pres_dat <- dat

# Convert list to data.frame
mean_df <- melt(mean_dat)
sd_df <- melt(sd_dat)
pres_df <- melt(pres_dat)

# EBSA column
mean_df$EBSA <- mean_df$L1
sd_df$EBSA <- sd_df$L1
pres_df$EBSA <- pres_df$L1

# Remove L1 column
mean_df <- mean_df[!names(mean_df) == "L1"]
sd_df <- sd_df[!names(sd_df) == "L1"]
pres_df <- pres_df[!names(pres_df) == "L1"]

# convert variable to character
mean_df$variable <- as.character(mean_df$variable)
sd_df$variable <- as.character(sd_df$variable)
pres_df$variable <- as.character(pres_df$variable)


#---------------------------------------------------------#
# aggregate

mean_aggr <- mean_df %>%
  group_by(EBSA,Area,variable) %>%
  summarise(
    mean = MEAN(value),
    sd = STDEV(value),
    pPres = pPres(value),
    pNA = pNA(value)) %>%
  as.data.frame()


pres_aggr <- pres_df %>%
  group_by(EBSA,Area,variable) %>%
  summarise(
    sum = SUM(value),
    pPres = pPres(value)) %>%
  as.data.frame()

# combine
mean_aggr$sum <- NA
pres_aggr$mean <- NA
pres_aggr$sd <- NA
pres_aggr$pNA <- NA

mean_aggr$type <- "survey"
pres_aggr$type <- "presence"
mean_aggr <- mean_aggr[c("EBSA","Area","variable","type","mean","sd","sum","pPres","pNA")]
df <- rbind(mean_aggr, pres_aggr[names(survey_aggr)])

# save data
save(df, file="Aggregated/EBSA_Aggregation.Rdata")
