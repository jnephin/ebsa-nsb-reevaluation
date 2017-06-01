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
# Summarises the percent of presence and no data observations for each species by EBSA
# Only includes species listed as important for the EBSAs in 
# Clark & Jamieson 2006 Phase II
#
###############################################################################

# Load packages
require(reshape2)

# global options
options(scipen = 999)

# Go to parent directory
setwd('..')

# load data
#  presence data
load("Aggregated/EBSA_Presence_Statistics.Rdata") #presdat
#  survey data
load("Aggregated/EBSA_Survey_Statistics.Rdata") #surveydat


# -------------------------------------------------#
# Organise EBSA by species table

# Convert list to data.frame
dfpres <- melt(presdat, id.vars=c("EBSA","stat"))
dfsurvey <- melt(surveydat, id.vars=c("EBSA","stat"))

# re-name list column to species
names(dfpres)[names(dfpres) == "L1"] <- "Species"
names(dfsurvey)[names(dfsurvey) == "L1"] <- "Species"

# remove mean statistic from survey dataset
dfsurvey <- dfsurvey[!dfsurvey$stat == "mean",]
dfsurvey <- dfsurvey[dfsurvey$variable == "value",]
dfpres <- dfpres[dfpres$variable == "value",]

# combine datasets
df <- rbind(dfsurvey, dfpres)
df$DataType <- c(rep("Survey", nrow(dfsurvey)), rep("Presence", nrow(dfpres)))

# round values
df$value <- round(df$value, 0)

# remove outside records
df <- df[!df$EBSA == "Outside",]

# cast stat variables
castdf <- dcast(EBSA + DataType + Species ~  stat, fun=mean, data=df)
castdf <- castdf[c("EBSA","DataType","Species","pPres","pNA")]
colnames(castdf) <- c("EBSA","DataType","Species","Presence (%)","No Data (%)")

# Export as csv
write.csv(castdf, file= "Output/Tables/EBSA_Species_SummaryTable.csv", row.names=FALSE)

