###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview: Summarises results. Used to create to final results table.
#
###############################################################################


# Load packages
library(reshape2)
library(dplyr)

# Go to parent directory
setwd('..')


# load data
#  presence data
load("Aggregated/EBSA_Presence_Statistics.Rdata") #presdat
#  density data
load("Aggregated/EBSA_Density_Statistics.Rdata") #densdat
# diversity productivity data
load("Aggregated/EBSA_DivProd_Statistics.Rdata") #dpdat

# Convert list to data.frame
dfpres <- melt(presdat, id.vars=c("EBSA","stat","value","lower_CI","upper_CI","ss"))
dfdens<- melt(densdat, id.vars=c("EBSA","stat","value","lower_CI","upper_CI","ss"))
dfdp <- melt(dpdat, id.vars=c("EBSA","stat","value","lower_CI","upper_CI","ss"))


# subset survey data to only include stat == mean
dfdens <- dfdens[dfdens$stat == "mean",]
# subset div prod data to only include stat == mean
dfdp <- dfdp[dfdp$stat == "mean",]
# subset presence data to only include stat == pPres
dfpres <- dfpres[dfpres$stat == "pPres",]

# bind
cols <- c("EBSA","L1","value","lower_CI","upper_CI")
df <- rbind(dfdens[cols],dfpres[cols],dfdp[cols])
colnames(df)[2] <- "Metric"

# seperate outside data
inside <- df[df$EBSA != "Outside",]
out <- df[df$EBSA == "Outside",]

# merge inside and out back together
dat <- merge(inside, out, by="Metric")
colnames(dat) <- c("Metric","EBSA","In_value","In_lower","In_upper","out","Out_value",
                   "Out_lower","Out_upper")
dat <- dat[,!names(dat) %in% "out"]

#---------------------------------------------------------#
# load species by ebsas
load("Aggregated/SpeciesByEBSAs.Rdata") #spbyebsa
spbyebsa <- melt(spbyebsa)
colnames(spbyebsa) <- c("EBSA","Metric")
spbyebsa$Imp <- "y"

# add attribute that indicates whether species was listed as important or not
dat <- merge(dat, spbyebsa, by=c("EBSA","Metric"), all.x=T)
dat$Imp[is.na(dat$Imp)] <- "n"



#---------------------------------------------------------#
# reclass 
dat$Metric <- gsub("_"," ", dat$Metric )
dat$Metric[dat$Metric == "CassinsAuklet"] <- "Cassin's Auklet"
dat$Metric[dat$Metric == "CommonMurre"] <- "Common Murre"
dat$Metric[dat$Metric == "StormPetrels"] <- "Storm Petrels"
dat$Metric[dat$Metric == "PigeonGuillemot"] <- "Pigeon Guillemot"
dat$Metric[dat$Metric == "GlaucousWingedGull"] <- "Glaucous-Winged Gull"
dat$Metric[dat$Metric == "RhinocerosAuklet"] <- "Rhinoceros Auklet"
dat$Metric[dat$Metric == "TuftedPuffin"] <- "Tufted Puffin"
dat$Metric[dat$Metric == "StellarSeaLionRookeries"] <- "Stellar Sea Lion Rookeries"
dat$Metric[dat$Metric == "SeaOtterRange"] <- "Sea Otter Range"
dat$Metric[dat$Metric == "SpongeReef"] <- "Sponge Reef"
dat$Metric[dat$Metric == "Div Fish"] <- "Fish Diversity"
dat$Metric[dat$Metric == "Div Invert"] <- "Invert Diversity"
dat$Metric[dat$Metric == "nSp Fish"] <- "Fish Richness"
dat$Metric[dat$Metric == "nSp Invert"] <- "Invert Richness"
dat$Metric[dat$Metric == "Chla mean nsb"] <- "Mean Chla"
dat$Metric[dat$Metric == "Bloom freq nsb"] <- "Bloom frequency"


#---------------------------------------------------------#
# add result column
dat$result <- "No"
dat$result[dat$In_upper > dat$Out_upper &
             dat$In_value > dat$Out_value & 
             dat$In_lower > dat$Out_lower ] <- "Moderate"
dat$result[dat$In_lower > dat$Out_upper] <- "Strong"

# remove non-important species with no support
dat <- dat[!(dat$Imp == "n" & dat$result == "No"),]
  
# re-organise data
reorg <- dat %>% group_by(EBSA, Imp, result) %>%
  summarise(species = paste(Metric, collapse = ", ")) %>%
  as.data.frame()

# sort
reorg <- reorg[order(reorg$EBSA, reorg$Imp, reorg$result),]

# Export as csv
write.csv(reorg, file= "Output/Tables/Results_Summary.csv", row.names=FALSE)



