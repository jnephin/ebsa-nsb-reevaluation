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
#  [1] Determine sample size for inside/outside EBSA comparison.
#
###############################################################################



# Go to parent directory
setwd('..')


#---------------------------------------------------------#
# load data
load(file="Aggregated/EBSA_Density_Overlay.Rdata") #densEBSA
load(file="Aggregated/EBSA_Presence_Overlay.Rdata") #presEBSA
load(file="Aggregated/EBSA_Diversity_Overlay.Rdata") #divEBSA
load(file="Aggregated/EBSA_Productivity_Overlay.Rdata") #prodEBSA

# calculate sample size
samplesize <- function(spdf){
  tmp <- spdf@data
  dat <- tmp[!is.na(tmp[,1]),-1]
  ss <- apply(dat, 2, sum)
  return(ss)
}

# calculate sample size total (include na)
samplesizetotal <- function(spdf){
  tmp <- spdf@data
  ss <- apply(tmp[,-1], 2, sum)
  return(ss)
}

# calculate sample size total (include na and outside EBSA)
samplesizetotalout <- function(spdf){
  outindex <- which(apply(spdf@data[,-1], 1, sum) == 0) 
  spdf@data$Outside <- 0
  spdf@data$Outside[outindex] <- 1
  tmp <- spdf@data
  ss <- apply(tmp[,-1], 2, sum)
  return(ss)
}

# calculate sample size with outside area included
samplesizeout <- function(spdf){
  outindex <- which(apply(spdf@data[,-1], 1, sum) == 0) 
  spdf@data$Outside <- 0
  spdf@data$Outside[outindex] <- 1
  tmp <- spdf@data
  dat <- tmp[!is.na(tmp[,1]),-1]
  ss <- apply(dat, 2, sum)
  return(ss)
}


# diversity and producitivty sample sizes
div_ss <- sapply(divEBSA, FUN=samplesizeout)
prod_ss <- sapply(prodEBSA, FUN=samplesizeout)
tss <- sapply(prodEBSA, FUN=samplesizetotalout)


# combine into diveristy ss table
tab <- merge(div_ss[,1],prod_ss[,1], by="row.names")
tab <- merge(tss[,1],tab, by.x="row.names", by.y="Row.names")
colnames(tab) <- c("EBSA","Total","Diversity","Productivity")

# calculate percent of total sample size
tab$DivPercent <- tab$Diversity / tab$Total * 100
tab$DivPercent <- round(tab$DivPercent)
tab$ProdPercent <- tab$Productivity / tab$Total * 100
tab$ProdPercent <- round(tab$ProdPercent)

# save
write.csv(tab, file = "Output/Tables/SampleSize_DivProd.csv", row.names = FALSE)


#---------------------------------------------------------#
# for density data


# load species by ebsas
load("Aggregated/SpeciesByEBSAs.Rdata") #spbyebsa


# empty list
dens_ss <- NULL

# loop through each species in survey data
for(s in names(densEBSA)){
  
  # aggregated data for species s
  spdf <- densEBSA[[s]]
  
  # ebsas where the species was listed as important
  eb <- spbyebsa[[s]]
  
  # add outside attribute - area outside of ebsa for which the sp is important
  out <- spdf@data[eb]
  outindex <- which(apply(out, 1, sum) == 0)
  spdf@data$Outside <- 0
  spdf@data$Outside[outindex] <- 1
  
  # get sample size
  ss <- samplesize(spdf)
  tt <- samplesizetotal(spdf)
  ss_ebsa <- ss[c(eb,"Outside")]
  tt_ebsa <- tt[c(eb,"Outside")]
  ss_df <- data.frame(SampleSize=ss_ebsa,Total=tt_ebsa,Species=s)
  ss_df$EBSA <- row.names(ss_df)
  
  # add to list
  dens_ss <- rbind(dens_ss, ss_df)
}

# fix row names
rownames(dens_ss) <- 1:nrow(dens_ss)


# -------------------------------------------------#
# Reclass species names to common names
dens_ss$Species <- gsub("_"," ", dens_ss$Species)
dens_ss$Species[dens_ss$Species == "CassinsAuklet"] <- "Cassin's Auklet"
dens_ss$Species[dens_ss$Species == "CommonMurre"] <- "Common Murre"
dens_ss$Species[dens_ss$Species == "StormPetrels"] <- "Storm Petrels"
dens_ss$Species[dens_ss$Species == "GlaucousWingedGull"] <- "Glaucous-Winged Gull"
dens_ss$Species[dens_ss$Species == "PigeonGuillemot"] <- "Pigeon Guillemot"
dens_ss$Species[dens_ss$Species == "RhinocerosAuklet"] <- "Rhinoceros Auklet"
dens_ss$Species[dens_ss$Species == "TuftedPuffin"] <- "Tufted Puffin"
dens_ss$Species[dens_ss$Species == "StellarSeaLionRookeries"] <- "Stellar Sea Lion Rookeries"
dens_ss$Species[dens_ss$Species == "SeaOtterRange"] <- "Sea Otter Range"
dens_ss$Species[dens_ss$Species == "SpongeReef"] <- "Sponge Reef"


dtab <- dens_ss
# calculate percent of total sample size
dtab$Percent <- dtab$SampleSize / dtab$Total * 100
dtab$Percent <- round(dtab$Percent)

# seperate inside and outside
out <- dtab[dtab$EBSA == "Outside",]
inside <- dtab[dtab$EBSA != "Outside",]

# merge inside and outside
denstab <- merge(inside,out, by = "Species")

### start here


# order table
denstab <- denstab[order(denstab$EBSA.x, denstab$Species), 
                   c("EBSA.x","Species","SampleSize.x","Percent.x", 
                     "SampleSize.y","Percent.y")]
denstab$Species <- sub("_"," ", denstab$Species)
colnames(denstab) <- c("EBSA","Species","InsideSampleSize","InsidePercent",
                       "OutsideSampleSize","OutsidePercent")


# save
write.csv(denstab, file = "Output/Tables/SampleSize_Density.csv", row.names = FALSE)

