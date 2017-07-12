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
#       Produces figures that compare species statistics inside and outside 
#       ebsa. Species are those listed as important for the EBSAs in 
#       Clark & Jamieson 2006 Phase II.
#
###############################################################################


# Load packages
library(ggplot2)
library(reshape2)
library(scales)
library(rgdal)

# Go to parent directory
setwd('..')


#---------------------------------------------------------#
# load data
#  presence data
load("Aggregated/EBSA_Presence_Statistics.Rdata") #presdat
#  density data
load("Aggregated/EBSA_Density_Statistics.Rdata") #densdat

# Convert list to data.frame
dfpres <- melt(presdat, id.vars=c("EBSA","stat","value","lower_CI","upper_CI"))
dfdens<- melt(densdat, id.vars=c("EBSA","stat","value","lower_CI","upper_CI"))

# re-name list column to species
names(dfpres)[names(dfpres) == "L1"] <- "Species"
names(dfdens)[names(dfdens) == "L1"] <- "Species"

# add missing data column
dfNA <- dfdens[dfdens$stat == "pNA",c("EBSA","Species","value")]
colnames(dfNA)[3] <- "pNA" 
dfNA$pData <- round(100 - dfNA$pNA)
dfdens <- merge(dfdens, dfNA, by=c("EBSA","Species"))
  
# subset survey data to only include stat == mean
dfdens <- dfdens[dfdens$stat == "mean",]
# subset presence data to only include stat == pPres
dfpres <- dfpres[dfpres$stat == "pPres",]


#---------------------------------------------------------#
# load species by ebsas
load("Aggregated/SpeciesByEBSAs.Rdata") #spbyebsa
spbyebsa <- melt(spbyebsa)
colnames(spbyebsa) <- c("EBSA","Species")

# remove species for ebsas which they were not listed as important
for (d in c("dfpres", "dfdens")){
  df <- get(d)
  mer <- merge(spbyebsa, df, by=c("EBSA", "Species"))
  mer$EBSA <- as.character(mer$EBSA)
  mer <- rbind(mer, df[df$EBSA == "Outside",])
  assign(d, mer)
}


#---------------------------------------------------------#

#  species by groups
gdbs=c("Fish","MarineMammals","MarineBirds","Invert")
groups <- NULL
for(gdb in gdbs){
  fc_list <- ogrListLayers(paste0("Data/",gdb,".gdb"))
  assign(gdb, fc_list)
}

# -------------------------------------------------#
# Add species groups
for (d in c("dfpres", "dfdens")){
  df <- get(d)
  df$Group <- ""
  df$Group[df$Species %in% Fish] <- "Fish"
  df$Group[df$Species %in% MarineBirds] <- "Birds"
  df$Group[df$Species %in% MarineMammals] <- "Cetaceans"
  df$Group[df$Species %in% c(Invert,"SpongeReef",
                             "StellarSeaLionRookeries","SeaOtterRange")] <- "Other"
  assign(d, df)
}

# -------------------------------------------------#
# Reclass ebsa names to short names
for (d in c("dfpres", "dfdens")){
  df <- get(d)
  df$EBSA[df$EBSA == "BellaBellaNearshore"] <- " BB "
  df$EBSA[df$EBSA == "BrooksPeninsula"] <- " BP "
  df$EBSA[df$EBSA == "CapeStJames"] <- " CSJ "
  df$EBSA[df$EBSA == "CentralMainland"] <- " CM "
  df$EBSA[df$EBSA == "ChathamSound"] <- " CS "
  df$EBSA[df$EBSA == "DogfishBank"] <- " DB "
  df$EBSA[df$EBSA == "HaidaGwaiiNearshore"] <- " HG "
  df$EBSA[df$EBSA == "HecateStraitFront"] <- " HSF "
  df$EBSA[df$EBSA == "LearmonthBank"] <- " LB "
  df$EBSA[df$EBSA == "McIntyreBay"] <- " MB "
  df$EBSA[df$EBSA == "NorthIslandsStraits"] <- " NIS "
  df$EBSA[df$EBSA == "ScottIslands"] <- " SI "
  df$EBSA[df$EBSA == "ShelfBreak"] <- " SB "
  df$EBSA[df$EBSA == "SpongeReefs"] <- " SR "
  df$EBSA[df$EBSA == "Outside"] <- "OUT"
  df$Area <- "In"
  df$Area[df$EBSA == "OUT"] <- "Out"
  assign(d, df)
}


# -------------------------------------------------#
# Reclass species names to common names
for (d in c("dfdens", "dfpres")){
  df <- get(d)
  df$Species <- gsub("_"," ", df$Species)
  df$Species[df$Species == "Fork tailed Storm petrel"] <- "Fork-tailed Storm-petrel"
  df$Species[df$Species == "Leachs Storm petrel"] <- "Leach's Storm petrel"
  df$Species[df$Species == "Cassins Auklet"] <- "Cassin's Auklet"
  df$Species[df$Species == "Red necked Phalarope"] <- "Red-necked Phalarope"
  df$Species[df$Species == "Yelloweye line"] <- "Yelloweye rockfish line"
  df$Species[df$Species == "RedUrchin"] <- "Red Urchin"
  df$Species[df$Species == "GreenUrchin"] <- "Green Urchin"
  df$Species[df$Species == "RedSeaCucumber"] <- "Red Sea Cucumber"
  df$Species[df$Species == "DungenessCrab"] <- "Dungeness Crab"
  df$Species[df$Species == "StellarSeaLionRookeries"] <- "Stellar Sea Lion Rookeries"
  df$Species[df$Species == "SeaOtterRange"] <- "Sea Otter Range"
  df$Species[df$Species == "TannerCrab"] <- "Tanner Crab"
  df$Species[df$Species == "SpongeReef"] <- "Sponge Reef"
  assign(d, df)
}



# Save dfpres and dfdens to plot with maps
save(dfdens, file="Aggregated/Denisty_PlotData.Rdata")
save(dfpres, file="Aggregated/Presence_PlotData.Rdata")

# -------------------------------------------------#
# plotting functions

densplot <- function(df, grp, ylab, height, width, ncol, size=size){
  # subset by species
  dfsub <- df[df$Group == grp,]
  # plot
  gfig <- ggplot(data = dfsub, aes(x=EBSA,y=value,colour=Area, size=pData))+
    geom_point(pch=16)+
    geom_errorbar(aes(ymin=lower_CI,ymax=upper_CI), size=1, width=0)+
    labs(x="", y=ylab)+
    scale_y_continuous(breaks = pretty_breaks(n=4))+
    scale_size_area(max_size = 3, name = "Data \ncoverage\n (%)")+
    facet_wrap(~Species, scales="free", ncol=ncol)+
    scale_colour_manual(values=c("#7fc97f","#386cb0"), guide=F)+
    theme(panel.border = element_rect(fill=NA, colour="black"),
          panel.background = element_rect(fill="white",colour="black"),
          strip.background = element_rect(fill="white",colour=NA),
          strip.text = element_text(size=size+1, colour = "black", hjust=0, vjust=1),
          axis.ticks = element_line(colour="black"),
          panel.grid= element_blank(),
          axis.ticks.length = unit(0.1,"cm"),
          axis.text = element_text(size=size, colour = "black"),
          axis.title = element_text(size=size+1, colour = "black"),
          panel.spacing = unit(0.1, "lines"),
          legend.key = element_blank(),
          plot.margin = unit(c(.2,.2,.2,.2), "lines"))
  tiff(file=file.path("Output/Figures", paste0(grp, ".tif")),
       width = width , height = height, units = "in", res = 100)
  print(gfig)
  dev.off()
}


presplot <- function(df, grp, ylab, height, width, ncol, size=size){
  # subset by species
  dfsub <- df[df$Group == grp,]
  # plot
  gfig <- ggplot(data = dfsub, aes(x=EBSA,y=value,colour=Area))+
    geom_point(pch=16)+
    geom_errorbar(aes(ymin=lower_CI,ymax=upper_CI), size=1, width=0)+
    labs(x="", y=ylab)+
    scale_y_continuous(breaks = pretty_breaks(n=4))+
    facet_wrap(~Species, scales="free", ncol=ncol)+
    scale_colour_manual(values=c("#7fc97f","#386cb0"), guide=F)+
    theme(panel.border = element_rect(fill=NA, colour="black"),
          panel.background = element_rect(fill="white",colour="black"),
          strip.background = element_rect(fill="white",colour=NA),
          strip.text = element_text(size=size+1, colour = "black", hjust=0, vjust=1),
          axis.ticks = element_line(colour="black"),
          panel.grid= element_blank(),
          axis.ticks.length = unit(0.1,"cm"),
          axis.text = element_text(size=size, colour = "black"),
          axis.title = element_text(size=size+1, colour = "black"),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = unit(c(.2,.2,.2,.2), "lines"))
  tiff(file=file.path("Output/Figures", paste0(grp, ".tif")),
       width = width , height = height, units = "in", res = 100)
  print(gfig)
  dev.off()
}


# -------------------------------------------------#
# figures
# fish
densplot(df=dfdens, grp="Fish", ylab="Mean Density", 
       height = 8.5, width = 7, ncol = 4, size = 8)
# birds
densplot(df=dfdens, grp="Birds", ylab="Mean Density", 
       height = 6.8, width = 5.5, ncol = 3, size = 8)
# mammals
densplot(df=dfdens, grp="Cetaceans", ylab="Mean Density", 
       height = 4, width = 5.5, ncol = 2, size = 8)
# inverts
presplot(df=dfpres, grp="Other", ylab="Prevalence (%)", 
       height = 5, width = 5.2, ncol = 3, size = 8)

