###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview: Produces figures that compare species statistics inside and outside 
#          ebsa. Species are those listed as important for the EBSAs in 
#          Clark & Jamieson 2006 Phase II.
#
###############################################################################


# Load packages
library(ggplot2)
library(reshape2)
library(scales)
library(rgdal)

# Go to parent directory
setwd('..')


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

# subset survey data to only include stat == mean
dfdens <- dfdens[dfdens$stat == "mean",]
# subset presence data to only include stat == pPres
dfpres <- dfpres[dfpres$stat == "pPres",]

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
# Reclass brid names to short names

dfdens$Species <- gsub("_"," ", dfdens$Species)
dfdens$Species <- gsub("rockfish","RF", dfdens$Species)
dfdens$Species[dfdens$Species == "Black footed Albatross"] <- "Albatross"
dfdens$Species[dfdens$Species == "Cassins Auklet"] <- "C. Auklet"
dfdens$Species[dfdens$Species == "Fork tailed Storm petrel"] <- "F.T. Storm Petrels"
dfdens$Species[dfdens$Species == "Large_Gulls"] <- "Large Gulls"
dfdens$Species[dfdens$Species == "Leachs Storm petrel"] <- "L. Storm Petrels"
dfdens$Species[dfdens$Species == "Red necked Phalarope"] <- "Phalarope"
dfdens$Species[dfdens$Species == "Rhinoceros Auklet"] <- "R. Auklet"
dfdens$Species[dfdens$Species == "Tufted Puffin"] <- "Tufted Puffin"
dfdens$Species[dfdens$Species == "Pacific Ocean perch"] <- "P.O. perch"
dfdens$Species[dfdens$Species == "Yelloweye line"] <- "Yelloweye RF line"


# -------------------------------------------------#
# plotting function

spplot <- function(df, grp, ylab, height, width, ncol, size=size){
  # subset by species
  dfsub <- df[df$Group == grp,]
  # plot
  gfig <- ggplot(data = dfsub, aes(x=EBSA,y=value,colour=Area))+
    geom_point(pch=16, size=2)+
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
spplot(df=dfdens, grp="Fish", ylab="Mean Density", 
       height = 7.5, width = 6, ncol = 4, size = 8)
# birds
spplot(df=dfdens, grp="Birds", ylab="Mean Density", 
       height = 6.8, width = 4.5, ncol = 3, size = 8)
# mammals
spplot(df=dfdens, grp="Cetaceans", ylab="Mean Density", 
       height = 4, width = 4.5, ncol = 2, size = 8)
# inverts
spplot(df=dfpres, grp="Other", ylab="Prevalence (%)", 
       height = 5, width = 5.2, ncol = 3, size = 8)

