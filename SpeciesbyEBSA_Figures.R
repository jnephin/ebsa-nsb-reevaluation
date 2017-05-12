###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview: Prodcues figures that compare species statistics inside and outside 
#          ebsa. Species are those listed as important for the EBSAs in 
#          Clark & Jamieson 2006 Phase II.
#
###############################################################################


# Load packages
library(ggplot2)
library(reshape2)
library(scales)


# Go to parent directory
setwd('..')


# load data
#  presence data
load("Aggregated/EBSA_Presence_Statistics.Rdata") #presdat
#  survey data
load("Aggregated/EBSA_Survey_Statistics.Rdata") #surveydat



# Convert list to data.frame
dfpres <- melt(presdat, id.vars=c("EBSA","stat","value","lower_CI","upper_CI"))
dfsurvey <- melt(surveydat, id.vars=c("EBSA","stat","value","lower_CI","upper_CI"))


# re-name list column to species
names(dfpres)[names(dfpres) == "L1"] <- "Species"
names(dfsurvey)[names(dfsurvey) == "L1"] <- "Species"

# subset survey data to only include stat == mean
dfsurvey <- dfsurvey[dfsurvey$stat == "mean",]
# subset presence data to only include stat == pPres
dfpres <- dfpres[dfpres$stat == "pPres",]

#---------------------------------------------------------#

#  species by groups
fish <- c("ButterSole","DoverSole","EnglishSole","Eulachon","GreenSturgeon",
          "GreenUrchin","Hake", "Halibut","Halibut_phma","Herring","HerringSpawn",
          "Lingcod","Lingcod_phma","PacificCod","PacificOceanPerch","RockSole",
          "Sablefish","Sablefish_phma","SandSole","WidowRockfish","YellowmouthRockfish",
          "YellowtailRockfish","BlackRockfish","ChinaRockfish","CopperRockfish",
          "QuillbackRockfish","TigerRockfish","YelloweyeRockfish")
birds <- c("Alcids","Ancient_Murrelet","Black_legged_Kittiwake","Brandts_Cormorant",
           "Cassins_Auklet","Common_Murre","Fork_tailed_Storm_petrels",
           "Glaucous_winged_Gull","Herring_Gulls","Leachs_Storm_petrels",
           "Pelagic_Cormorant","Phalaropes","Pigeon_Guillemot","Rhinoceros_Auklet",
           "Scoters","Shearwaters","Sooty_Shearwaters","Tufted_Puffin")
mammals <- c("BlueWhale","FinWhale","GreyWhale","Humpback","KillerWhale",
             "SeaOtterRange","SeiWhale","SpermWhale","StellarSeaLionRookeries")
inverts <- c("Abalone","Geoduck","DungenessCrab","GreenUrchin","Prawn","RedSeaCucumber",
             "RedUrchin","Shrimp","SpongeReef","TannerCrab")


# -------------------------------------------------#
# Add species groups
for (d in c("dfpres", "dfsurvey")){
  df <- get(d)
  df$Group <- ""
  df$Group[df$Species %in% fish] <- "Fish"
  df$Group[df$Species %in% birds] <- "Birds"
  df$Group[df$Species %in% mammals] <- "Mammals"
  df$Group[df$Species %in% inverts] <- "Inverts"
  assign(d, df)
}

# -------------------------------------------------#
# Reclass ebsa names to short names
for (d in c("dfpres", "dfsurvey")){
  df <- get(d)
  df$EBSA[df$EBSA == "BellaBellaNearshore"] <- " BB "
  df$EBSA[df$EBSA == "BrooksPeninsula"] <- " BP "
  df$EBSA[df$EBSA == "CapeStJames"] <- " CJ "
  df$EBSA[df$EBSA == "CentralMainland"] <- " CM "
  df$EBSA[df$EBSA == "ChathamSound"] <- " CS "
  df$EBSA[df$EBSA == "DogfishBank"] <- " DB "
  df$EBSA[df$EBSA == "HaidaGwaiiNearshore"] <- " HG "
  df$EBSA[df$EBSA == "HecateStraitFront"] <- " HS "
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

dfsurvey$Species[dfsurvey$Species == "Ancient_Murrelet"] <- "Murrelet"
dfsurvey$Species[dfsurvey$Species == "Black_legged_Kittiwake"] <- "Kittiwake"
dfsurvey$Species[dfsurvey$Species == "Brandts_Cormorant"] <- "B. Cormorant"
dfsurvey$Species[dfsurvey$Species == "Cassins_Auklet"] <- "C. Auklet"
dfsurvey$Species[dfsurvey$Species == "Common_Murre"] <- "Common Murre"
dfsurvey$Species[dfsurvey$Species == "Fork_tailed_Storm_petrels"] <- "F.T. Storm Petrels"
dfsurvey$Species[dfsurvey$Species == "Glaucous_winged_Gull"] <- "G.W. Gull"
dfsurvey$Species[dfsurvey$Species == "Herring_Gulls"] <- "Herring Gulls"
dfsurvey$Species[dfsurvey$Species == "Leachs_Storm_petrels"] <- "L. Storm Petrels"
dfsurvey$Species[dfsurvey$Species == "Pelagic_Cormorant"] <- "P. Cormorant"
dfsurvey$Species[dfsurvey$Species == "Pigeon_Guillemot"] <- "Pigeon Guillemot"
dfsurvey$Species[dfsurvey$Species == "Rhinoceros_Auklet"] <- "R. Auklet"
dfsurvey$Species[dfsurvey$Species == "Tufted_Puffin"] <- "Tufted Puffin"



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
spplot(df=dfsurvey, grp="Fish", ylab="Mean Density", 
       height = 7.5, width = 6, ncol = 4, size = 8)
# birds
spplot(df=dfsurvey, grp="Birds", ylab="Mean Density", 
       height = 5.5, width = 4, ncol = 3, size = 8)
# mammals
spplot(df=dfpres, grp="Mammals", ylab="Prevalence (%)", 
       height = 4.5, width = 4, ncol = 2, size = 8)
# inverts
spplot(df=dfpres, grp="Inverts", ylab="Prevalence (%)", 
       height = 5.5, width = 3, ncol = 2, size = 8)

