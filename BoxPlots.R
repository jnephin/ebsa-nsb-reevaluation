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
# Creates boxplot figures for species abundance and diveristy/richness data by EBSA
# * Only includes species listed as important for the EBSAs in Clark & Jamieson 2006 Phase II
#
###############################################################################

# Load packages
library(ggplot2)
library(reshape2)
library(reshape2)

# Go to parent directory
setwd('..')

# global options
options(scipen = 999)

# load data
load(file="Aggregated/EBSA_Mean_Overlay.Rdata") #dat

# Convert list to data.frame
df <- melt(dat)

# EBSA column
df$EBSA <- df$L1

# Remove L1 column
df <- df[!names(df) == "L1"]

# convert variable to character
df$variable <- as.character(df$variable)

# -------------------------------------------------#
# Reclass species names to common names

df$species[df$species == "Hippoglossus_stenolepis"] <- "Halibut"
df$species[df$species == "Clupea_pallasii_pallasii"] <- "Herring"
df$species[df$species == "Gadus_macrocephalus"] <- "PacificCod"
df$species[df$species == "Ophiodon_elongatus"] <- "Lingcod"
df$species[df$species == "Merluccius_productus"] <- "Hake"
df$species[df$species == "Anoplopoma_fimbria"] <- "Sablefish"
df$species[df$species == "Sebastes_alutus"] <- "PacificOceanPerch"
df$species[df$species == "Sebastes_flavidus"] <- "YellowtailRockfish"
df$species[df$species == "Sebastes_reedi"] <- "YellowmouthRockfish"
df$species[df$species == "Sebastes_saxicola"] <- "WidowRockfish"
df$species[df$species == "Thaleichthys_pacificus"] <- "Eulachon"
df$species[df$species == "Microstomus_pacificus"] <- "DoverSole"
df$species[df$species == "Psettichthys_melanostictus"] <- "SandSole"
df$species[df$species == "Isopsetta_isolepis"] <- "ButterSole"
df$species[df$species == "Parophrys_vetulus"] <- "EnglishSole"
df$species[df$species == "Lepidopsetta_bilineata"] <- "RockSole"
df$species[df$species == "Acipenser_medirostris"] <- "GreenSturgeon"
df$species[df$species == "Sebastes_ruberrimus"] <- "YelloweyeRockfish"
df$species[df$species == "Sebastes_caurinus"] <- "CopperRockfish"
df$species[df$species == "Sebastes_nigrocinctus"] <- "TigerRockfish"
df$species[df$species == "Sebastes_nebulosus"] <- "ChinaRockfish"
df$species[df$species == "Sebastes_maliger"] <- "QuillbackRockfish"
df$species[df$species == "Sebastes_melanops"] <- "BlackRockfish"
df$species[df$species == "Rissa_tridactyla"] <- "Black-legged_Kittiwake"
df$species[df$species == "Phalacrocorax_penicillatus"] <- "Brandts_Cormorant"
df$species[df$species == "Ptychoramphus_aleuticus"] <- "Cassins_Auklet"
df$species[df$species == "Uria_aalge"] <- "Common_Murre"
df$species[df$species == "Larus_glaucescens"] <- "Glaucous-winged_Gull"
df$species[df$species == "Oceanodroma_leucorhoa"] <- "Leachs_Storm-petrels"
df$species[df$species == "Oceanodroma_furcata"] <- "Fork-tailed_Storm-petrels"
df$species[df$species == "Phalacrocorax_pelagicus"] <- "Pelagic_Cormorant"
df$species[df$species == "Cepphus_columba"] <- "Pigeon_Guillemot"
df$species[df$species == "Cerorhinca_monocerata"] <- "Rhinoceros_Auklet"
df$species[df$species == "Fratercula_cirrhata"] <- "Tufted_Puffin"
df$species[df$species == "Melanitta"] <- "Scoters"
df$species[df$species == "Alcidae"] <- "Alcids"
df$species[df$species == "Phalaropus"] <- "Phalaropes"

# -------------------------------------------------#
# Species important to each EBSA

HecateStraitFront <- NULL
McIntyreBay <-  c("DungenessCrab","Halibut","Halibut_Longline","Eulachon","Herring",
                  "KillerWhale","Humpback")
DogfishBank <-  c("DungenessCrab","PacificCod", "Shearwaters", "Phalaropes",
                  "Herring_Gulls","Ancient_Murrelet", "Scoters",
                  "SandSole","ButterSole","EnglishSole","RockSole")
LearmonthBank  <-  c("FinWhale","GreyWhale", "Alcids")
BrooksPeninsula <- c("SeaOtterRange","Lingcod","GreenSturgeon","Phalaropes","Common_Murre",
                     "Tufted_Puffin","Sooty_Shearwater","Glaucous-winged_Gull",
                     "Rhinoceros_Auklet","Black-legged_Kittiwake")
CapeStJames <- c("Halibut", "Halibut_Longline","StellarSeaLionRookeries",
                 "Humpback","BlueWhale","FinWhale")
ShelfBreak <- c("Hake","Humpback","TannerCrab","SpermWhale", "BlueWhale","SeiWhale",
                "FinWhale","Eulachon","Sablefish","Sablefish_Longline","DoverSole",
                "PacificOceanPerch","YellowtailRockfish","YellowmouthRockfish","GreyWhale",
                "Cassins_Auklet","Ancient_Murrelet","Rhinoceros_Auklet","Tufted_Puffin",
                "Fork-tailed_Storm-petrels", "Leachs_Storm-petrels")
ScottIslands <-  c("SeaOtterRange","GreyWhale","Humpback","StellarSeaLionRookeries",
                   "PacificCod","Lingcod","Sablefish","Sablefish_Longline","Hake",
                   "Herring","WidowRockfish","Cassins_Auklet","Rhinoceros_Auklet",
                   "Tufted_Puffin","Common_Murre","Brandts_Cormorant","Pelagic_Cormorant",
                   "Pigeon_Guillemot","Glaucous-winged_Gull","Fork-tailed_Storm-petrels",
                   "Leachs_Storm-petrels")
NorthIslandsStraits <- c("KillerWhale","GreyWhale", "Humpback","Herring","Shrimp", "Prawn",
                         "GreenUrchin","SeaOtterRange","Rhinoceros_Auklet",
                         "Fork-tailed_Storm-petrels", "Leachs_Storm-petrels")
SpongeReefs <-  c("SpongeReefs")
ChathamSound  <- c("GreenUrchin","DungenessCrab","Shrimp","HerringSpawn","KillerWhale",
                   "Humpback","Scoters")
CentralMainland  <- c("SeaOtterRange","StellarSeaLionRookeries","KillerWhale","FinWhale",
                      "Humpback","GreyWhale","RedSeaCucumber") # not in table: overlap with sea cuc IA, rockfish are inshore spp found in conservation priorities SAR
BellaBellaNearshore <- c("SeaOtterRange","Geoduck","RedUrchin","RedSeaCucumber",
                         "Shrimp","KillerWhale","HerringSpawn")
HaidaGwaiiNearshore <- c("FinWhale","Humpback","RedUrchin","RedSeaCucumber","HerringSpawn",
                         "PacificCod","Abalone","StellarSeaLionRookeries","Sooty_Shearwaters")


spint <- list(BellaBellaNearshore=BellaBellaNearshore,
              BrooksPeninsula=BrooksPeninsula,
              CapeStJames=CapeStJames,
              CentralMainland=CentralMainland,
              ChathamSound=ChathamSound,
              DogfishBank=DogfishBank,
              HaidaGwaiiNearshore=HaidaGwaiiNearshore,
              LearmonthBank=LearmonthBank,
              McIntyreBay=McIntyreBay,
              NorthIslandsStraits=NorthIslandsStraits,
              ScottIslands=ScottIslands,
              ShelfBreak=ShelfBreak,
              SpongeReefs=SpongeReefs)


# ----------------------------------------------------#
# Subset df with only species of interest to each EBSA

areas <- c("BellaBellaNearshore","BrooksPeninsula","CapeStJames","CentralMainland","ChathamSound",
           "DogfishBank","HaidaGwaiiNearshore","LearmonthBank","McIntyreBay","NorthIslandsStraits",
           "ScottIslands","ShelfBreak","SpongeReefs")
subint <- NULL
for(a in areas){
  tmp <- df[df$variable %in% spint[[a]] & df$EBSA == a,]
  subint <- rbind(subint, tmp)
}

# -------------------------------------------------#
# box plots for species

spboxplot <- ggplot(data = subint, aes(x=variable,y=value,fill=Area))+
  geom_boxplot(notch = FALSE, width=.8)+
  labs(x="",y="Log (Mean density / 5km grid cell)")+
  scale_y_log10()+
  scale_fill_manual(values=c("#7fc97f","#386cb0"))+
  facet_wrap(~EBSA, scales="free", ncol=3)+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
        strip.background = element_rect(fill="white",colour="black"),
        axis.ticks = element_line(colour="black"),
        panel.grid= element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        axis.text.y = element_text(size=9, colour = "black"),
        axis.text.x = element_text(size=9, colour = "black", angle=30, hjust=1),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11, face="plain"),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.key.height = unit(.5,"cm"), legend.key.width = unit(.4,"cm"),
        legend.position = c(.85,.1),
        panel.spacing = unit(.05, "lines"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines"))

spboxplot

pdf("Output/Figures/SpeciesEBSA_boxplot.pdf", width=7, height=9)
spboxplot
dev.off()



# -------------------------------------------------#
# box plots for diveristy

divdf <- df[df$variable %in% c("Div_Fish","Div_Invert"),]
divdf$label <- sub(".*_","", divdf$variable)

divboxplot <- ggplot(data = divdf, aes(x=EBSA,y=value,fill=Area))+
  geom_boxplot(notch = FALSE, width=.8)+
  labs(x="",y="Mean species diversity / 5km grid cell")+
  scale_fill_manual(values=c("#7fc97f","#386cb0"))+
  facet_grid(label~., scales="free")+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
        strip.background = element_rect(fill="white",colour="black"),
        axis.ticks = element_line(colour="black"),
        panel.grid= element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        axis.text.y = element_text(size=9, colour = "black"),
        axis.text.x = element_text(size=9, colour = "black", angle=40, hjust=1),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11, face="plain"),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.key.height = unit(.5,"cm"), legend.key.width = unit(.4,"cm"),
        legend.position = "right",
        panel.spacing = unit(.2, "lines"),
        plot.margin = unit(c(.5,.5,.5,.5), "lines"))

divboxplot

pdf("Output/Figures/DiversityEBSA_boxplot.pdf", width=9, height=7)
divboxplot
dev.off()



# -------------------------------------------------#
# box plots for richness

richdf <- df[df$variable %in% c("nSp_Fish","nSp_Invert"),]
richdf$label <- sub(".*_","", richdf$variable)

richboxplot <- ggplot(data = richdf, aes(x=EBSA,y=value,fill=Area))+
  geom_boxplot(notch = FALSE, width=.8)+
  labs(x="",y="Mean species richness / 5km grid cell")+
  scale_fill_manual(values=c("#7fc97f","#386cb0"))+
  facet_grid(label~., scales="free")+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
        strip.background = element_rect(fill="white",colour="black"),
        axis.ticks = element_line(colour="black"),
        panel.grid= element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        axis.text.y = element_text(size=9, colour = "black"),
        axis.text.x = element_text(size=9, colour = "black", angle=40, hjust=1),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11, face="plain"),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.key.height = unit(.5,"cm"), legend.key.width = unit(.4,"cm"),
        legend.position = "right",
        panel.spacing = unit(.2, "lines"),
        plot.margin = unit(c(.5,.5,.5,.5), "lines"))

richboxplot

pdf("Output/Figures/RichnessbyEBSA_boxplot.pdf", width=9, height=7)
richboxplot
dev.off()





# -------------------------------------------------#
# box plots for productivity

load("Aggregated/EBSA_Productivity_Overlay.Rdata")
colnames(dat)[1:2] <- c("Mean Chlorophyll Concentration", "Bloom Frequency")
prod <- melt(dat)



#plot
Chlaboxplot <- ggplot(data = prod, aes(x=EBSA,y=value,fill=Area))+
  geom_boxplot(notch = TRUE, width=.8)+
  labs(x="",y="Mean Chlorophyll Concentration")+
  scale_fill_manual(values=c("#7fc97f","#386cb0"))+
  facet_grid(variable~., scales="free")+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
        strip.background = element_rect(fill="white",colour="black"),
        axis.ticks = element_line(colour="black"),
        panel.grid= element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        axis.text.y = element_text(size=9, colour = "black"),
        axis.text.x = element_text(size=9, colour = "black", angle=40, hjust=1),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11, face="plain"),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.key.height = unit(.5,"cm"), legend.key.width = unit(.4,"cm"),
        legend.position = "right",
        panel.spacing = unit(.2, "lines"),
        plot.margin = unit(c(.5,.5,.5,.5), "lines"))

Chlaboxplot

pdf("Output/Figures/ProductivityEBSA_boxplot.pdf", width=9, height=7)
Chlaboxplot
dev.off()
