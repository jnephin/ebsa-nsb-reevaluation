# Load packages
library(ggplot2)
library(reshape2)

# global options
options(scipen = 999)

# load data
load(file="Aggregated/EBSA_Overlay.Rdata") #dat

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

df$variable[df$variable == "Hippoglossus_stenolepis"] <- "Halibut"
df$variable[df$variable == "Clupea_pallasii_pallasii"] <- "Herring"
df$variable[df$variable == "Gadus_macrocephalus"] <- "PacificCod"
df$variable[df$variable == "Ophiodon_elongatus"] <- "Lingcod"
df$variable[df$variable == "Merluccius_productus"] <- "Hake"
df$variable[df$variable == "Anoplopoma_fimbria"] <- "Sablefish"
df$variable[df$variable == "Microstomus_pacificus"] <- "DoverSole"
df$variable[df$variable == "Sebastes_alutus"] <- "PacificOceanPerch"
df$variable[df$variable == "Sebastes_flavidus"] <- "YellowtailRockfish"
df$variable[df$variable == "Sebastes_reedi"] <- "YellowmouthRockfish"
df$variable[df$variable == "Sebastes_saxicola"] <- "WidowRockfish"
df$variable[df$variable == "Thaleichthys_pacificus"] <- "Eulachon"

# -------------------------------------------------#
# Species important to each EBSA

BellaBellaNearshore <- c("SeaOtterRange","Geoduck","RedUrchin","RedSeaCucumber",
                         "Shrimp","KillerWhale","Herring")
BrooksPeninsula <- c("SeaOtterRange","Lingcod")
CapeStJames <- c("Halibut", "Halibut_Longline","StellarSeaLionRookeries",
                 "Humpback","BlueWhale","FinWhale")
CentralMainland  <- c("SeaOtterRange","StellarSeaLionRookeries","KillerWhale","FinWhale",
                      "Humpback","GreyWhale","GreenUrchin","RedUrchin","Abalone")
ChathamSound  <- c("GreenUrchin","DungenessCrab","Shrimp","Herring","KillerWhale","Humpback")
DogfishBank <-  c("DungenessCrab","PacificCod")
HaidaGwaiiNearshore <- c("FinWhale","Humpback","RedUrchin","RedSeaCucumber","Herring",
                         "PacificCod","Abalone","StellarSeaLionRookeries")
HecateStraitFront <- NULL
LearmonthBank  <-  c("FinWhale")
McIntyreBay <-  c("DungenessCrab","Halibut","Halibut_Longline","Eulachon","Herring",
                  "KillerWhale","Humpback")
NorthIslandsStraits <- c("KillerWhale","GreyWhale", "Humpback","Herring","Shrimp",
                         "GreenUrchin","SeaOtterRange")
ScottIslands <-  c("SeaOtterRange","GreyWhale","Humpback","StellarSeaLionRookeries",
                   "PacificCod","Lingcod","Sablefish","Sablefish_Longline","Hake",
                   "Herring","WidowRockfish")
ShelfBreak <- c("Hake","Humpback","TannerCrab","SpermWhale", "BlueWhale",
                "FinWhale", "Eulachon","Sablefish","Sablefish_Longline","DoverSole",
                "PacificOceanPerch","YellowtailRockfish","YellowmouthRockfish")
SpongeReefs <-  c("SpongeReefs")

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
        panel.spacing = unit(.2, "lines"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines"))

spboxplot

pdf("Output/Figures/SpeciesbyEBSA_boxplot.pdf", width=7, height=9)
spboxplot
dev.off()



# -------------------------------------------------#
# box plots for diveristy

divdf <- df[df$variable %in% c("Div_Fish","Div_Invert"),]
divdf$label <- sub(".*_","", divdf$variable)

divboxplot <- ggplot(data = divdf, aes(x=label,y=value,fill=Area))+
  geom_boxplot(notch = FALSE, width=.8)+
  labs(x="",y="Mean species diversity / 5km grid cell")+
  scale_fill_manual(values=c("#7fc97f","#386cb0"))+
  facet_wrap(~EBSA, scales="free", ncol=3)+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
        strip.background = element_rect(fill="white",colour="black"),
        axis.ticks = element_line(colour="black"),
        panel.grid= element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        axis.text.y = element_text(size=9, colour = "black"),
        axis.text.x = element_text(size=9, colour = "black"),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11, face="plain"),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.key.height = unit(.5,"cm"), legend.key.width = unit(.4,"cm"),
        legend.position = c(.85,.1),
        panel.spacing = unit(.2, "lines"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines"))

divboxplot

pdf("Output/Figures/DiversitybyEBSA_boxplot.pdf", width=7, height=9)
divboxplot
dev.off()



# -------------------------------------------------#
# box plots for richness 

richdf <- df[df$variable %in% c("nSp_Fish","nSp_Invert"),]
richdf$label <- sub(".*_","", richdf$variable)

richboxplot <- ggplot(data = richdf, aes(x=label,y=value,fill=Area))+
  geom_boxplot(notch = FALSE, width=.8)+
  labs(x="",y="Mean species richness / 5km grid cell")+
  scale_fill_manual(values=c("#7fc97f","#386cb0"))+
  facet_wrap(~EBSA, scales="free", ncol=3)+
  theme(panel.border = element_rect(fill=NA, colour="black"),
        panel.background = element_rect(fill="white",colour="black"),
        strip.background = element_rect(fill="white",colour="black"),
        axis.ticks = element_line(colour="black"),
        panel.grid= element_blank(),
        axis.ticks.length = unit(0.1,"cm"),
        axis.text.y = element_text(size=9, colour = "black"),
        axis.text.x = element_text(size=9, colour = "black"),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=11, face="plain"),
        legend.background = element_blank(), legend.key = element_blank(),
        legend.key.height = unit(.5,"cm"), legend.key.width = unit(.4,"cm"),
        legend.position = c(.85,.1),
        panel.spacing = unit(.2, "lines"),
        plot.margin = unit(c(.2,.2,.2,.2), "lines"))

richboxplot

pdf("Output/Figures/RichnessbyEBSA_boxplot.pdf", width=7, height=9)
richboxplot
dev.off()
