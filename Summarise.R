# Load packages
require(reshape2)
require(ggplot2)

# global options
options(scipen = 999)


# load data
load(file="Aggregated/EBSA_Aggregation.Rdata") #df


# -------------------------------------------------#
# organise data frame

# colname variable
colnames(df)[3] <- "species"

# round values
df$mean <- round(df$mean, 2)
df$sd <- round(df$sd, 2)
df$sum <- round(df$sum, 2)
df$pPres <- round(df$pPres, 2)
df$pNA <- round(df$pNA, 2)

# melt
melted <- melt(df)

# cast
casted <- dcast(EBSA + species + type ~  variable + Area, fun=mean, data=melted)
df <- casted[c("EBSA","species","type","mean_in","mean_out","sd_in","sd_out",
               "sum_in","sum_out","pPres_in","pPres_out","pNA_in","pNA_out")]

# percent
df$pPres_in <- df$pPres_in * 100
df$pPres_out <- df$pPres_out * 100
df$pNA_in <- df$pNA_in * 100
df$pNA_out <- df$pNA_out * 100



# -------------------------------------------------#
# Reclass species names to common names

df$species[df$species == "Hippoglossus_stenolepis"] <- "Halibut"
df$species[df$species == "Clupea_pallasii_pallasii"] <- "Herring"
df$species[df$species == "Gadus_macrocephalus"] <- "PacificCod"
df$species[df$species == "Ophiodon_elongatus"] <- "Lingcod"
df$species[df$species == "Merluccius_productus"] <- "Hake"
df$species[df$species == "Anoplopoma_fimbria"] <- "Sablefish"
df$species[df$species == "Microstomus_pacificus"] <- "DoverSole"
df$species[df$species == "Sebastes_alutus"] <- "PacificOceanPerch"
df$species[df$species == "Sebastes_flavidus"] <- "YellowtailRockfish"
df$species[df$species == "Sebastes_reedi"] <- "YellowmouthRockfish"
df$species[df$species == "Sebastes_saxicola"] <- "WidowRockfish"
df$species[df$species == "Thaleichthys_pacificus"] <- "Eulachon"

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
  tmp <- df[df$species %in% spint[[a]] & df$EBSA == a,]
  subint <- rbind(subint, tmp)
}




# ----------------------------------------------------#
# Export as csv
write.csv(subint, file= "Output/EBSA_Species_SummaryTable.csv", row.names=FALSE, na = "")






# -------------------------------------------------#
# Diversity metrics for each EBSA

div <- c("nSp_Fish", "nSp_Invert","Div_Fish", "Div_Invert")
subdiv <- df[df$species %in% div,]

# clean table
subdiv <- subdiv[,!names(subdiv) == "variable"]
subdiv$SpeciesGroup <- sub(".*_","", subdiv$species)
subdiv$Variable <- sub("_.*","", subdiv$species)

subdiv <- subdiv[c("EBSA","SpeciesGroup","Variable","mean_in","mean_out","sd_in","sd_out",
                   "sum_in","sum_out","pPres_in","pPres_out","pNA_in","pNA_out")]

# ----------------------------------------------------#
# Export as csv
write.csv(subdiv, file= "Output/EBSA_Diversity_SummaryTable.csv", row.names=FALSE, na = "")





