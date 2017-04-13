# Load packages
require(reshape2)
require(ggplot2)


options(scipen = 999)


# load data
load(file="Aggregated/EBSA_Aggregation.Rdata") #dat


# Convert list to data.frame
df <- melt(dat)

# add in/out tag
df$Area <- "in"
df$Area[grep("^nsb_",df$L1)] <- "out"

# EBSA column
df$EBSA <- sub("^nsb_","", df$L1)

# Remove L1 column
df <- df[!names(df) == "L1"]

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

# clean table
subint$value <- round(subint$value, 2)
subint <- subint[,!names(subint) == "variable"]

# save
save(subint,file= "Output/EBSA_MetledTable.RData")

# cast
casted <- dcast(EBSA + species + type  ~ metric + Area, fun=mean, data=subint)
casted <- casted[c("EBSA","species","type","mean_in","mean_out","sd_in","sd_out",
                   "sum_in","sum_out","pPres_in","pPres_out","pNA_in","pNA_out")]

# percent
casted$pPres_in <- casted$pPres_in * 100
casted$pPres_out <- casted$pPres_out * 100
casted$pNA_in <- casted$pNA_in * 100
casted$pNA_out <- casted$pNA_out * 100


# ----------------------------------------------------#
# Export as csv
write.csv(casted, file= "Output/EBSA_SummaryTable.csv", row.names=FALSE, na = "")






# -------------------------------------------------#
# Diversity metrics for each EBSA

div <- c("nSp_Fish", "nSp_Invert","Div_Fish", "Div_Invert")

subdiv <- NULL
for(a in areas){
  tmp <- df[df$species %in% div,]
  subdiv <- rbind(subdiv, tmp)
}

# clean table
subdiv$value <- round(subdiv$value, 2)
subdiv <- subdiv[,!names(subdiv) == "variable"]
subdiv$SpeciesGroup <- sub(".*_","", subdiv$species)
subdiv$Variable <- sub("_.*","", subdiv$species)

# save
save(subdiv,file= "Output/EBSA_Diversity_MeltedTable.RData")

# cast
casteddiv <- dcast(EBSA + SpeciesGroup + Variable + type  ~ metric + Area, fun=mean, data=subdiv)
casteddiv <- casteddiv[c("EBSA","SpeciesGroup", "Variable", "type",
                         "mean_in","mean_out","sd_in","sd_out",
                         "pPres_in","pPres_out","pNA_in","pNA_out")]

# percent
casteddiv$pPres_in <- casteddiv$pPres_in * 100
casteddiv$pPres_out <- casteddiv$pPres_out * 100
casteddiv$pNA_in <- casteddiv$pNA_in * 100
casteddiv$pNA_out <- casteddiv$pNA_out * 100


# ----------------------------------------------------#
# Export as csv
write.csv(casteddiv, file= "Output/EBSA_Diversity_SummaryTable.csv", row.names=FALSE, na = "")





