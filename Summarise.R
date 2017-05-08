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
# Summarises survery and presence data metrics calculated in GroupbyEBSA.R
# * Only includes species listed as important for the EBSAs in
#   Clark & Jamieson 2006 Phase II
# * Exports 2 summary tables: species abundance/presence and diveristy/richness
#
###############################################################################

# Load packages
require(reshape2)
require(ggplot2)

# global options
options(scipen = 999)

# Go to parent directory
setwd('..')


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
casted <- dcast(EBSA + type + species ~  variable + Area, fun=mean, data=melted)
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
df$species[df$species == "Hippoglossus_stenolepis_phma"] <- "Halibut_phma"
df$species[df$species == "Clupea_pallasii_pallasii"] <- "Herring"
df$species[df$species == "Gadus_macrocephalus"] <- "PacificCod"
df$species[df$species == "Ophiodon_elongatus"] <- "Lingcod"
df$species[df$species == "Ophiodon_elongatus_phma"] <- "Lingcod_phma"
df$species[df$species == "Merluccius_productus"] <- "Hake"
df$species[df$species == "Anoplopoma_fimbria"] <- "Sablefish"
df$species[df$species == "Anoplopoma_fimbria_phma"] <- "Sablefish_phma"
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
df$species[df$species == "Rissa_tridactyla"] <- "Black_legged_Kittiwake"
df$species[df$species == "Phalacrocorax_penicillatus"] <- "Brandts_Cormorant"
df$species[df$species == "Ptychoramphus_aleuticus"] <- "Cassins_Auklet"
df$species[df$species == "Uria_aalge"] <- "Common_Murre"
df$species[df$species == "Larus_glaucescens"] <- "Glaucous_winged_Gull"
df$species[df$species == "Oceanodroma_leucorhoa"] <- "Leachs_Storm_petrels"
df$species[df$species == "Oceanodroma_furcata"] <- "Fork_tailed_Storm_petrels"
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
McIntyreBay <-  c("DungenessCrab","Halibut","Halibut_phma","Eulachon","Herring",
                  "KillerWhale","Humpback")
DogfishBank <-  c("DungenessCrab","PacificCod", "Shearwaters", "Phalaropes",
                  "Herring_Gulls","Ancient_Murrelet", "Scoters",
                  "SandSole","ButterSole","EnglishSole","RockSole")
LearmonthBank  <-  c("FinWhale","GreyWhale", "Alcids")
BrooksPeninsula <- c("SeaOtterRange","Lingcod","Lingcod_phma",
                     "GreenSturgeon","Phalaropes","Common_Murre",
                     "Tufted_Puffin","Sooty_Shearwaters","Glaucous_winged_Gull",
                     "Rhinoceros_Auklet","Black_legged_Kittiwake")
CapeStJames <- c("Halibut", "Halibut_phma","StellarSeaLionRookeries",
                 "Humpback","BlueWhale","FinWhale")
ShelfBreak <- c("Hake","Humpback","TannerCrab","SpermWhale", "BlueWhale","SeiWhale",
                "FinWhale","Eulachon","Sablefish","Sablefish_phma","DoverSole",
                "PacificOceanPerch","YellowtailRockfish","YellowmouthRockfish","GreyWhale",
                "Cassins_Auklet","Ancient_Murrelet","Rhinoceros_Auklet","Tufted_Puffin",
                "Fork_tailed_Storm_petrels", "Leachs_Storm_petrels")
ScottIslands <-  c("SeaOtterRange","GreyWhale","Humpback","StellarSeaLionRookeries",
                   "PacificCod","Lingcod","Lingcod_phma","Sablefish",
                   "Sablefish_phma","Hake","Herring","WidowRockfish",
                   "Cassins_Auklet","Rhinoceros_Auklet","Tufted_Puffin","Common_Murre",
                   "Brandts_Cormorant","Pelagic_Cormorant","Pigeon_Guillemot",
                   "Glaucous_winged_Gull","Fork_tailed_Storm_petrels",
                   "Leachs_Storm_petrels")
NorthIslandsStraits <- c("KillerWhale","GreyWhale", "Humpback","Herring","Shrimp", "Prawn",
                         "GreenUrchin","SeaOtterRange","Rhinoceros_Auklet",
                         "Fork_tailed_Storm_petrels", "Leachs_Storm_petrels")
SpongeReefs <-  c("SpongeReefs")
ChathamSound  <- c("GreenUrchin","DungenessCrab","Shrimp","HerringSpawn","KillerWhale",
                   "Humpback","Scoters")
CentralMainland  <- c("SeaOtterRange","StellarSeaLionRookeries","KillerWhale","FinWhale",
                      "Humpback","GreyWhale","RedSeaCucumber","BlackRockfish","ChinaRockfish",
                      "CopperRockfish","QuillbackRockfish","TigerRockfish","YelloweyeRockfish") # not in table: overlap with sea cuc IA, rockfish are inshore spp found in conservation priorities SAR
BellaBellaNearshore <- c("SeaOtterRange","Geoduck","RedUrchin","RedSeaCucumber",
                         "Shrimp","KillerWhale","HerringSpawn")
HaidaGwaiiNearshore <- c("FinWhale","Humpback","RedUrchin","RedSeaCucumber","HerringSpawn",
                         "PacificCod","Abalone","StellarSeaLionRookeries","Sooty_Shearwaters")


# species
species <- c("Abalone","Alcids","Ancient_Murrelet","Black_legged_Kittiwake","BlueWhale",
             "Brandts_Cormorant","ButterSole","Cassins_Auklet","Common_Murre","DoverSole",
             "DungenessCrab","EnglishSole","Eulachon","FinWhale","Fork_tailed_Storm_petrels",
             "Geoduck","Glaucous_winged_Gull","GreenSturgeon","GreenUrchin","GreyWhale","Hake", 
             "Halibut","Halibut_phma","Herring","Herring_Gulls","HerringSpawn","Humpback",
             "KillerWhale","Leachs_Storm_petrels","Lingcod","Lingcod_phma","PacificCod",
             "PacificOceanPerch","Pelagic_Cormorant","Phalaropes","Pigeon_Guillemot",
             "Prawn","RedSeaCucumber","RedUrchin","Rhinoceros_Auklet","RockSole","Sablefish",
             "Sablefish_phma","SandSole","Scoters","SeaOtterRange","SeiWhale","Shearwaters",
             "Shrimp","Sooty_Shearwaters","SpermWhale","SpongeReefs","StellarSeaLionRookeries",
             "TannerCrab","Tufted_Puffin","WidowRockfish","YellowmouthRockfish","YellowtailRockfish",
             "BlackRockfish","ChinaRockfish","CopperRockfish","QuillbackRockfish","TigerRockfish",
             "YelloweyeRockfish")

# ebsas
ebsas <- c("HecateStraitFront","BellaBellaNearshore","BrooksPeninsula","CapeStJames",
           "CentralMainland","ChathamSound","DogfishBank","HaidaGwaiiNearshore",
           "LearmonthBank","McIntyreBay","NorthIslandsStraits","ScottIslands",
           "ShelfBreak","SpongeReefs")

# which EBSAs are important for each species
spbyebsa <- list()
for(s in species){
  tmp <- NULL
  for(e in ebsas){
    ebsa <- get(e)
    if(s %in% ebsa) tmp <- c(tmp, e)
  }
  spbyebsa[[s]] <- tmp
}



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

areas <- c("BellaBellaNearshore","BrooksPeninsula","CapeStJames","CentralMainland","ChathamSound",
           "DogfishBank","HaidaGwaiiNearshore","LearmonthBank","McIntyreBay","NorthIslandsStraits",
           "ScottIslands","ShelfBreak","SpongeReefs")
subint <- NULL
for(a in areas){
  tmp <- df[df$species %in% spint[[a]] & df$EBSA == a,]
  subint <- rbind(subint, tmp)
}

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
# Export as csv
write.csv(subdiv, file= "Output/EBSA_Diversity_SummaryTable.csv", row.names=FALSE, na = "")
