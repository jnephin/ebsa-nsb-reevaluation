#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview:
# 
#
###############################################################################


# Load packages
library(rgdal)
library(raster)
library(rgeos)


# -------------------------------------------------#
# Load gridded data
load(file="Aggregated/Survey_Mean.Rdata") #smean
load(file="Aggregated/Presence.Rdata") #pres



# -------------------------------------------------#
# Reclass species names to common names
for (d in c("smean", "pres")){
  df <- get(d)
  names(df)[names(df) == "Hippoglossus_stenolepis"] <- "Halibut"
  names(df)[names(df) == "Hippoglossus_stenolepis_phma"] <- "Halibut_phma"
  names(df)[names(df) == "Clupea_pallasii_pallasii"] <- "Herring"
  names(df)[names(df) == "Gadus_macrocephalus"] <- "PacificCod"
  names(df)[names(df) == "Ophiodon_elongatus"] <- "Lingcod"
  names(df)[names(df) == "Ophiodon_elongatus_phma"] <- "Lingcod_phma"
  names(df)[names(df) == "Merluccius_productus"] <- "Hake"
  names(df)[names(df) == "Anoplopoma_fimbria"] <- "Sablefish"
  names(df)[names(df) == "Anoplopoma_fimbria_phma"] <- "Sablefish_phma"
  names(df)[names(df) == "Sebastes_alutus"] <- "PacificOceanPerch"
  names(df)[names(df) == "Sebastes_flavidus"] <- "YellowtailRockfish"
  names(df)[names(df) == "Sebastes_reedi"] <- "YellowmouthRockfish"
  names(df)[names(df) == "Sebastes_saxicola"] <- "WidowRockfish"
  names(df)[names(df) == "Thaleichthys_pacificus"] <- "Eulachon"
  names(df)[names(df) == "Microstomus_pacificus"] <- "DoverSole"
  names(df)[names(df) == "Psettichthys_melanostictus"] <- "SandSole"
  names(df)[names(df) == "Isopsetta_isolepis"] <- "ButterSole"
  names(df)[names(df) == "Parophrys_vetulus"] <- "EnglishSole"
  names(df)[names(df) == "Lepidopsetta_bilineata"] <- "RockSole"
  names(df)[names(df) == "Acipenser_medirostris"] <- "GreenSturgeon"
  names(df)[names(df) == "Sebastes_ruberrimus"] <- "YelloweyeRockfish"
  names(df)[names(df) == "Sebastes_caurinus"] <- "CopperRockfish"
  names(df)[names(df) == "Sebastes_nigrocinctus"] <- "TigerRockfish"
  names(df)[names(df) == "Sebastes_nebulosus"] <- "ChinaRockfish"
  names(df)[names(df) == "Sebastes_maliger"] <- "QuillbackRockfish"
  names(df)[names(df) == "Sebastes_melanops"] <- "BlackRockfish"
  names(df)[names(df) == "Rissa_tridactyla"] <- "Black_legged_Kittiwake"
  names(df)[names(df) == "Phalacrocorax_penicillatus"] <- "Brandts_Cormorant"
  names(df)[names(df) == "Ptychoramphus_aleuticus"] <- "Cassins_Auklet"
  names(df)[names(df) == "Uria_aalge"] <- "Common_Murre"
  names(df)[names(df) == "Larus_glaucescens"] <- "Glaucous_winged_Gull"
  names(df)[names(df) == "Oceanodroma_leucorhoa"] <- "Leachs_Storm_petrels"
  names(df)[names(df) == "Oceanodroma_furcata"] <- "Fork_tailed_Storm_petrels"
  names(df)[names(df) == "Phalacrocorax_pelagicus"] <- "Pelagic_Cormorant"
  names(df)[names(df) == "Cepphus_columba"] <- "Pigeon_Guillemot"
  names(df)[names(df) == "Cerorhinca_monocerata"] <- "Rhinoceros_Auklet"
  names(df)[names(df) == "Fratercula_cirrhata"] <- "Tufted_Puffin"
  names(df)[names(df) == "Melanitta"] <- "Scoters"
  names(df)[names(df) == "Alcidae"] <- "Alcids"
  names(df)[names(df) == "Phalaropus"] <- "Phalaropes"
  assign(d, df)
}


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
SpongeReefs <-  c("SpongeReef")
ChathamSound  <- c("GreenUrchin","DungenessCrab","Shrimp","HerringSpawn","KillerWhale",
                   "Humpback","Scoters")
CentralMainland  <- c("SeaOtterRange","StellarSeaLionRookeries","KillerWhale","FinWhale",
                      "Humpback","GreyWhale","RedSeaCucumber","BlackRockfish","ChinaRockfish",
                      "CopperRockfish","QuillbackRockfish","TigerRockfish","YelloweyeRockfish") # not in table: overlap with sea cuc IA, rockfish are inshore spp found in conservation priorities SAR
BellaBellaNearshore <- c("SeaOtterRange","Geoduck","RedUrchin","RedSeaCucumber",
                         "Shrimp","KillerWhale","HerringSpawn")
HaidaGwaiiNearshore <- c("FinWhale","Humpback","RedUrchin","RedSeaCucumber","HerringSpawn",
                         "PacificCod","Abalone","StellarSeaLionRookeries","Sooty_Shearwaters")


# all important species
species <- c("Abalone","Alcids","Ancient_Murrelet","Black_legged_Kittiwake","BlueWhale",
             "Brandts_Cormorant","ButterSole","Cassins_Auklet","Common_Murre","DoverSole",
             "DungenessCrab","EnglishSole","Eulachon","FinWhale","Fork_tailed_Storm_petrels",
             "Geoduck","Glaucous_winged_Gull","GreenSturgeon","GreenUrchin","GreyWhale","Hake", 
             "Halibut","Halibut_phma","Herring","Herring_Gulls","HerringSpawn","Humpback",
             "KillerWhale","Leachs_Storm_petrels","Lingcod","Lingcod_phma","PacificCod",
             "PacificOceanPerch","Pelagic_Cormorant","Phalaropes","Pigeon_Guillemot",
             "Prawn","RedSeaCucumber","RedUrchin","Rhinoceros_Auklet","RockSole","Sablefish",
             "Sablefish_phma","SandSole","Scoters","SeaOtterRange","SeiWhale","Shearwaters",
             "Shrimp","Sooty_Shearwaters","SpermWhale","SpongeReef","StellarSeaLionRookeries",
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


# -------------------------------------------------#
# List EBSA polygons
gdb <- "EBSA_Polygons/EBSAs.gdb"
ebsalist <- ogrListLayers(gdb)

# loop through each ebsa polygon
# merge into one spatial polygon data frame
spdf <- readOGR(dsn=gdb, layer=ebsalist[1])
spdf <- as( spdf, "SpatialPolygons" )
spdf$EBSA <- rep(ebsalist[1],length(spdf))
for(i in ebsalist[2:length(ebsalist)]){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  poly <- as( poly, "SpatialPolygons" )
  poly$EBSA <- rep(i,length(poly))
  spdf <- rbind(spdf,poly)
}

# Remove extra attributes
spdf <- spdf[,names(spdf) %in% "EBSA"]



# -------------------------------------------------#
# Overlay

# empty list for loop
meanEBSA <- list()
# loop through each species in survey data
for(s in species){
  # is the species present in the data
  if( s %in% names(smean) ){
    # get gridded data for s only
    sp <- smean[s]
    # empty list
    overlay <- list()
    # ebsas where species was listed as important
    for( e in spbyebsa[[s]] ){
      # get single ebsa 
      ebsa <- spdf[spdf$EBSA %in% e,]
      # overlay grid with relevant ebsa polygon(s)
      tmp <- over(sp, ebsa)
      tmp$ID <- 1
      tmp$ID[is.na(tmp$EBSA)] <- 0
      overlay[[e]] <- tmp[,2]
    }
    # convert list to dataframe
    df <- as.data.frame(overlay)
    # add overlay to species data
    sp@data <- cbind(sp@data, df)
    # return
    meanEBSA[[s]] <- sp
  }
}

# Save
save(meanEBSA, file="Aggregated/EBSA_Mean_Overlay.Rdata")




# empty list for loop
presEBSA <- list()
# loop through each species in presence data
for(s in species){
  # is the species present in the data
  if( s %in% names(pres) ){
    # get gridded data for s only
    sp <- pres[s]
    # empty list
    overlay <- list()
    # ebsas where species was listed as important
    for( e in spbyebsa[[s]] ){
      # get single ebsa 
      ebsa <- spdf[spdf$EBSA %in% e,]
      # overlay grid with relevant ebsa polygon(s)
      tmp <- over(sp, ebsa)
      tmp$ID <- 1
      tmp$ID[is.na(tmp$EBSA)] <- 0
      overlay[[e]] <- tmp[,2]
    }
    # convert list to dataframe
    df <- as.data.frame(overlay)
    # add overlay to species data
    sp@data <- cbind(sp@data, df)
    # return
    presEBSA[[s]] <- sp
  }
}

# Save
save(presEBSA, file="Aggregated/EBSA_Presence_Overlay.Rdata")

