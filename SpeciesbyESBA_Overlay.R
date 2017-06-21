#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview:
#  1) Groups species into EBSAs for which they were listed as important
#  2) Performs spatial overlay for each species. Adds an inside or outside
#     attribute for each cell for each of the relavant EBSAs.
#
###############################################################################


###-- to do: use centroids of the grids rather than the polygon grids themselves in overlay. That way a grid cell mostly outside of the EBSA won't be counted as inside.

# Load packages
library(rgdal)
library(raster)
library(rgeos)

# Go to parent directory
setwd('..')


# -------------------------------------------------#
# Load gridded data
load(file="Aggregated/Grid_DensityData.Rdata") #dens
load(file="Aggregated/Grid_PresenceData.Rdata") #pres




# Species important to each EBSA

HecateStraitFront <- NULL
McIntyreBay <-  c("DungenessCrab","Halibut","Halibut_line","Eulachon","Herring",
                  "Killer_Whale","Humpback_Whale")
DogfishBank <-  c("DungenessCrab", "Shearwaters", "Red_necked_Phalarope",
                  "Large_Gulls","Ancient_Murrelet")
#, "Scoters","Pacific_cod","Sand_sole","Butter_sole","English_sole","Rock_sole") # flatfish IA, no data for these species on dogfish bank 
LearmonthBank  <-  c("Fin_Whale","Gray_Whale","Common_Murre","Pigeon_Guillemot",
                     "Marbled_Murrelet","Rhinoceros_Auklet","Ancient_Murrelet") # included all auk species b/c paper just said alcids
BrooksPeninsula <- c("SeaOtterRange","Lingcod","Lingcod_line",
                     "Green_sturgeon","Red_necked_Phalarope","Common_Murre",
                     "Tufted_Puffin","Shearwaters","Large_Gulls",
                     "Rhinoceros_Auklet")
CapeStJames <- c("Halibut", "Halibut_line","StellarSeaLionRookeries",
                 "Humpback_Whale","Blue_Whale","Fin_Whale")
ShelfBreak <- c("Pacific_hake","Humpback_Whale","TannerCrab","Sperm_Whale", "Blue_Whale",
                "Fin_Whale","Eulachon","Sablefish","Sablefish_line","Dover_sole",
                "Pacific_Ocean_perch","Yellowtail_rockfish","Yellowmouth_rockfish","Gray_Whale",
                "Cassins_Auklet","Ancient_Murrelet","Rhinoceros_Auklet","Tufted_Puffin",
                "Fork_tailed_Storm_petrel", "Leachs_Storm_petrel","Shearwaters")
ScottIslands <-  c("SeaOtterRange","Gray_Whale","Humpback_Whale","StellarSeaLionRookeries",
                   "Pacific_cod","Lingcod","Lingcod_line","Sablefish",
                   "Sablefish_line","Pacific_hake","Herring","Widow_rockfish",
                   "Cassins_Auklet","Rhinoceros_Auklet","Tufted_Puffin","Common_Murre",
                   "Cormorants","Pigeon_Guillemot","Large_Gulls","Fork_tailed_Storm_petrel",
                   "Leachs_Storm_petrel","Black_footed_Albatross", "Northern_Fulmar",
                   "Sand_sole","Butter_sole","English_sole","Rock_sole")
NorthIslandsStraits <- c("Killer_Whale","Gray_Whale", "Humpback_Whale","Herring",
                         "Shrimp","Prawn","GreenUrchin","SeaOtterRange","Rhinoceros_Auklet",
                         "Fork_tailed_Storm_petrel", "Leachs_Storm_petrel")
SpongeReefs <-  c("SpongeReef")
ChathamSound  <- c("GreenUrchin","DungenessCrab","Shrimp","Herring_spawn","Killer_Whale",
                   "Humpback_Whale","Scoters","Walleye_pollock")
CentralMainland  <- c("SeaOtterRange","StellarSeaLionRookeries","Killer_Whale","Fin_Whale",
                      "Humpback_Whale","Gray_Whale","RedSeaCucumber","Black_rockfish",
                      "China_rockfish","Copper_rockfish","Quillback_rockfish",
                      "Tiger_rockfish","Yelloweye_rockfish","Yelloweye_line" ) # not in table: overlap with sea cuc IA, rockfish are inshore spp found in conservation priorities SAR
BellaBellaNearshore <- c("SeaOtterRange","Geoduck","RedUrchin","RedSeaCucumber",
                         "Shrimp","Killer_Whale","Herring_spawn")
HaidaGwaiiNearshore <- c("Fin_Whale","Humpback_Whale","RedUrchin","RedSeaCucumber",
                         "Herring_spawn","Pacific_cod","Abalone","StellarSeaLionRookeries",
                         "Shearwaters")


# all important species
species <- sort(unique(c(HecateStraitFront,BellaBellaNearshore,BrooksPeninsula,
                         CapeStJames,CentralMainland,ChathamSound,DogfishBank,
                         HaidaGwaiiNearshore,LearmonthBank,McIntyreBay,
                         NorthIslandsStraits,ScottIslands,ShelfBreak,SpongeReefs)))
save(species, file="Scripts/Species")

# ebsas
ebsas <- c("HecateStraitFront","BellaBellaNearshore","BrooksPeninsula","CapeStJames",
           "CentralMainland","ChathamSound","DogfishBank","HaidaGwaiiNearshore",
           "LearmonthBank","McIntyreBay","NorthIslandsStraits","ScottIslands",
           "ShelfBreak","SpongeReefs")

# Output table summary
dat <- data.frame(ebsas = ebsas, 
                  species=c(paste(HecateStraitFront, collapse = " "),
                            paste(BellaBellaNearshore, collapse = " "),
                            paste(BrooksPeninsula, collapse = " "),
                            paste(CapeStJames, collapse = " "),
                            paste(CentralMainland, collapse = " "),
                            paste(ChathamSound, collapse = " "),
                            paste(DogfishBank, collapse = " "),
                            paste(HaidaGwaiiNearshore, collapse = " "),
                            paste(LearmonthBank, collapse = " "),
                            paste(McIntyreBay, collapse = " "),
                            paste(NorthIslandsStraits, collapse = " "),
                            paste(ScottIslands, collapse = " "),
                            paste(ShelfBreak, collapse = " "),
                            paste(SpongeReefs, collapse = " ")))
write.csv(dat,file="Output/Tables/EBSA_Summary.csv")


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
densEBSA <- list()
# loop through each species in survey data
for(s in species){
  # is the species present in the data
  if( s %in% names(dens) ){
    # get gridded data for s only
    sp <- dens[s]
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
    densEBSA[[s]] <- sp
  }
}

# Save
save(densEBSA, file="Aggregated/EBSA_Density_Overlay.Rdata")




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

