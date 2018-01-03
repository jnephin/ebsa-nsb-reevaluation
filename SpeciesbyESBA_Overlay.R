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
#  [1] Groups species into EBSAs for which they were listed as important.
#  [2] Performs spatial overlay for each species. Adds an inside or outside
#      attribute for each cell for each EBSA.
#
###############################################################################


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

# Convert to points for overlay
# that way the grid cell will be marked as inside only if the centroid is within the ebsa
proj <- proj4string(dens)
ptdens <- gCentroid(dens, byid=TRUE)
dens <- SpatialPointsDataFrame(coordinates(ptdens), dens@data, proj4string = CRS(proj))
ptpres <- gCentroid(pres, byid=TRUE)
pres <- SpatialPointsDataFrame(coordinates(ptpres), pres@data, proj4string = CRS(proj))

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
# proj
bcalb <- "+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
spdf <- spTransform(spdf, bcalb)


# ebsas
ebsas <- c("HecateStraitFront","BellaBellaNearshore","BrooksPeninsula","CapeStJames",
           "CentralMainland","ChathamSound","DogfishBank","HaidaGwaiiNearshore",
           "LearmonthBank","McIntyreBay","NorthIslandsStraits","ScottIslands",
           "ShelfBreak","SpongeReefs")

#---------------------------------------------------------#
# Species important to each EBSA

HecateStraitFront <- NULL
McIntyreBay <-  c("Dungeness_Crab","Halibut","Herring",
                  "Killer_Whale","Humpback_Whale") #"Eulachon", "Scoters"
DogfishBank <-  c("Dungeness_Crab") # "GlaucousWingedGull" no colonies within DB
                 #"AncientMurrelet","Scoters", "Shearwaters", "Red_necked_Phalarope"
LearmonthBank  <-  c("Fin_Whale","Gray_Whale","Coral")# "Ancient_Murrelet", "CommonMurre","PigeonGuillemot", "RhinocerosAuklet" no colonies within learmonth bank
BrooksPeninsula <- c("SeaOtterRange","Lingcod_line","TuftedPuffin",
                     "GlaucousWingedGull","Tanner_Crab") 
                    #"Shearwaters","RhinocerosAuklet","CommonMurre",
CapeStJames <- c("Halibut", "Halibut_line","StellarSeaLionRookeries",
                 "Humpback_Whale","Blue_Whale","Fin_Whale","Sponge", "Coral", "Seapen")#"Shearwaters"
ShelfBreak <- c("Pacific_hake","Humpback_Whale","Sperm_Whale", "Blue_Whale",
                "Fin_Whale","Sablefish","Dover_sole", "Tanner_Crab",
                "Pacific_Ocean_perch","Yellowtail_rockfish","Yellowmouth_rockfish",
                "Gray_Whale","CassinsAuklet","RhinocerosAuklet","StormPetrels","TuftedPuffin",
                "Coral", "Seapen", "Sponge") #"Eulachon"
                # lonline rockfish data does not have enough coverage on the shelf to meet 20% rule
ScottIslands <-  c("SeaOtterRange","Gray_Whale","Humpback_Whale","StellarSeaLionRookeries",
                   "Pacific_cod","Lingcod","Lingcod_line","Sablefish",
                   "Sablefish_line","Pacific_hake","Herring","Widow_rockfish",
                   "CassinsAuklet","RhinocerosAuklet","TuftedPuffin","CommonMurre",
                   "GlaucousWingedGull","StormPetrels","PigeonGuillemot","Cormorants",
                   "Arrowtooth_flounder", "Petrale_sole", "Butter_sole", 
                   "Rock_sole", "Dover_sole", "English_sole") 
                  #"Black_footed_Albatross", "Northern_Fulmar","Shearwaters"
NorthIslandsStraits <- c("Killer_Whale","Gray_Whale", "Humpback_Whale",
                         "Shrimp","Prawn","Green_Urchin","SeaOtterRange","StormPetrels",
                         "RhinocerosAuklet") 
SpongeReefs <-  c("SpongeReef")
ChathamSound  <- c("Green_Urchin","Dungeness_Crab","Shrimp","Herring_spawn","Killer_Whale",
                   "Humpback_Whale") #"Scoters"
HaidaGwaiiNearshore <- c("Fin_Whale","Humpback_Whale","Gray_Whale","Red_Urchin",
                         "Red_Sea_Cucumber","Abalone", "Herring_spawn",
                         "Pacific_cod","StellarSeaLionRookeries","Arrowtooth_flounder",
                         "Petrale_sole", "Butter_sole", "Rock_sole", "Dover_sole", 
                         "English_sole") #"Shearwaters",
CentralMainland  <- c("SeaOtterRange","StellarSeaLionRookeries","Killer_Whale","Fin_Whale",
                      "Humpback_Whale","Gray_Whale","Red_Sea_Cucumber",
                      "Sablefish_line") #"Shearwaters"
BellaBellaNearshore <- c("SeaOtterRange","Geoduck","Red_Urchin","Red_Sea_Cucumber",
                         "Shrimp","Killer_Whale","Herring_spawn")

# all important species
species <- sort(unique(c(HecateStraitFront,BellaBellaNearshore,BrooksPeninsula,
                         CapeStJames,CentralMainland,ChathamSound,DogfishBank,
                         HaidaGwaiiNearshore,LearmonthBank,McIntyreBay,
                         NorthIslandsStraits,ScottIslands,ShelfBreak,SpongeReefs)))

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
write.csv(dat,file="Output/Tables/Species_Summary.csv")


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

# save species by ebsa
save(spbyebsa, file="Aggregated/SpeciesByEBSAs.Rdata")


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
    # ebsas
    for( e in ebsas ){
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
    # ebsas 
    for( e in ebsas ){
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

