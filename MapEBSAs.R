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
# Exports maps of species abundance/presence and richness/diveristy data
#   aggregate by grid
#
###############################################################################

# Load packages
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(classInt)
library(RColorBrewer)

# Go to parent directory
setwd('..')


# -------------------------------------------------#
# Load EBSA in and outside polygons
gdb <- "EBSA_Polygons/EBSAs.gdb"
ebsas <- ogrListLayers(gdb)

# loop through each ebsa polygon
# merge into one spatial polygon data frame
spdf <- readOGR(dsn=gdb, layer=ebsas[1])
for(i in ebsas[2:length(ebsas)]){
  #load polygons
  poly <- readOGR(dsn=gdb, layer=i)
  spdf <- rbind(spdf,poly)
}

# Convert to spatial polygons (i.e., drop the data)
spdf <- as( spdf, "SpatialPolygons" )
# -------------------------------------------------#



# -------------------------------------------------#
# load boundary polygon
nsb <- readOGR(dsn="Boundary", layer="NSB")
# Convert to spatial polygons (i.e., drop the data)
nsb <- as( nsb, "SpatialPolygons" )

# Load coastline shapefile
bcPoly <- readOGR( dsn="Boundary", layer="coast_sim" )
# Convert to spatial polygons (i.e., drop the data)
bcPoly <- as( bcPoly, "SpatialPolygons" )
# Project
bcPoly <- spTransform( bcPoly, proj4string(nsb))

# -------------------------------------------------#




# -------------------------------------------------#
# Load gridded data
load(file="Aggregated/Grid_Survey_Mean.Rdata") #smean
load(file="Aggregated/Grid_Presence.Rdata") #pres

# species/groups to map
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

# Reclass species names to common names
for (d in c("smean", "pres")){
  df <- get(d)
  names(df)[names(df) == "Hippoglossus_stenolepis"] <- "Halibut"
  names(df)[names(df) == "Hippoglossus_stenolepis_phma"] <- "Halibut (PHMA)"
  names(df)[names(df) == "Clupea_pallasii_pallasii"] <- "Herring"
  names(df)[names(df) == "Gadus_macrocephalus"] <- "Pacific Cod"
  names(df)[names(df) == "Ophiodon_elongatus"] <- "Lingcod"
  names(df)[names(df) == "Ophiodon_elongatus_phma"] <- "Lingcod (PHMA)"
  names(df)[names(df) == "Merluccius_productus"] <- "Pacific Hake"
  names(df)[names(df) == "Anoplopoma_fimbria"] <- "Sablefish"
  names(df)[names(df) == "Anoplopoma_fimbria_phma"] <- "Sablefish (PHMA)"
  names(df)[names(df) == "Sebastes_alutus"] <- "Pacific Ocean Perch"
  names(df)[names(df) == "Sebastes_flavidus"] <- "Yellowtail Rockfish"
  names(df)[names(df) == "Sebastes_reedi"] <- "Yellowmouth Rockfish"
  names(df)[names(df) == "Sebastes_saxicola"] <- "Widow Rockfish"
  names(df)[names(df) == "Thaleichthys_pacificus"] <- "Eulachon"
  names(df)[names(df) == "Microstomus_pacificus"] <- "Dover Sole"
  names(df)[names(df) == "Psettichthys_melanostictus"] <- "Sand Sole"
  names(df)[names(df) == "Isopsetta_isolepis"] <- "Butter Sole"
  names(df)[names(df) == "Parophrys_vetulus"] <- "English Sole"
  names(df)[names(df) == "Lepidopsetta_bilineata"] <- "Rock Sole"
  names(df)[names(df) == "Acipenser_medirostris"] <- "Green Sturgeon"
  names(df)[names(df) == "Sebastes_ruberrimus"] <- "Yelloweye Rockfish"
  names(df)[names(df) == "Sebastes_caurinus"] <- "Copper Rockfish"
  names(df)[names(df) == "Sebastes_nigrocinctus"] <- "Tiger Rockfish"
  names(df)[names(df) == "Sebastes_nebulosus"] <- "China Rockfish"
  names(df)[names(df) == "Sebastes_maliger"] <- "Quillback Rockfish"
  names(df)[names(df) == "Sebastes_melanops"] <- "Black Rockfish"
  names(df)[names(df) == "Rissa_tridactyla"] <- "Black-legged Kittiwake"
  names(df)[names(df) == "Phalacrocorax_penicillatus"] <- "Brandt's Cormorant"
  names(df)[names(df) == "Ptychoramphus_aleuticus"] <- "Cassin's Auklet"
  names(df)[names(df) == "Uria_aalge"] <- "Common Murre"
  names(df)[names(df) == "Larus_glaucescens"] <- "Glaucous-winged Gull"
  names(df)[names(df) == "Oceanodroma_leucorhoa"] <- "Leach's Storm Petrels"
  names(df)[names(df) == "Oceanodroma_furcata"] <- "Fork Tailed Storm Petrels"
  names(df)[names(df) == "Phalacrocorax_pelagicus"] <- "Pelagic Cormorant"
  names(df)[names(df) == "Cepphus_columba"] <- "Pigeon Guillemot"
  names(df)[names(df) == "Synthliboramphus_antiquus"] <-"Ancient Murrelet"
  names(df)[names(df) == "Cerorhinca_monocerata"] <- "Rhinoceros Auklet"
  names(df)[names(df) == "Fratercula_cirrhata"] <- "Tufted Puffin"
  names(df)[names(df) == "Melanitta"] <- "Scoters"
  names(df)[names(df) == "Alcidae"] <- "Alcids"
  names(df)[names(df) == "Phalaropus"] <- "Phalaropes"
  names(df)[names(df) == "Div_Fish"] <- "Fish Diversity"
  names(df)[names(df) == "Div_Invert"] <- "Invert Diversity"
  names(df)[names(df) == "nSp_Fish"] <- "Fish Richness"
  names(df)[names(df) == "nSp_Invert"] <- "Invert Richness"
  assign(d, df)
}


# -------------------------------------------------#



# -------------------------------------------------#
# Map function
MapLayers <- function( griddata, layers, dir, type, style="kmeans"){

  # loop through layers
  layers <- unique(names(griddata))
  for(p in layers){

    # Keep only attribute to plot
    Layer <- griddata[p]

    # Set colour scale
    if( type == "presence" ){ #If factor
      # colour
      colours <- c("#3288BD","#D53E4F")
      # combine colours and unique factor levels
      brks <- na.omit(sort(unique( Layer@data[,1] )))
      pal <- data.frame( brks, colours, stringsAsFactors = FALSE )
      colnames(pal) <- c( p, "colours" )
      # break labels
      pal$labels <- as.character( brks )
      # merge colours with layer data
      Layer <- sp::merge( Layer, pal )
    } else { # If continuous
      # get breaks
      dat <- Layer@data[,1]
      dat <- dat[!is.na(dat)]
      if( length(unique(dat)) > 6 ){
        brksint <- classIntervals( dat, n=6, style=style, intervalClosure="right" )
      } else {
        brksint <- NULL
        brksint$brks <- unique(dat)
      }
      # Cut data using breaks
      brks <- cut( Layer@data[,1], breaks = brksint$brks, include.lowest = TRUE )
      # break labels
      labels <- sub( ".*,","", brks )
      labels <- as.numeric( sub( "]","", labels ) )
      # set zero label
      labels[Layer@data[,1] == 0] <- 0
      # get colours
      if ( length(na.omit(unique(labels))) >= 3 ){
        colours <- rev(brewer.pal( length(na.omit(unique(labels))), "Spectral" ))
      } else{
        colours <-  c("#3288BD","#D53E4F")
      }
      # combine colours and breaks
      pal <- data.frame( labels = na.omit(sort(unique( labels ))), colours = colours,
                         stringsAsFactors = FALSE )
      # merge colours with layer data
      Layer@data$labels <- labels
      Layer <- sp::merge( Layer, pal, by = "labels")
    }

    # Get the vertical and horizontal limits
    ext <- extent( Layer )
    # Get x and y limits
    lims <- list( x=c(ext@xmin, ext@xmax), y=c(ext@ymin, ext@ymax*.98) )

    # Map
    tiff(file=file.path("Output/Maps", dir, paste0(p, ".tif")),
         width = 5 , height = 5.25, units = "in", res = 300,
         compression = "lzw")
    par(mar=c(1,1,1,1))
    plot( Layer, col=Layer$colours, border=NA, xlim = lims$x , ylim = lims$y )
    box( lty = 'solid', col = 'black')
    plot( bcPoly, col = "grey40", border = NA, add = T )
    plot( spdf, add=T )
    legend( "bottomleft", legend = pal$labels, fill = pal$colours,
            title = p, bg = NA, box.col = NA)
    dev.off()

  } # end loop through layers
} # End MapLayers function
# -------------------------------------------------#


# Map
MapLayers( griddata = smean, layers = species, dir = "Survey", type = "survey")
MapLayers( griddata = pres, layers = species, dir = "Presence", type = "presence")
