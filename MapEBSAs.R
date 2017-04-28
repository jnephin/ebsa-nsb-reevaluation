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
library(rgeos)
library(classInt)
library(RColorBrewer)

# Go to parent directory
setwd('..')


# -------------------------------------------------#
# Load EBSA in and outside polygons
gdb <- "EBSA_Polygons/EBSAs_Overlay.gdb"
ebsas <- ogrListLayers(gdb)

# within only
ebsas <- ebsas[-grep("^nsb_",ebsas)]

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
load(file="Aggregated/Survey_Mean.Rdata") #survey_grid
smean <- survey_grid
load(file="Aggregated/Survey_SD.Rdata") #survey_grid
ssd <- survey_grid
load(file="Aggregated/Presence.Rdata") #presence_grid
pres <- presence_grid

# species/groups to map
species <- c("SeaOtterRange","Geoduck","RedUrchin","RedSeaCucumber","Shrimp",
             "KillerWhale","Herring","Lingcod","Halibut","Halibut_Longline",
             "StellarSeaLionRookeries","Humpback","BlueWhale","FinWhale",
             "GreyWhale","GreenUrchin", "Abalone","DungenessCrab","PacificCod",
             "Eulachon","Sablefish","Sablefish_Longline","Hake","WidowRockfish",
             "TannerCrab","SpermWhale","SpongeReefs","DoverSole", "HerringSpawn",
             "PacificOceanPerch","YellowtailRockfish","YellowmouthRockfish",
             "nSp_Fish", "nSp_Invert","Div_Fish", "Div_Invert",
             "DoverSole","SandSole","ButterSole","EnglishSole","RockSole",
             "GreenSturgeon","YelloweyeRockfish","CopperRockfish",
             "TigerRockfish","ChinaRockfish","QuillbackRockfish","BlackRockfish",
             "Black-legged_Kittiwake","Brandts_Cormorant","Cassins_Auklet",
             "Common_Murre","Glaucous-winged_Gull","Leachs_Storm-petrels",
             "Fork-tailed_Storm-petrels","Pelagic_Cormorant","Pigeon_Guillemot",
             "Rhinoceros_Auklet","Tufted_Puffin","Scoters","Alcids","Phalaropes")

# Reclass species names to common names
for (d in c("smean", "ssd", "pres")){
  df <- get(d)
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
  assign(d, df)
}


# -------------------------------------------------#



# -------------------------------------------------#
# Map function
MapLayers <- function( griddata, layers, type, style="kmeans"){

  # loop through layers
  layers <- layers[layers %in% names(griddata)]
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
      brksint <- classIntervals( Layer@data[,1], n=6, style=style,
                                 intervalClosure="right" )
      # Cut data using breaks
      brks <- cut( Layer@data[,1], breaks = brksint$brks, include.lowest = TRUE )
      # break labels
      labels <- sub( ".*,","", brks )
      labels <- as.numeric( sub( "]","", labels ) )
      # set zero label
      labels[Layer@data[,1] == 0] <- 0
      # get colours
      colours <- rev(brewer.pal( length(na.omit(unique(labels))), "Spectral" ))
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
    lims <- list( x=c(ext@xmin, ext@xmax), y=c(ext@ymin, ext@ymax) )

    # Map
    pdf( file=file.path("Output/Maps",  paste0(p,"_", type, ".pdf")),
         height=6, width=5.25*diff(lims$x)/diff(lims$y)+1 )
    par(mar=c(1,1,1,1))
    plot( bcPoly, col = "grey40", border = NA, xlim = lims$x*.98 , ylim = lims$y*.98 )
    box( lty = 'solid', col = 'black')
    plot( Layer, col=Layer$colours, border=NA, add = T )
    # if(type != "presence") plot( nsb, add=T, border= "brown" )
    plot( spdf, add=T )
    legend( "bottomleft", legend = pal$labels, fill = pal$colours,
            title = p, bg = NA, box.col = NA)
    dev.off()

  } # end loop through layers
} # End MapLayers function
# -------------------------------------------------#


# Map
MapLayers( griddata = smean, layers = species, type = "survey")
MapLayers( griddata = ssd, layers = species, type = "survey")
MapLayers( griddata = pres, layers = species, type = "presence")
