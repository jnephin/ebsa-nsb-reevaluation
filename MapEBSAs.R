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
             "YelloweyeRockfish","Div_Fish","Div_Invert","nSp_Fish","nSp_Invert")

# Reclass species names to common names
for (d in c("smean", "ssd", "pres")){
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


# -------------------------------------------------#



# -------------------------------------------------#
# Map function
MapLayers <- function( griddata, layers, dir, type, style="kmeans"){

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
         width = 5 , height = 5.25, units = "in", res = 800,
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
MapLayers( griddata = smean, layers = species, dir = "Abundance/Mean", type = "survey")
MapLayers( griddata = ssd, layers = species[!species == "GreenSturgeon"], 
           dir = "Abundance/SD", type = "survey")
MapLayers( griddata = pres, layers = species, dir = "Presence", type = "presence")
