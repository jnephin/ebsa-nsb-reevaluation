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
load(file="Aggregated/Mean.Rdata") #survey_grid
load(file="Aggregated/Presence.Rdata") #presence_grid

# species/groups to map
species <- c("SeaOtterRange","Geoduck","RedUrchin","RedSeaCucumber","Shrimp","KillerWhale",
             "Herring","Lingcod","Halibut","Halibut_Longline","StellarSeaLionRookeries",
             "Humpback","BlueWhale","FinWhale","GreyWhale","GreenUrchin", "Abalone",
             "DungenessCrab","PacificCod","Eulachon","Sablefish","Sablefish_Longline",
             "Hake","WidowRockfish","TannerCrab","SpermWhale","SpongeReefs","DoverSole",
             "PacificOceanPerch","YellowtailRockfish","YellowmouthRockfish",
             "nSp_Fish", "nSp_Invert","Div_Fish", "Div_Invert")


# Reclass species names to common names
names(survey_grid)[names(survey_grid) == "Hippoglossus_stenolepis"] <- "Halibut"
names(survey_grid)[names(survey_grid) == "Clupea_pallasii_pallasii"] <- "Herring"
names(survey_grid)[names(survey_grid) == "Gadus_macrocephalus"] <- "PacificCod"
names(survey_grid)[names(survey_grid) == "Ophiodon_elongatus"] <- "Lingcod"
names(survey_grid)[names(survey_grid) == "Merluccius_productus"] <- "Hake"
names(survey_grid)[names(survey_grid) == "Anoplopoma_fimbria"] <- "Sablefish"
names(survey_grid)[names(survey_grid) == "Microstomus_pacificus"] <- "DoverSole"
names(survey_grid)[names(survey_grid) == "Sebastes_alutus"] <- "PacificOceanPerch"
names(survey_grid)[names(survey_grid) == "Sebastes_flavidus"] <- "YellowtailRockfish"
names(survey_grid)[names(survey_grid) == "Sebastes_reedi"] <- "YellowmouthRockfish"
names(survey_grid)[names(survey_grid) == "Sebastes_saxicola"] <- "WidowRockfish"
names(survey_grid)[names(survey_grid) == "Thaleichthys_pacificus"] <- "Eulachon"

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
MapLayers( griddata = survey_grid, layers = species, type = "survey")
MapLayers( griddata = presence_grid, layers = species, type = "presence")
