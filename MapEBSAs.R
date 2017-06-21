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
# aggregate by grid
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
load(file="Aggregated/Grid_DensityData.Rdata") #dens
load(file="Aggregated/Grid_PresenceData.Rdata") #pres
load(file="Aggregated/Grid_DiversityData.Rdata") #div

# all important species
load("Scripts/Species")

# Reclass species names to common names
for (d in c("dens", "pres", "div")){
  df <- get(d)
  names(df)[names(df) == "Hippoglossus_stenolepis"] <- "Halibut"
  names(df) <- gsub("_"," ", names(df))
  names(df) <- gsub("rockfish","RF", names(df))
  names(df)[names(df) == "Black footed Albatross"] <- "Albatross"
  names(df)[names(df) == "Cassins Auklet"] <- "C. Auklet"
  names(df)[names(df) == "Fork tailed Storm petrel"] <- "F.T. Storm Petrels"
  names(df)[names(df) == "Large_Gulls"] <- "Large Gulls"
  names(df)[names(df) == "Leachs Storm petrel"] <- "L. Storm Petrels"
  names(df)[names(df) == "Red necked Phalarope"] <- "Phalarope"
  names(df)[names(df) == "Rhinoceros Auklet"] <- "R. Auklet"
  names(df)[names(df) == "Tufted Puffin"] <- "Tufted Puffin"
  names(df)[names(df) == "Pacific Ocean perch"] <- "P.O. perch"
  names(df)[names(df) == "Yelloweye line"] <- "Yelloweye RF line"
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
         width = 4.8 , height = 5.25, units = "in", res = 300,
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
MapLayers( griddata = dens, layers = species, dir = "Density", type = "density")
MapLayers( griddata = pres, layers = species, dir = "Presence", type = "presence")
MapLayers( griddata = div, layers = species, dir = "Diversity", type = "diversity")
