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
# Exports maps of species abundance and richness/diveristy data
# aggregate by grid with an inset plot showing the variance metric by ebsa
#
###############################################################################

# Load packages
library(rgdal)
library(sp)
library(raster)
library(rgeos)
library(classInt)
library(RColorBrewer)
library(ggplot2)
library(grid)

# Go to parent directory
setwd('..')

# options
options(scipen=999)

# Create output directories
dir.create("Output/Maps/Variance")


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

# reclass
spdf@data$EBSA <- as.character(spdf@data$EBSA)
spdf@data$EBSA[spdf@data$EBSA == "BellaBellaNearshore"] <- " BB "
spdf@data$EBSA[spdf@data$EBSA == "BrooksPeninsula"] <- " BP "
spdf@data$EBSA[spdf@data$EBSA == "CapeStJames"] <- " CSJ "
spdf@data$EBSA[spdf@data$EBSA == "CentralMainland"] <- " CM "
spdf@data$EBSA[spdf@data$EBSA == "ChathamSound"] <- " CS "
spdf@data$EBSA[spdf@data$EBSA == "DogfishBank"] <- " DB "
spdf@data$EBSA[spdf@data$EBSA == "HaidaGwaiiNearshore"] <- " HG "
spdf@data$EBSA[spdf@data$EBSA == "HecateStraitFront"] <- " HSF "
spdf@data$EBSA[spdf@data$EBSA == "LearmonthBank"] <- " LB "
spdf@data$EBSA[spdf@data$EBSA == "McIntyreBay"] <- " MB "
spdf@data$EBSA[spdf@data$EBSA == "NorthIslandsStraits"] <- " NIS "
spdf@data$EBSA[spdf@data$EBSA == "ScottIslands"] <- " SI "
spdf@data$EBSA[spdf@data$EBSA == "ShelfBreak"] <- " SB "
spdf@data$EBSA[spdf@data$EBSA == "SpongeReefs"] <- " SR "


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
load(file="Aggregated/Grid_DensityData_var.Rdata") #dens
load(file="Aggregated/Grid_ProductivityData_var.Rdata") #prod
load(file="Aggregated/Grid_DiversityData_var.Rdata") #div


# -------------------------------------------------#
# Reclass species names to common names
for (d in c("dens", "div", "prod")){
  df <- get(d)
  names(df) <- gsub("_"," ", names(df))
  names(df)[names(df) == "CassinsAuklet"] <- "Cassin's Auklet"
  names(df)[names(df) == "CommonMurre"] <- "Common Murre"
  names(df)[names(df) == "StormPetrels"] <- "Storm Petrels"
  names(df)[names(df) == "PigeonGuillemot"] <- "Pigeon Guillemot"
  names(df)[names(df) == "GlaucousWingedGull"] <- "Glaucous-Winged Gull"
  names(df)[names(df) == "RhinocerosAuklet"] <- "Rhinoceros Auklet"
  names(df)[names(df) == "TuftedPuffin"] <- "Tufted Puffin"
  names(df)[names(df) == "StellarSeaLionRookeries"] <- "Stellar Sea Lion Rookeries"
  names(df)[names(df) == "SeaOtterRange"] <- "Sea Otter Range"
  names(df)[names(df) == "SpongeReef"] <- "Sponge Reef"
  names(df)[names(df) == "Div Fish"] <- "Fish Diversity"
  names(df)[names(df) == "Div Invert"] <- "Invert Diversity"
  names(df)[names(df) == "nSp Fish"] <- "Fish Richness"
  names(df)[names(df) == "nSp Invert"] <- "Invert Richness"
  names(df)[names(df) == "Chla mean nsb"] <- "Mean Chla"
  names(df)[names(df) == "Bloom freq nsb"] <- "Bloom frequency"
  assign(d, df)
}


# -------------------------------------------------#
# Load data 
load("Aggregated/Denisty_PlotData.Rdata") #dfdens
load("Aggregated/Presence_PlotData.Rdata") #dfpres
load("Aggregated/DivProd_PlotData.Rdata") #dfdp
dfdp$Species <- dfdp$Metric


# -------------------------------------------------#
# Map function
MapLayers <- function( griddata, df, style="quantile", size=8){
  
  # layers in both grid and df data
  gridlayers <- unique(names(griddata))
  dflayers <- unique(df$Species)
  layers <- dflayers[dflayers %in% gridlayers]
  
  # loop through layers
  for(p in layers){
    
    # EBSAs to bolden
    e <- df$EBSA[df$Species == p]
    
    # Keep only attribute to plot
    Layer <- griddata[p]
    
    # if all values are NA do nothing
    if(all(is.na(Layer@data))){
      
    } else {
      
      # Set colour scale
      dat <- Layer@data[,1]
      dat <- dat[!is.na(dat)]
      udat <- unique(dat)
      if( length(udat) > 6 ){
        brksint <- classIntervals( udat, n=6, style=style, intervalClosure="right" )
        # Cut data using breaks
        brks <- cut( Layer@data[,1], breaks = brksint$brks, include.lowest = TRUE )
        # break labels
        labels <- sub( ".*,","", brks )
        labels <- as.numeric( sub( "]","", labels ) )
        if (p %in% c("Sponge", "Seapen","Coral")){
          labels <- round(labels,5)
        } else {
          labels <- round(labels,3)
        }
        # set zero label
        labels[Layer@data[,1] == 0] <- 0
        # get colours
        colours <- rev(brewer.pal( length(na.omit(unique(labels))), "Spectral" ))
        
        # less than 6 unique data values
      } else {
        brksint <-NULL
        brksint$brks <- unique(dat)
        brks <- Layer@data[,1]
        labels <- round(brks, 3)
        if( length(brksint$brks) == 1 ) colours <-  c("#D53E4F")
        if( length(brksint$brks) == 2 ) colours <-  c("#3288BD","#D53E4F")
        if( length(brksint$brks) > 2 ) colours <- rev(brewer.pal( length(unique(dat)), "Spectral" ))
      }
      # combine colours and breaks
      pal <- data.frame( labels = c(sort(unique( labels )),NA), colours = c(colours,"snow2"),
                         stringsAsFactors = FALSE )
      #set NA label
      labels[is.na(Layer@data[,1])] <- "No data"
      pal$labels[is.na(pal$labels)] <- "No data"
      # merge colours with layer data
      Layer@data$labels <- labels
      Layer <- sp::merge( Layer, pal, by = "labels")
      # label factor levels
      lev <- unique(sort(as.numeric(
        as.character(Layer@data$labels[!Layer@data$labels == "No data"])))) 
      Layer@data$labels <- factor(Layer@data$labels, levels = c(lev, "No data"))
    }
      
      # Get the vertical and horizontal limits
      ext <- extent( Layer )
      # Get x and y limits
      lims <- list( x=c(ext@xmin*1.05, ext@xmax), y=c(ext@ymin, ext@ymax*.98) )
      
      # subset by species
      dfsub <- df[df$Species == p,]
      
      #y-axis limit and breaks
      brks <- pretty( c(dfsub$value,dfsub$lower_CI,dfsub$upper_CI), n=4 )
      if(max(brks) > max(dfsub$upper_CI, na.rm=T))  brks <- brks[-length(brks)]
      
      # legend title
      ltitle <- "Standard deviation"
      
      # Export Figure
      tiff(file=file.path("Output/Maps/Variance", paste0(p, ".tif")),
           width = 4.8 , height = 5.25, units = "in", res = 300,
           compression = "lzw")
      par(mar=c(1,1,1,1))
      plot( bcPoly, col = "grey60", border = NA, xlim = lims$x , ylim = lims$y)
      plot( Layer, col=Layer$colours, border=NA, add = T )
      plot( spdf[!spdf$EBSA %in% e,], add=T, lwd=.6 )
      plot( spdf[spdf$EBSA %in% e,], add=T, lwd=1.1 )
      legend( "bottomleft", legend = pal$labels, fill = pal$colours,
              title = ltitle, bg = NA, box.col = NA, cex = .8, pt.cex=3, border=NA)
      title(p, adj=.05, cex.main = 1 )
      box( lty = 'solid', col = 'black')
      dev.off()
  } # end loop through layers
} # End MapLayers function
# -------------------------------------------------#


# Maps
MapLayers( griddata = dens, df=dfdens) # density
MapLayers( griddata = prod, df=dfdp) #  prod
MapLayers( griddata = div, df=dfdp) # div 

