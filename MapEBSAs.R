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
# aggregate by grid with an inset plot showing the mean metric by ebsa
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

# Create output directories
dir.create("Output/Maps/Density")
dir.create("Output/Maps/Diversity")
dir.create("Output/Maps/Presence")
dir.create("Output/Maps/Productivity")

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
load(file="Aggregated/Grid_DensityData.Rdata") #dens
load(file="Aggregated/Grid_PresenceData.Rdata") #pres
load(file="Aggregated/Grid_DiversityData.Rdata") #div
load(file="Aggregated/Grid_ProductivityData.Rdata") #prod


# Reclass species names to common names
for (d in c("dens", "pres", "div", "prod")){
  df <- get(d)
  names(df) <- gsub("_"," ", names(df))
  names(df)[names(df) == "Fork tailed Storm petrel"] <- "Fork-tailed Storm-petrel"
  names(df)[names(df) == "Leachs Storm petrel"] <- "Leach's Storm petrel"
  names(df)[names(df) == "Cassins Auklet"] <- "Cassin's Auklet"
  names(df)[names(df) == "Red necked Phalarope"] <- "Red-necked Phalarope"
  names(df)[names(df) == "Yelloweye line"] <- "Yelloweye rockfish line"
  names(df)[names(df) == "Div Fish"] <- "Fish Diversity"
  names(df)[names(df) == "Div Invert"] <- "Invert Diversity"
  names(df)[names(df) == "nSp Fish"] <- "Fish Richness"
  names(df)[names(df) == "nSp Invert"] <- "Invert Richness"
  names(df)[names(df) == "Chla mean nsb"] <- "Mean Chla"
  names(df)[names(df) == "Bloom freq nsb"] <- "Bloom frequency"
  names(df)[names(df) == "RedUrchin"] <- "Red Urchin"
  names(df)[names(df) == "GreenUrchin"] <- "Green Urchin"
  names(df)[names(df) == "RedSeaCucumber"] <- "Red Sea Cucumber"
  names(df)[names(df) == "DungenessCrab"] <- "Dungeness Crab"
  names(df)[names(df) == "StellarSeaLionRookeries"] <- "Stellar Sea Lion Rookeries"
  names(df)[names(df) == "SeaOtterRange"] <- "Sea Otter Range"
  names(df)[names(df) == "TannerCrab"] <- "Tanner Crab"
  names(df)[names(df) == "SpongeReef"] <- "Sponge Reef"
  assign(d, df)
}


# -------------------------------------------------#
# Load data for inset figure 
load("Aggregated/Denisty_PlotData.Rdata") #dfdens
load("Aggregated/Presence_PlotData.Rdata") #dfpres
load("Aggregated/DivProd_PlotData.Rdata") #dfdp
dfdp$Species <- dfdp$Metric

# -------------------------------------------------#
# Map function
MapLayers <- function( griddata, df, type, style="kmeans", size=8){
  
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
    
    # Set colour scale
    if( type == "Presence" ){ #If factor
      # colour
      colours <- c("#3288BD","#D53E4F")
      # combine colours and unique factor levels
      brks <- na.omit(sort(unique( Layer@data[,1] )))
      pal <- data.frame( brks, colours, stringsAsFactors = FALSE )
      colnames(pal) <- c( p, "colours" )
      # break labels
      pal$labels <- as.character( brks )
      pal$labels[ pal$labels == "0"] <- "No data"
      pal$labels[ pal$labels == "1"] <- "Presence"
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
    lims <- list( x=c(ext@xmin*1.15, ext@xmax), y=c(ext@ymin, ext@ymax*.98) )
    
    # subset by species
    dfsub <- df[df$Species == p,]

    #y-axis limit and breaks
    brks <- pretty( c(dfsub$value,dfsub$lower_CI,dfsub$upper_CI), n=4 )
    if(max(brks) > max(dfsub$upper_CI, na.rm=T))  brks <- brks[-length(brks)]
    
    # legend title
    if(type == "Presence"){
      ltitle <- ""
    } else if(type == "Density") {
      ltitle <- "Mean Density"
    } else if( p == "Invert Richness" | p == "Fish Richness") {
      ltitle <- "Number of species"
    } else if( p == "Invert Diversity" | p == "Fish Diversity") {
      ltitle <- "Shannon's H'"
    }  else if( p == "Mean Chla") {
      ltitle <- "Mean Chlorophyll a\n (mg m-3)"
    }  else if( p == "Bloom frequency") {
      ltitle <- "Frequency of monthly\n plankton blooms"
    }
    
    # plot
    if(type == "Presence" |  type == "Productivity") 
      insetfig <- ggplot(data = dfsub, aes(x=EBSA,y=value))
    if( !(type == "Presence" | type == "Productivity") ) 
      insetfig <- ggplot(data = dfsub, aes(x=EBSA,y=value, size=pData))
    insetfig <- insetfig + geom_point(pch=16)
    insetfig <- insetfig + geom_errorbar(aes(ymin=lower_CI,ymax=upper_CI), size=1, width=0)
    if(! (type == "Presence" | type == "Productivity") ) insetfig <- insetfig + 
      scale_size_area(max_size = 3, name = "Data \ncoverage (%)",
                      breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100))
    if(type == "Presence"){
      insetfig <- insetfig + labs(x="", y="Occurrence (%)")
    } else {
      insetfig <- insetfig + labs(x="", y=ltitle)
    }
    insetfig <- insetfig + scale_y_continuous(breaks = brks)
    insetfig <- insetfig + theme(plot.background = element_blank(),
                                 panel.border = element_blank(),
                                 panel.background = element_rect(fill=NA,colour="black"),
                                 axis.ticks = element_line(colour="black"),
                                 panel.grid= element_blank(),
                                 axis.ticks.length = unit(0.1,"cm"),
                                 axis.text = element_text(size=size, colour = "black"),
                                 axis.title = element_text(size=size+1, colour = "black"),
                                 panel.spacing = unit(0.1, "lines"),
                                 legend.key = element_blank(),
                                 legend.background = element_blank(),
                                 legend.direction = "vertical",
                                 legend.position = c(.7,-.25),
                                 legend.justification = c(.5,1),
                                 legend.text = element_text(size=size, colour = "black"),
                                 legend.title = element_text(size=size+1, colour = "black"),
                                 plot.margin = unit(c(1,1,1,1), "lines"))

    if(type=="Diversity" | type=="Productivity") insetfig <- insetfig + 
      theme(axis.text.x = element_text(size=size, colour = "black", angle = 90, vjust=0.5, hjust=1))
    
    #A viewport taking up a fraction of the plot area
    vp <- viewport(width = 0.58, height = 0.4, x = 1, y = 1, just=c(1,1))
    
    # Export Figure
    tiff(file=file.path("Output/Maps", type, paste0(p, ".tif")),
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
    print(insetfig, vp = vp)
    box( lty = 'solid', col = 'black')
    dev.off()
    
  } # end loop through layers
} # End MapLayers function
# -------------------------------------------------#


# Maps
MapLayers( griddata = dens, df=dfdens, type = "Density" )
MapLayers( griddata = pres, df=dfpres,  type = "Presence" )
MapLayers( griddata = div, df=dfdp, type = "Diversity" )
MapLayers( griddata = prod, df=dfdp, type = "Productivity" )

