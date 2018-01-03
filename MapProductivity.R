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
# Exports maps of mean chla and bloom freqency with EBSA boundaries
#
###############################################################################

# Load packages
library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(RColorBrewer)

# Go to parent directory
setwd('..')


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
# Load rasters
chla <- raster("Data/Productivity/Chla_mean_nsb.tif")
bloom <- raster("Data/Productivity/Bloom_freq_nsb.tif")
uncert <- raster("Data/Productivity/Uncertainty_nsb.tif")


# -------------------------------------------------#
# reclass uncert raster
m <- c(-1, 31, 1,  31, 32, 0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
binaryuncert <- reclassify(uncert, rclmat)

# -------------------------------------------------#
# remove uncertain areas from chla rasters
unc_chla <- chla * binaryuncert
unc_bloom <- bloom * binaryuncert

# -------------------------------------------------#
# convert to polgyon for plot
chlapoly <- rasterToPolygons(unc_chla,  n=4, na.rm=TRUE, digits=6, dissolve=FALSE)
chlapoly <- rasterToPolygons(unc_chla,  n=4, na.rm=TRUE, digits=6, dissolve=FALSE)

# -------------------------------------------------#
# write rasters
writeRaster(unc_chla, "Data/Productivity/Bloom_freq_nsb_cert.tif")
writeRaster(unc_bloom, "Data/Productivity/Uncertainty_nsb_cert.tif")


# -------------------------------------------------#
# Map function
MapLayers <- function( griddata, style="quantile", size=8){
  
  # layers in both grid and df data
  gridlayers <- unique(names(griddata))
  
  # loop through layers
  for(p in gridlayers){
    
    # EBSAs to bolden
    e <- df$EBSA[df$Species == p]
    
    # Keep only attribute to plot
    Layer <- griddata[p]
    
    # Set colour scale
    if( type == "Presence" ){ #If factor
      # colour
      colours <- c("snow2","#D53E4F") 
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
      udat <- unique(dat)
      if( length(udat) > 6 ){
        brksint <- classIntervals( udat, n=6, style=style, intervalClosure="right" )
      } else {
        brksint <- NULL
        brksint$brks <- unique(dat)
      }
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
      if ( length(na.omit(unique(labels))) >= 3 ){
        colours <- rev(brewer.pal( length(na.omit(unique(labels))), "Spectral" ))
      } else{
        colours <-  c("#3288BD","#D53E4F")
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
    if(type=="Productivity") {
      lims <- list( x=c(ext@xmin*1.4, ext@xmax*.95), y=c(ext@ymin, ext@ymax*.98) )
    } else {
      lims <- list( x=c(ext@xmin*1.15, ext@xmax), y=c(ext@ymin, ext@ymax*.98) )
    }
    
    # subset by species
    dfsub <- df[df$Species == p,]
    
    #y-axis limit and breaks
    brks <- pretty( c(dfsub$value,dfsub$lower_CI,dfsub$upper_CI), n=4 )
    if(max(brks) > max(dfsub$upper_CI, na.rm=T))  brks <- brks[-length(brks)]
    
    # legend title
    if(type == "Presence"){
      ltitle <- ""
    } else if(type == "Density" & p != "Herring spawn") {
      ltitle <- "Mean Density"
    } else if(type == "Density" & p == "Herring spawn") {
      ltitle <- "Mean SHI"
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
    insetfig <- ggplot(data = dfsub, aes(x=EBSA,y=value))
    insetfig <- insetfig + geom_point(pch=16, size = 2)
    insetfig <- insetfig + geom_errorbar(aes(ymin=lower_CI,ymax=upper_CI), size=1, width=0)
    insetfig <- insetfig + geom_text(aes(label=ss, y=upper_CI), vjust=-1, size=2.5)
    if(type == "Presence"){
      insetfig <- insetfig + labs(x="", y="Occurrence (%)")
    } else {
      insetfig <- insetfig + labs(x="", y=ltitle)
    }
    insetfig <- insetfig + scale_y_continuous(breaks = brks, 
                                              limits=c(min(c(dfsub$lower_CI, dfsub$value))*.95, 
                                                       max(dfsub$upper_CI)*1.1))
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
    if(type=="Productivity") {
      vp <- viewport(width = 0.64, height = 0.45, x = 1, y = 1, just=c(1,1))
    } else {
      vp <- viewport(width = 0.58, height = 0.45, x = 1, y = 1, just=c(1,1))
    }
    
    # Export Figure
    if(type=="Productivity") {
      tiff(file=file.path("Output/Maps", type, paste0(p, ".tif")),
           width = 5.3 , height = 5.25, units = "in", res = 300,
           compression = "lzw")    
    } else {
      tiff(file=file.path("Output/Maps", type, paste0(p, ".tif")),
           width = 4.8 , height = 5.25, units = "in", res = 300,
           compression = "lzw")
    }
    
    par(mar=c(1,1,1,1))
    plot( bcPoly, col = "grey60", border = NA, xlim = lims$x , ylim = lims$y)
    if(p != "Abalone") plot( Layer, col=Layer$colours, border=NA, add = T )
    plot( spdf[!spdf$EBSA %in% e,], add=T, lwd=.6 )
    plot( spdf[spdf$EBSA %in% e,], add=T, lwd=1.1 )
    if(p != "Abalone")  legend( "bottomleft", legend = pal$labels, fill = pal$colours,
                                title = ltitle, bg = NA, box.col = NA, cex = .8, pt.cex=3, border=NA)
    if(p == "Abalone") mtext(text = "   Cannot display \n   Abalone locations", 
                             side = 1, line = -2.5, adj = 0, cex = .8)
    title(p, adj=.05, cex.main = 1 )
    print(insetfig, vp = vp)
    box( lty = 'solid', col = 'black')
    dev.off()
    
  } # end loop through layers
} # End MapLayers function
# -------------------------------------------------#



# Map
MapRaster( chla, filename="MeanChla", 
           legend = "Mean Chlorophyll a\n (mg m-3)" )
MapRaster( bloom, filename="BloomFreq", 
           legend = "Frequency of monthly\n plankton blooms" )


