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
# Exports a reference map of each EBSA showing their code
#
###############################################################################

# Load packages
library(rgdal)
library(sp)
library(raster)
library(maps)  
library(GISTools) 

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
# Re-class ebsa names
spdf@data$EBSA <- as.character(spdf@data$EBSA)
spdf@data$EBSA[spdf@data$EBSA == "BellaBellaNearshore"] <- "Bella Bella Nearshore"
spdf@data$EBSA[spdf@data$EBSA == "BrooksPeninsula"] <- "Brooks Peninsula"
spdf@data$EBSA[spdf@data$EBSA == "CapeStJames"] <- "Cape St James"
spdf@data$EBSA[spdf@data$EBSA == "CentralMainland"] <- "Central Mainland"
spdf@data$EBSA[spdf@data$EBSA == "ChathamSound"] <- "Chatham Sound"
spdf@data$EBSA[spdf@data$EBSA == "DogfishBank"] <- "Dogfish Bank"
spdf@data$EBSA[spdf@data$EBSA == "HaidaGwaiiNearshore"] <- "Haida Gwaii Nearshore"
spdf@data$EBSA[spdf@data$EBSA == "HecateStraitFront"] <- "Hecate Strait Front"
spdf@data$EBSA[spdf@data$EBSA == "LearmonthBank"] <- "Learmonth Bank"
spdf@data$EBSA[spdf@data$EBSA == "McIntyreBay"] <- "McIntyre Bay"
spdf@data$EBSA[spdf@data$EBSA == "NorthIslandsStraits"] <- "North Islands Straits"
spdf@data$EBSA[spdf@data$EBSA == "ScottIslands"] <- "Scott Islands"
spdf@data$EBSA[spdf@data$EBSA == "ShelfBreak"] <- "Shelf Break"
spdf@data$EBSA[spdf@data$EBSA == "SpongeReefs"] <- "Sponge Reefs"


# -------------------------------------------------#
# Add ebsa names to short names
spdf@data$short <- ""
spdf@data$short[spdf@data$EBSA == "Bella Bella Nearshore"] <- " BB "
spdf@data$short[spdf@data$EBSA == "Brooks Peninsula"] <- " BP "
spdf@data$short[spdf@data$EBSA == "Cape St James"] <- " CSJ "
spdf@data$short[spdf@data$EBSA == "Central Mainland"] <- " CM "
spdf@data$short[spdf@data$EBSA == "Chatham Sound"] <- " CS "
spdf@data$short[spdf@data$EBSA == "Dogfish Bank"] <- " DB "
spdf@data$short[spdf@data$EBSA == "Haida Gwaii Nearshore"] <- " HG "
spdf@data$short[spdf@data$EBSA == "Hecate Strait Front"] <- " HSF "
spdf@data$short[spdf@data$EBSA == "Learmonth Bank"] <- " LB "
spdf@data$short[spdf@data$EBSA == "McIntyre Bay"] <- " MB "
spdf@data$short[spdf@data$EBSA == "North Islands Straits"] <- " NIS "
spdf@data$short[spdf@data$EBSA == "Scott Islands"] <- " SI "
spdf@data$short[spdf@data$EBSA == "Shelf Break"] <- " SB "
spdf@data$short[spdf@data$EBSA == "Sponge Reefs"] <- " SR "


# -------------------------------------------------#
# Add colours
spdf@data$cols <- ""
spdf@data$cols[spdf@data$EBSA == "Bella Bella Nearshore"] <- "#782b00"
spdf@data$cols[spdf@data$EBSA == "Brooks Peninsula"] <- "#1f78b4"
spdf@data$cols[spdf@data$EBSA == "Cape St James"] <- "#cab2d6"
spdf@data$cols[spdf@data$EBSA == "Central Mainland"] <- "#33a02c"
spdf@data$cols[spdf@data$EBSA == "Chatham Sound"] <- "#fb9a99"
spdf@data$cols[spdf@data$EBSA == "Dogfish Bank"] <- "#b66e46"
spdf@data$cols[spdf@data$EBSA == "Haida Gwaii Nearshore"] <- "#ff7f00"
spdf@data$cols[spdf@data$EBSA == "Hecate Strait Front"] <- "#a6cee3"
spdf@data$cols[spdf@data$EBSA == "Learmonth Bank"] <- "#6a3d9a"
spdf@data$cols[spdf@data$EBSA == "McIntyre Bay"] <- "#e31a1c" 
spdf@data$cols[spdf@data$EBSA == "North Islands Straits"] <- "#f4e41b"
spdf@data$cols[spdf@data$EBSA == "Scott Islands"] <- "#b2df8a"
spdf@data$cols[spdf@data$EBSA == "Shelf Break"] <- "#fdbf6f"
spdf@data$cols[spdf@data$EBSA == "Sponge Reefs"] <- "#000000"


# -------------------------------------------------#
# legend
leg <- spdf@data[c("EBSA","short","cols")]
leg <- leg[!duplicated(leg),]
leg$labels <- paste(leg$short, "-", leg$EBSA)
leg <- leg[order(leg$EBSA),]

# -------------------------------------------------#
# Get the vertical and horizontal limits
ext <- extent( spdf )
# Get x and y limits
lims <- list( x=c(ext@xmin*1.03, ext@xmax), y=c(ext@ymin*1.17, ext@ymax) )


# -------------------------------------------------#
#axis labels

# y axis:
x <- -134 # x value in longitude at the y axis
ylab <- seq(-90,90,2) # you can adapt this sequence if you want more or less labels
yS <- SpatialPoints(cbind(x,ylab), proj4string = CRS("+proj=longlat +datum=WGS84"))
ySP<- spTransform(yS, proj4string(spdf)) 

# x axis
y <- 50 # y value in latitude at the x axis
xlab = seq(-180,180,2)
xS <- SpatialPoints(cbind(xlab,y), proj4string = CRS("+proj=longlat +datum=WGS84"))
xSP<- spTransform(xS, proj4string(spdf))

# -------------------------------------------------#
# Map 
 

# Export Figure
tiff(file="Output/Maps/ReferenceMap.tif",width = 4.8*1.3 , height = 5.25*1.3, 
     units = "in", res = 300, compression = "lzw")
par(mar=c(1.5,1.5,1.5,1.5))
plot( bcPoly, col = "grey70", border = NA, xlim = lims$x , ylim = lims$y)
rng <- par("usr") # Grab the plotting region dimensions
plot( spdf, add=T, lwd=1, border=spdf@data$cols, density=25, col=spdf@data$cols)
legend( "topright", legend = leg$labels, fill = leg$cols, density=35,
        bg = "white", box.col = NA, cex = .9, border=leg$cols)
map.scale(ext@xmin*1.25,ext@ymin*1.1,150000,"Kilometers",ndivs=3,subdiv=50,sfcol='black')
north.arrow(xb=ext@xmin*1.25, yb=ext@ymax*.6, len=10000, lab="N") 
box( lty = 'solid', col = 'black')
axis(side = 1, pos=rng[3], at = xSP@coords[,'xlab'], lab=xlab,  tcl = .3,padj=-1.5)
axis(side = 2, pos=rng[1], at = ySP@coords[,'ylab'], lab=ylab, las=1, tcl = .3,hadj=0.2)
dev.off()


