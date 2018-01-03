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
# Prep productivity data 
#
###############################################################################

# Load packages
library(rgdal)
library(sp)
library(rgeos)
library(raster)

# Go to parent directory
setwd('..')



# -------------------------------------------------#
# Load rasters
# original straylight raster have been masked to nsb and have
# not been contrained to only show data that is not 100% interpolated
chla <- raster("Data/Productivity/Chla_mean_nsb_uncert.tif")
bloom <- raster("Data/Productivity/Bloom_freq_nsb_uncert.tif")
uncert <- raster("Data/Productivity/Uncertainty_nsb.tif")

# -------------------------------------------------#
# reclass uncert raster
m <- c(-1, 31, 1,  31, 32, -1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
binaryuncert <- reclassify(uncert, rclmat)

# -------------------------------------------------#
# remove uncertain areas from chla rasters
unc_chla <- chla * binaryuncert
unc_bloom <- bloom * binaryuncert

# -------------------------------------------------#
# reclass negatives to NA 
m <- c(-Inf, -0.00000001, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rchla <- reclassify(unc_chla, rclmat)
rbloom <- reclassify(unc_bloom, rclmat)


# -------------------------------------------------#
# write 
writeRaster(rchla, "Data/Productivity/Chla_mean_nsb.tif", overwrite=T)
writeRaster(rbloom, "Data/Productivity/Bloom_freq_nsb.tif", overwrite=T)


# -------------------------------------------------#
# convert to polgyon for plot
chlapoly <- rasterToPolygons(unc_chla)
bloompoly <- rasterToPolygons(unc_bloom)

# save
prodpoly <- list(Chla_mean_nsb=chlapoly,Bloom_freq_nsb=bloompoly)
save(prodpoly, file = "Aggregated/ProductivityPolygons.RData")

