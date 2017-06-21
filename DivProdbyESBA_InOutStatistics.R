###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview: Computes statistics for diveristy and productivity metrics by ebsa. 
#           Uses the percentile bootstrapping method to estimate a 95% confidence 
#           interval around the statistic.
# 
# References:
#  [1] Efron, B. (1987) Better bootstrap confidence intervals (with Discussion).
#      Journal of the American Statistical Association, 82, 171-200.
#
###############################################################################


# Load packages
library(sp)

# Go to parent directory
setwd('..')


# load data
load(file="Aggregated/EBSA_Diversity_Overlay.Rdata") #divEBSA
load(file="Aggregated/EBSA_Productivity_Overlay.Rdata") #prodEBSA

# combine lists
comb <- c(divEBSA, prodEBSA)

# ebsas
ebsas <- c("HecateStraitFront","BellaBellaNearshore","BrooksPeninsula","CapeStJames",
           "CentralMainland","ChathamSound","DogfishBank","HaidaGwaiiNearshore",
           "LearmonthBank","McIntyreBay","NorthIslandsStraits","ScottIslands",
           "ShelfBreak","SpongeReefs")

#---------------------------------------------------------#

## Diveristy Productivity Data ##

# empty list
dpdat <- list()

# loop through each metric
for(s in names(comb)){
    # aggregated data
    spdf <- comb[[s]]

    # add outside grid cells column
    out <- spdf@data[-1]
    outindex <- which(apply(out, 1, sum) == 0)
    spdf@data$Outside <- 0
    spdf@data$Outside[outindex] <- 1
    
    # esbas (including outside area)
    ebsa <- names(spdf)[-1]
  
    # empty
    dat <- NULL
    # loop through ebsas including outside of ebsa area
    for(e in ebsa){
      
      # subset the data to include only species data in the inside cells of ebsa == e
      datsub <- data.frame(sp=spdf@data[,1],spdf@data[e])
      spdat <- datsub$sp[datsub[e] == 1]
      
      # Compute statistics of sample data
      sp_mean <- mean(spdat, na.rm=TRUE)
      
      # Number of bootstrap samples
      nboot <- 10000
      # Generate bootstrap samples, i.e. an array of n x nboot 
      rmna <- spdat[!is.na(spdat)]
      n <- length(rmna)
      tmpdata = sample(rmna,n*nboot, replace=TRUE)
      bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot)
      
      # Compute statistics of the bootstrap samples
      boot_mean <- apply(bootstrapsample, 2, mean, na.rm=TRUE)
      
      # Find the 0.05 and 0.95 quantile for each statistic
      # 95% CI using the percentile method
      q_mean = quantile(boot_mean, c(0.025, 0.975), na.rm=TRUE)
      
      # Bind together
      df <- data.frame(EBSA = e, mean = sp_mean, lower_CI = q_mean[1], upper_CI = q_mean[2],
                       stringsAsFactors = FALSE)
      dat <- rbind(dat, df)
    }
    
    # add to list
    dpdat[[s]] <- dat
  }


# save survey data
save(dpdat, file="Aggregated/EBSA_DivProd_Statistics.Rdata")

