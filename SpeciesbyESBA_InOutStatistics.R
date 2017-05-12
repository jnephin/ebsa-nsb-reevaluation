###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview: Computes statistics by species for each important ebsa and the area 
#           outside of the important ebsas for that species. Uses the percentile
#           bootstrapping method to estimate a 95% confidence interval around
#           the statistic.
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

# functions
pPres <- function(x){
  y <- x[!is.na(x)]
  p <- length(y[y>0])/length(y)
  return(p)
}
pNA <- function(x){
  length(x[is.na(x)])/length(x)
}



# load data
load(file="Aggregated/EBSA_Mean_Overlay.Rdata") #meanEBSA
load(file="Aggregated/EBSA_Presence_Overlay.Rdata") #presEBSA


#---------------------------------------------------------#

# all important species
species <- c("Abalone","Alcids","Ancient_Murrelet","Black_legged_Kittiwake","BlueWhale",
             "Brandts_Cormorant","ButterSole","Cassins_Auklet","Common_Murre","DoverSole",
             "DungenessCrab","EnglishSole","Eulachon","FinWhale","Fork_tailed_Storm_petrels",
             "Geoduck","Glaucous_winged_Gull","GreenSturgeon","GreenUrchin","GreyWhale","Hake", 
             "Halibut","Halibut_phma","Herring","Herring_Gulls","HerringSpawn","Humpback",
             "KillerWhale","Leachs_Storm_petrels","Lingcod","Lingcod_phma","PacificCod",
             "PacificOceanPerch","Pelagic_Cormorant","Phalaropes","Pigeon_Guillemot",
             "Prawn","RedSeaCucumber","RedUrchin","Rhinoceros_Auklet","RockSole","Sablefish",
             "Sablefish_phma","SandSole","Scoters","SeaOtterRange","SeiWhale","Shearwaters",
             "Shrimp","Sooty_Shearwaters","SpermWhale","SpongeReef","StellarSeaLionRookeries",
             "TannerCrab","Tufted_Puffin","WidowRockfish","YellowmouthRockfish","YellowtailRockfish",
             "BlackRockfish","ChinaRockfish","CopperRockfish","QuillbackRockfish","TigerRockfish",
             "YelloweyeRockfish")



#---------------------------------------------------------#

## Survay Data ##

# empty list
surveydat <- list()

# loop through each species in survey data
for(s in species){
  # is the species present in the dataset
  if( s %in% names(meanEBSA) ){
    
    # aggregated data for species s
    spdf <- meanEBSA[[s]]

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
      sp_pPres <- pPres(spdat)*100
      sp_pNA  <- pNA(spdat)*100
      
      # Number of bootstrap samples
      nboot <- 10000
      # Generate bootstrap samples, i.e. an array of n x nboot 
      n <- length(spdat)
      tmpdata = sample(spdat,n*nboot, replace=TRUE)
      bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot)
      
      # Compute statistics of the bootstrap samples
      boot_mean <- apply(bootstrapsample, 2, mean, na.rm=TRUE)
      boot_pPres <- apply(bootstrapsample, 2, pPres) * 100
      boot_pNA  <- apply(bootstrapsample, 2, pNA) * 100
      
      # Find the 0.05 and 0.95 quantile for each statistic
      # 95% CI using the percentile method
      q_mean = quantile(boot_mean, c(0.025, 0.975), na.rm=TRUE)
      q_pPres = quantile(boot_pPres, c(0.025, 0.975), na.rm=TRUE)
      q_pNA = quantile(boot_pNA, c(0.025, 0.975))
      
      # Bind together
      stats <- c(sp_mean,sp_pPres,sp_pNA)
      lower <- c(q_mean[1],q_pPres[1],q_pNA[1])
      upper <-c(q_mean[2],q_pPres[2],q_pNA[2])
      df <- data.frame(EBSA = e, stat = c("mean","pPres","pNA"),
                       value = stats,lower_CI = lower, upper_CI = upper,
                       stringsAsFactors = FALSE)
      dat <- rbind(dat, df)
    }
    
    # add to list
    surveydat[[s]] <- dat
  }
}

# save survey data
save(surveydat, file="Aggregated/EBSA_Survey_Statistics.Rdata")



## Presence Data ##

# empty list
presdat <- list()

# loop through each species in presence data
for(s in species){
  # is the species present in the dataset
  if( s %in% names(presEBSA) ){
    
    # aggregated data for species s
    spdf <- presEBSA[[s]]
    
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
      sp_pPres <- pPres(spdat)*100

      # Number of bootstrap samples
      nboot <- 10000
      # Generate bootstrap samples, i.e. an array of n x nboot 
      n <- length(spdat)
      tmpdata = sample(spdat,n*nboot, replace=TRUE)
      bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot)
      
      # Compute statistics of the bootstrap samples
      boot_pPres <- apply(bootstrapsample, 2, pPres) * 100

      # Find the 0.05 and 0.95 quantile for each statistic
      # 95% CI using the percentile method
      q_pPres = quantile(boot_pPres, c(0.025, 0.975))

      # Bind together
      df <- data.frame(EBSA = e, stat = "pPres",
                       value = sp_pPres,lower_CI = q_pPres[1], upper_CI = q_pPres[2],
                       stringsAsFactors = FALSE)
      dat <- rbind(dat, df)
    }
    
    # add to list
    presdat[[s]] <- dat
  }
}

# save presence data
save(presdat, file="Aggregated/EBSA_Presence_Statistics.Rdata")



