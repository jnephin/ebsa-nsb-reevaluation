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
  length(x[x>0 & !is.na(x)])/length(x)
}
pNA <- function(x){
  length(x[is.na(x)])/length(x)
}
p0 <- function(x){
  length(x[x==0])/length(x)
}


# load data
load(file="Aggregated/EBSA_Density_Overlay.Rdata") #densEBSA
load(file="Aggregated/EBSA_Presence_Overlay.Rdata") #presEBSA


#---------------------------------------------------------#
# ebsas
ebsas <- c("HecateStraitFront","BellaBellaNearshore","BrooksPeninsula","CapeStJames",
           "CentralMainland","ChathamSound","DogfishBank","HaidaGwaiiNearshore",
           "LearmonthBank","McIntyreBay","NorthIslandsStraits","ScottIslands",
           "ShelfBreak","SpongeReefs")

# load species by ebsas
load("Aggregated/SpeciesByEBSAs.Rdata") #spbyebsa

#---------------------------------------------------------#

## Density Data ##

# empty list
densdat <- list()

# loop through each species in survey data
for(s in names(densEBSA)){
  
  # aggregated data for species s
  spdf <- densEBSA[[s]]
  
  # ebsas where the species was listed as important
  e <- spbyebsa[[s]]
  
  # add outside attribute - area outside of ebsa for which the sp is important
  out <- spdf@data[e]
  outindex <- which(apply(out, 1, sum) == 0)
  spdf@data$Outside <- 0
  spdf@data$Outside[outindex] <- 1
  spdf@data$OutsideSmall <- spdf@data$Outside
  spdf@data$OutsideMed <- spdf@data$Outside
  spdf@data$OutsideLarge <- spdf@data$Outside
  
  # esbas (including outside area)
  ebsa <- names(spdf)[-1]
  
  # mean sample size inside EBSAs of interest
  indat <- spdf@data[s]
  out[out == 0] <- NA
  ess <- c(indat) * out
  ss <- apply(ess, 2, function(x) length(x[!is.na(x)]))
  mss <- round(mean(ss))
  
  # sample size outside EBSAs of interest
  outside <- spdf@data$Outside
  outside[outside == 0] <- NA
  outsidenona <- indat * outside
  outss <- length(outsidenona[!is.na(outsidenona)])
  
  # range of sample sizes
  ssrange <- round(seq(from = mss, to = outss, by = ((outss - mss)/(3))))
  
  # empty
  dat <- NULL
  # loop through ebsas including outside of ebsa area
  for(i in ebsa){
    
    # subset the data to include only species data in the inside cells of ebsa == e
    datsub <- data.frame(sp=spdf@data[,1],spdf@data[i])
    spdat <- datsub$sp[datsub[i] == 1]
    ssn <- length(spdat[!is.na(spdat)])
    
    # remove na values
    rmna <- spdat[!is.na(spdat)]
    
    # Compute statistics of sample data
    if( !i %in% c("OutsideSmall","OutsideMed","OutsideLarge")){
      sp_mean <- mean(spdat, na.rm=TRUE)
    } else if(i == "OutsideSmall"){
      s_sample <- sample(rmna,ssrange[1])
      sp_mean <- mean(s_sample)
    } else if(i == "OutsideMed"){
      m_sample <- sample(rmna,ssrange[2])
      sp_mean <- mean(m_sample)
    } else if(i == "OutsideLarge"){
      l_sample <- sample(rmna,ssrange[3])
      sp_mean <- mean(l_sample)
    }
    
    # Number of bootstrap samples
    nboot <- 10000
    # Generate bootstrap samples, i.e. an array of n x nboot 
    if( !i %in% c("OutsideSmall","OutsideMed","OutsideLarge")){
      n <- length(rmna)   
    } else if (i == "OutsideSmall") {
      rmna <- s_sample
      n <- ssrange[1]
      ssn <- ssrange[1]
    } else if (i == "OutsideMed") {
      rmna <- m_sample
      n <- ssrange[2]
      ssn <- ssrange[2]
    } else if (i == "OutsideLarge") {
      rmna <- l_sample
      n <- ssrange[3]
      ssn <- ssrange[3]
    }
    tmpdata = sample(rmna,n*nboot, replace=TRUE)
    bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot)
    
    # Compute statistics of the bootstrap samples
    boot_mean <- apply(bootstrapsample, 2, mean, na.rm=TRUE)
    boot_pPres <- apply(bootstrapsample, 2, pPres) * 100
    
    # Find the 0.05 and 0.95 quantile for each statistic
    # 95% CI using the percentile method
    q_mean = quantile(boot_mean, c(0.025, 0.975), na.rm=TRUE)
    q_pPres = quantile(boot_pPres, c(0.025, 0.975), na.rm=TRUE)
    
    # Bind together
    stats <- c(sp_mean,sp_pPres,sp_pNA)
    lower <- c(q_mean[1],q_pPres[1],NA)
    upper <-c(q_mean[2],q_pPres[2],NA)
    df <- data.frame(EBSA = i, stat = c("mean","pPres","pNA"),
                     value = stats,lower_CI = lower, upper_CI = upper,
                     ss = rep(ssn,3),
                     stringsAsFactors = FALSE)
    dat <- rbind(dat, df)
  }
  
  # add to list
  densdat[[s]] <- dat
}

# save survey data
save(densdat, file="Aggregated/EBSA_Density_Statistics_Sensitivity.Rdata")



## Presence Data ##

# empty list
presdat <- list()

# loop through each species in presence data
for( s in names(presEBSA) ){
  
  # aggregated data for species s
  spdf <- presEBSA[[s]]
  # ebsas where the species was listed as important
  e <- spbyebsa[[s]]
  
  # add outside attribute - area outside of ebsa for which the sp is important
  out <- spdf@data[e]
  outindex <- which(apply(out, 1, sum) == 0)
  spdf@data$Outside <- 0
  spdf@data$Outside[outindex] <- 1
  spdf@data$OutsideSmall <- spdf@data$Outside
  spdf@data$OutsideMed <- spdf@data$Outside
  spdf@data$OutsideLarge <- spdf@data$Outside
  
  # esbas (including outside area)
  ebsa <- names(spdf)[-1]
  
  # mean sample size inside EBSAs of interest
  indat <- spdf@data[s]
  out[out == 0] <- NA
  ess <- c(indat) * out
  ss <- apply(ess, 2, function(x) length(x[!is.na(x)]))
  mss <- round(mean(ss))
  
  # sample size outside EBSAs of interest
  outside <- spdf@data$Outside
  outside[outside == 0] <- NA
  outsidenona <- indat * outside
  outss <- length(outsidenona[!is.na(outsidenona)])
  
  # range of sample sizes
  ssrange <- round(seq(from = mss, to = outss, by = ((outss - mss)/(3))))
  
  # empty
  dat <- NULL
  # loop through ebsas including outside of ebsa area
  for(i in ebsa){
    
    # subset the data to include only species data in the inside cells of ebsa == e
    datsub <- data.frame(sp=spdf@data[,1],spdf@data[i])
    spdat <- datsub$sp[datsub[i] == 1]
    ssn <- length(spdat[!is.na(spdat)])
    
    # Compute statistics of sample data
    if( !i %in% c("OutsideSmall","OutsideMed","OutsideLarge")){
      sp_pPres <- pPres(spdat)*100
      sp_pNA <- p0(spdat)*100
    } else if(i == "OutsideSmall"){
      s_sample <- sample(spdat,ssrange[1])
      sp_pPres <- pPres(s_sample)*100
      sp_pNA <- p0(s_sample)*100
    } else if(i == "OutsideMed"){
      m_sample <- sample(spdat,ssrange[2])
      sp_pPres <- pPres(m_sample)*100
      sp_pNA <- p0(m_sample)*100
    } else if(i == "OutsideLarge"){
      l_sample <- sample(spdat,ssrange[3])
      sp_pPres <- pPres(l_sample)*100
      sp_pNA <- p0(l_sample)*100
    }
    
    # Number of bootstrap samples
    nboot <- 10000
    # Generate bootstrap samples, i.e. an array of n x nboot 
    if( !i %in% c("OutsideSmall","OutsideMed","OutsideLarge")){
      rmna <- spdat[!is.na(spdat)]
      n <- length(rmna)   
    } else if (i == "OutsideSmall") {
      rmna <- s_sample
      n <- ssrange[1]
      ssn <- ssrange[1]
    } else if (i == "OutsideMed") {
      rmna <- m_sample
      n <- ssrange[2]
      ssn <- ssrange[2]
    } else if (i == "OutsideLarge") {
      rmna <- l_sample
      n <- ssrange[3]
      ssn <- ssrange[3]
    }
    tmpdata = sample(rmna,n*nboot, replace=TRUE)
    bootstrapsample = matrix(tmpdata, nrow=n, ncol=nboot)
    
    
    # Compute statistics of the bootstrap samples
    boot_pPres <- apply(bootstrapsample, 2, pPres) * 100
    boot_pNA <- apply(bootstrapsample, 2, pNA) * 100
    
    # Find the 0.05 and 0.95 quantile for each statistic
    # 95% CI using the percentile method
    q_pPres = quantile(boot_pPres, c(0.025, 0.975))
    q_pNA = quantile(boot_pNA, c(0.025, 0.975))
    
    # Bind together
    stats <- c(sp_pPres,sp_pNA)
    lower <- c(q_pPres[1],q_pNA[1])
    upper <-c(q_pPres[2],q_pNA[2])
    df <- data.frame(EBSA = i, stat = c("pPres","pNA"),
                     value = stats,lower_CI = lower, upper_CI = upper,
                     ss = rep(ssn,2),
                     stringsAsFactors = FALSE)
    dat <- rbind(dat, df)
  }
  
  # add to list
  presdat[[s]] <- dat
}

# save presence data
save(presdat, file="Aggregated/EBSA_Presence_Statistics_Sensitivity.Rdata")



