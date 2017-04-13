# Load packages
library(dplyr)
library(reshape2)

# global options
options(scipen = 999)


# aggregating functions
pPres <- function(x){
  length(x[x>0])/length(x[x>=0])
}
pNA <- function(x){
  length(x[is.na(x)])/length(x)
}
MEAN <- function(x){
  mean(x, na.rm=TRUE)
}
STDEV <- function(x){
  sd(x, na.rm=TRUE)
}
SUM <- function(x){
  sum(x, na.rm=TRUE)
}


#---------------------------------------------------------#


# load data
load(file="Aggregated/EBSA_Survey_Overlay.Rdata") #dat
survey_dat <- dat
load(file="Aggregated/EBSA_Presence_Overlay.Rdata") #dat
pres_dat <- dat

# Convert list to data.frame
survey_df <- melt(survey_dat)
pres_df <- melt(pres_dat)

# EBSA column
survey_df$EBSA <- survey_df$L1
pres_df$EBSA <- pres_df$L1

# Remove L1 column
survey_df <- survey_df[!names(survey_df) == "L1"]
pres_df <- pres_df[!names(pres_df) == "L1"]

# convert variable to character
survey_df$variable <- as.character(survey_df$variable)
pres_df$variable <- as.character(pres_df$variable)


#---------------------------------------------------------#
# aggregate

survey_aggr <- survey_df %>%
  group_by(EBSA,Area,variable) %>%
  summarise(
    mean = MEAN(value),
    sd = STDEV(value),
    pPres = pPres(value),
    pNA = pNA(value)) %>%
  as.data.frame()


pres_aggr <- pres_df %>%
  group_by(EBSA,Area,variable) %>%
  summarise(
    sum = SUM(value),
    pPres = pPres(value)) %>%
  as.data.frame()

# combine
survey_aggr$sum <- NA
pres_aggr$mean <- NA
pres_aggr$sd <- NA
pres_aggr$pNA <- NA

survey_aggr$type <- "survey"
pres_aggr$type <- "presence"
survey_aggr <- survey_aggr[c("EBSA","Area","variable","type","mean","sd","sum","pPres","pNA")]
df <- rbind(survey_aggr, pres_aggr[names(survey_aggr)])

# save data
save(df, file="Aggregated/EBSA_Aggregation.Rdata")



