###############################################################################
#
# Authors:      Jessica Nephin
# Affiliation:  Fisheries and Oceans Canada (DFO)
# Group:        Marine Spatial Ecology and Analysis
# Location:     Institute of Ocean Sciences
# Contact:      e-mail: jessica.nephin@dfo-mpo.gc.ca | tel: 250.363.6564
# Project:      NSB EBSA re-assessment
#
# Overview: Prodcues figures that compare species statistics inside and outside 
#          ebsa. Species are those listed as important for the EBSAs in 
#          Clark & Jamieson 2006 Phase II.
#
###############################################################################


# Load packages
library(ggplot2)
library(reshape2)
library(scales)


# Go to parent directory
setwd('..')


# load data
load("Aggregated/EBSA_DivProd_Statistics_Sensitivity.Rdata") #dpdat


# Convert list to data.frame
dfdp <- melt(dpdat, id.vars=c("EBSA","stat","lower_CI","upper_CI", "ss"))


# re-name list column to metric
names(dfdp)[names(dfdp) == "L1"] <- "Metric"


# add missing data column
dfNA <- dfdp[dfdp$stat == "pNA",c("EBSA","Metric","value")]
colnames(dfNA)[3] <- "pNA" 
dfNA$pData <- round(100 - dfNA$pNA)
dfdp <- merge(dfdp, dfNA, by=c("EBSA","Metric"))

# subset survey data to only include stat == mean
dfdp <- dfdp[dfdp$stat == "mean",]

# -------------------------------------------------#
# remove ebsas with limited data
dfdp <- dfdp[!(dfdp$EBSA %in% c("BellaBellaNearshore","BrooksPeninsula",
                                "CentralMainland","ChathamSound",
                                "DogfishBank","LearmonthBank",
                                "NorthIslandsStraits") &
                 dfdp$Metric %in% c("Div_Invert","Div_Fish",
                                    "nSp_Fish","nSp_Invert")),]


#-------------------------------------------------#
# Reclass ebsa names to short names
for (d in c("dfdp")){
  df <- get(d)
  df$EBSA[df$EBSA == "BellaBellaNearshore"] <- " BB"
  df$EBSA[df$EBSA == "BrooksPeninsula"] <- " BP"
  df$EBSA[df$EBSA == "CapeStJames"] <- " CSJ"
  df$EBSA[df$EBSA == "CentralMainland"] <- " CM"
  df$EBSA[df$EBSA == "ChathamSound"] <- " CS"
  df$EBSA[df$EBSA == "DogfishBank"] <- " DB"
  df$EBSA[df$EBSA == "HaidaGwaiiNearshore"] <- " HG"
  df$EBSA[df$EBSA == "HecateStraitFront"] <- " HSF"
  df$EBSA[df$EBSA == "LearmonthBank"] <- " LB"
  df$EBSA[df$EBSA == "McIntyreBay"] <- " MB"
  df$EBSA[df$EBSA == "NorthIslandsStraits"] <- " NIS"
  df$EBSA[df$EBSA == "ScottIslands"] <- " SI"
  df$EBSA[df$EBSA == "ShelfBreak"] <- " SB"
  df$EBSA[df$EBSA == "SpongeReefs"] <- " SR"
  df$Area <- "In"
  df$Area[grep("Outside", df$EBSA)] <- "Out"
  df$EBSA[df$EBSA == "Outside"] <- "OUT"
  df$EBSA[df$EBSA == "OutsideSmall"] <- "OUTS"
  df$EBSA[df$EBSA == "OutsideMed"] <- "OUTM"
  df$EBSA[df$EBSA == "OutsideLarge"] <- "OUTL"
  assign(d, df)
}

# -------------------------------------------------#
# Reclass Metric names to short names

dfdp$Metric[dfdp$Metric == "Div_Fish"] <- "Fish Diversity"
dfdp$Metric[dfdp$Metric == "Div_Invert"] <- "Invert Diversity"
dfdp$Metric[dfdp$Metric == "nSp_Fish"] <- "Fish Richness"
dfdp$Metric[dfdp$Metric == "nSp_Invert"] <- "Invert Richness"
dfdp$Metric[dfdp$Metric == "Chla_mean_nsb"] <- "Mean Chla"
dfdp$Metric[dfdp$Metric == "Bloom_freq_nsb"] <- "Bloom frequency"

# add maxval
dfdp$maxval <- apply(data.frame(dfdp$upper_CI,dfdp$value), 1, max)

# order factors
dfdp$Metric <- as.factor(dfdp$Metric)
dfdp$Metric <- factor(dfdp$Metric, levels=c("Bloom frequency","Mean Chla","Fish Diversity","Invert Diversity","Fish Richness", "Invert Richness"))

# Save dfdp to plot with maps
save(dfdp, file="Aggregated/DivProd_PlotData_Sensitivity.Rdata")



# -------------------------------------------------#
# plotting functions

sensplot <- function(df, sp, ylab, height, width, size, folder){
  # subset by species
  dfsub <- df[df$Metric == sp,]
  # plot
  gfig <- ggplot(data = dfsub, aes(x=EBSA,y=value,colour=Area))+
    geom_point(pch=16, size=2.5)+
    geom_errorbar(aes(ymin=lower_CI,ymax=upper_CI), size=1, width=0)+
    geom_text(aes(label=ss, y=maxval), vjust=-.5, size=2.5) +
    labs(x="", y=ylab)+
    scale_y_continuous(breaks = pretty_breaks(n=4),expand = c(0.2, 0))+
    scale_size_area(max_size = 3, name = "Data \ncoverage\n (%)")+
    scale_colour_manual(values=c("grey20","#386cb0"), guide=F)+
    theme(panel.border = element_rect(fill=NA, colour="black"),
          panel.background = element_rect(fill="white",colour="black"),
          strip.background = element_rect(fill="white",colour=NA),
          strip.text = element_text(size=size+1, colour = "black", hjust=0, vjust=1),
          axis.ticks = element_line(colour="black"),
          panel.grid= element_blank(),
          axis.ticks.length = unit(0.1,"cm"),
          axis.text.x = element_text(size=size, colour = "black", angle=90, vjust=0.5,hjust=1),
          axis.text.y = element_text(size=size, colour = "black"),
          axis.title = element_text(size=size+1, colour = "black"),
          legend.key = element_blank(),
          plot.margin = unit(c(.1,.1,.1,.1), "lines"))+
    ggtitle(label="",subtitle = sp)
  tiff(file=file.path("Output/Figures/Sensitivity/", folder, paste0(sp, ".tif")),
       width = width , height = height, units = "in", res = 90)
  print(gfig)
  dev.off()
}




# -------------------------------------------------#
# figures

# diveristy
for (i in c("Fish Diversity","Invert Diversity")){
  sensplot(df=dfdp, sp=i, ylab="Mean Diversity", folder="Diversity",
           height = 2.5, width = 2.8, size = 8)
}


# richness
for (i in c("Fish Richness","Invert Richness")){
  sensplot(df=dfdp, sp=i, ylab="Mean Richness", folder="Diversity",
           height = 2.5, width = 2.8, size = 8)
}

# richness
sensplot(df=dfdp, sp="Bloom frequency", ylab="Frequency of monthly\n plankton blooms", 
         folder="Productivity", height = 3, width = 4.5, size = 8)
sensplot(df=dfdp, sp="Mean Chla", ylab="Mean Chlorophyll a\n (mg m-3)", 
         folder="Productivity", height = 3, width = 4.5, size = 8)
