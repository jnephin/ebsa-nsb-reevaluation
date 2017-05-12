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
load("Aggregated/EBSA_DivProd_Statistics.Rdata") #dpdat


# Convert list to data.frame
dfdp <- melt(dpdat, id.vars=c("EBSA","mean","lower_CI","upper_CI"))


# re-name list column to metric
names(dfdp)[names(dfdp) == "L1"] <- "Metric"


# -------------------------------------------------#
# Reclass ebsa names to short names
dfdp$EBSA[dfdp$EBSA == "BellaBellaNearshore"] <- " BB "
dfdp$EBSA[dfdp$EBSA == "BrooksPeninsula"] <- " BP "
dfdp$EBSA[dfdp$EBSA == "CapeStJames"] <- " CJ "
dfdp$EBSA[dfdp$EBSA == "CentralMainland"] <- " CM "
dfdp$EBSA[dfdp$EBSA == "ChathamSound"] <- " CS "
dfdp$EBSA[dfdp$EBSA == "DogfishBank"] <- " DB "
dfdp$EBSA[dfdp$EBSA == "HaidaGwaiiNearshore"] <- " HG "
dfdp$EBSA[dfdp$EBSA == "HecateStraitFront"] <- " HS "
dfdp$EBSA[dfdp$EBSA == "LearmonthBank"] <- " LB "
dfdp$EBSA[dfdp$EBSA == "McIntyreBay"] <- " MB "
dfdp$EBSA[dfdp$EBSA == "NorthIslandsStraits"] <- " NIS "
dfdp$EBSA[dfdp$EBSA == "ScottIslands"] <- " SI "
dfdp$EBSA[dfdp$EBSA == "ShelfBreak"] <- " SB "
dfdp$EBSA[dfdp$EBSA == "SpongeReefs"] <- " SR "
dfdp$EBSA[dfdp$EBSA == "Outside"] <- "OUT"
dfdp$Area <- "In"
dfdp$Area[dfdp$EBSA == "OUT"] <- "Out"


# -------------------------------------------------#
# Reclass Metric names to short names

dfdp$Metric[dfdp$Metric == "Div_Fish"] <- " Fish Diversity"
dfdp$Metric[dfdp$Metric == "Div_Invert"] <- " Invert Diversity"
dfdp$Metric[dfdp$Metric == "nSp_Fish"] <- " Fish Richness"
dfdp$Metric[dfdp$Metric == "nSp_Invert"] <- " Invert Richness"
dfdp$Metric[dfdp$Metric == "spchla"] <- "Mean Chla"
dfdp$Metric[dfdp$Metric == "spbloom"] <- "Bloom frequency"


# -------------------------------------------------#
# plotting function

spplot <- function(df, ylab, height, width, ncol, size=size){
  # plot
  gfig <- ggplot(data = df, aes(x=EBSA,y=mean,colour=Area))+
    geom_point(pch=16, size=2)+
    geom_errorbar(aes(ymin=lower_CI,ymax=upper_CI), size=1, width=0)+
    labs(x="", y=ylab)+
    scale_y_continuous(breaks = pretty_breaks(n=4))+
    facet_wrap(~Metric, scales="free", ncol=ncol)+
    scale_colour_manual(values=c("#7fc97f","#386cb0"), guide=F)+
    theme(panel.border = element_rect(fill=NA, colour="black"),
          panel.background = element_rect(fill="white",colour="black"),
          strip.background = element_rect(fill="white",colour=NA),
          strip.text = element_text(size=size+1, colour = "black", hjust=0, vjust=1),
          axis.ticks = element_line(colour="black"),
          panel.grid= element_blank(),
          axis.ticks.length = unit(0.1,"cm"),
          axis.text = element_text(size=size, colour = "black"),
          axis.title = element_text(size=size+1, colour = "black"),
          panel.spacing = unit(0.1, "lines"),
          plot.margin = unit(c(.2,.2,.2,.2), "lines"))
  tiff("Output/Figures/Diversity_Productivity.tif",
       width = width , height = height, units = "in", res = 100)
  print(gfig)
  dev.off()
}

## add y axis labels

# -------------------------------------------------#
# figure
spplot(df=dfdp, ylab="", height = 7.5, width = 6.5, ncol = 2, size = 8)

