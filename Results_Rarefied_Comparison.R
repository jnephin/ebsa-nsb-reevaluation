res <- read.csv("Output/Tables/Results_Summary.csv", header=T, stringsAsFactors = F)
rare <- read.csv( "Output/Tables/Results_Summary_Rarefied.csv", header=T, stringsAsFactors = F)


dat <- merge(res,rare, by=c("EBSA","Imp","result"), all =T)

dat$match <- "YES"
dat$match[dat$species.x != dat$species.y] <- "NO"

nomat <- dat[dat$match == "NO" & dat$Imp == "y" & dat$result != "No",]
nomat


# Cape st James - Lost Moderate support for Halibut
# CentralMainland -   Lost Strong support for Red Sea Cucumber
# HaidaGwaiiNearshore  - Went from Strong to Moderate support for English sole, Herring spawn, Red Sea Cucumber
# ScottIslands - Lost strong support for Pacific Cod