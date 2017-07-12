Re-evaluation of NSB EBSAs
==========================

Before running the scripts:   
1) Obtain relevant biological datasets   
2) Project into BC albers   
3) Clip data to NSB boundary   
4) Organise data into geodatabases: Fish.gdb, Invert.gdb, MarineMammals.gdb, 
   MarineBirds.gdb and PolygonRanges.gdb   
5) Each feature in the geodatabase is a distinct layer (e.g. species, cholorophyll, diversity)   
6) For survey data features, rename the field of interest with the name of the feature   
7) Create 5km grid from NSB polygon (with 1km buffer to ensure no data is lost at the coastal boundary)


GroupbyGrid.R
-------------
Aggregates by grid cell
* Calculates mean grid cell for density, diversity and productivity data
* Calculates presence of points within grid cell for presence only and polygon layers


SpeciesbyEBSA_Overlay.R
-----------------------
Assigns species to EBSAs based on IA info and reports. Performs spatial overlay for each species. 
Adds an inside or outside attribute for each cell for each EBSAs.

SpeciesbyEBSA_InOutStatistics.R
-------------------------------
Computes sample and bootstrap statistics by species for each important EBSA and the area 
outside of all important EBSAs. Uses the percentile bootstrapping method to estimate 
a 95% confidence interval around the sample statistic.

SpeciesbyEBSA_Figures.R
-----------------------
Produces figures that compare statistics inside and outside EBSAs.


ProductivitybyEBSA_Overlay.R
----------------------------
Performs spatial overlay for each chlorophyll layer. Adds an inside or outside attribute
for each raster cell for each EBSAs.

DiversitybyEBSA_Overlay.R
-------------------------
Performs spatial overlay for each diversity metric. Adds an inside or outside attribute
for each cell for each EBSAs.

DivProdbyEBSA_InOutStatistics.R
-------------------------------
Computes sample and bootstrap statistics for diversity and productivity metrics by species 
for each EBSA. Uses the percentile bootstrapping method to estimate a 95% confidence 
interval around the sample statistic.

DivProdbyEBSA_Figures.R
-----------------------
Produces figures that compare diveristy and productivity statistics between EBSAs and 
outside of all EBSAs.


Summarise.R
-----------
Summarises the percent of presence and no data observations for each species by EBSA

MapEBSAs.R
----------
Exports maps of species density and presence as well as and richness and diveristy 
data aggregated by a 5km grid

Results_Summary.R
-----------------
Exports table documenting level of support by species for each EBSA.
