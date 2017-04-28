Re-evaluation of NSB EBSAs
==========================

Before running the scripts:   
1) Obtain relevant biological datasets   
2) Project into BC albers   
3) Clip data to NSB boundary   
4) Organise data into 3 geodatabases: SurveyData.gdb, PresenceData.gdb and PolygonData.gdb   
5) Each feature in the geodatabase is a distinct layer (e.g. species, cholorophyll, diversity)   
6) For survey data features rename the field of interest with the name of the feature   
7) Create 1km grid from NSB polygon (with 1km buffer)


GroupbyGrid.R
-------------
Aggregates by grid cell
* Calculates mean and sd per grid cell for survey data
* Calculates presence of points within grid cell for presence and polygon data


MapEBSAs.R
----------
Exports maps of species abundance/presence and richness/diveristy data aggregate by grid


OverlaybyEBSA.R
---------------
Adds an attribute to the gridded data that describes its position inside or outside each EBSA


GroupbyEBSA.R
--------------
Aggregates by in and outside each EBSA polgyon
* Calculates the mean, sd, percent of occurence and percent of missing data for survey data
* Caluclates the sum and percent of occurence for presence data


Summarise.R
-----------
Summarises survery and presence data metrics calculated in GroupbyEBSA.R 
* Only includes species listed as important for the EBSAs in Clark & Jamieson 2006 Phase II
* Exports 2 summary tables: species abundance and presence and diveristy/richness


ProductivitybyEBSA.R
--------------------
Summarises the productivity raster layers by EBSAs
* Adds an attribute to the chlorophyll data that describes its position inside or outside each EBSA
* Calculates the mean and stdev for mean chla and bloom frequencyfor each EBSA


MapProductivity.R
-----------------
Exports maps of mean chla and bloom frequencywith EBSA boundaries


BoxPlots.R
----------
Creates boxplot figures for species abundance, diveristy, richness and productivity 
for each EBSA
* Only includes species listed as important for the EBSAs in Clark & Jamieson 2006 Phase II



