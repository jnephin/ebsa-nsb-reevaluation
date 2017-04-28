Re-evaluation of NSB EBSAs
==========================

Before running the scripts:
1) Obtain relevant biological datasets
2) Project into BC albers
3) Clip data to NSB boundary
4) Organise data into 3 geodatabase: SurveyData.gdb, PresenceData.gdb and PolygonData.gdb
5) Each feature in the geodatabase is a distinct layer (e.g. species, cholorophyll, diversity)
6) For survey data features rename the field of interest with the name of the feature
7) Create 1km grid from NSB polygon (with 1km buffer)


GroupbyGrid.R
-------------
Aggregates by grid cell
* Calculates mean and sd per grid cell for survey data
* Calculates presence of points within grid cell for presence and polygon data


