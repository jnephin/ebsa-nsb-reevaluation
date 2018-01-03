# Name: Raster2Poly.py
# Author: Jessica Nephin

# Description: Converts raster layers to polygon features
# Notes: Run from parent directory

# Import modules
import os
import arcpy

# Check out the ArcGIS Spatial Analyst extension license
arcpy.CheckOutExtension("Spatial")


# Set environment settings
rasterpath = os.getcwd()+"/Data/Productivity"
gdbpath = os.getcwd()+"/Data/Productivity.gdb/"

# ------------------------------------#
# loop through rasters
rasters = ["Chla_mean_nsb.tif","Bloom_freq_nsb.tif","Uncertainty_nsb.tif"]
for r in rasters:

    # Input
    ras = rasterpath+"/"+r

    # Multiply raster
    tmpRas = arcpy.sa.Raster(ras) * 100000

    # Convert to Integer
    intRas = arcpy.sa.Int(tmpRas)

    # Build raster attributes
    arcpy.BuildRasterAttributeTable_management (intRas, "OVERWRITE")

    # Execute PolygonToRaster
    arcpy.RasterToPolygon_conversion (intRas, out, "NO_SIMPLIFY", "VALUE")

    # Create geodatabase
    arcpy.CreateFileGDB_management (gdbpath, r)

    # Add new attribute and divide value by 100000
    arcpy.AddField_management(out, r, "DOUBLE", 18, 11)
    arcpy.CalculateField_management (out, r, "!GRIDCODE! / 100000 ", "PYTHON_9.3")
