import imp
from xml.dom.minidom import TypeInfo
from osgeo import gdal
import fiona
import os
import numpy as np
import subprocess
import sys
import glob
from pathlib import Path
import os
import datetime
from sentinelhub import (
    MimeType,
    CRS,
    BBox,
    SentinelHubRequest,
    SentinelHubDownloadClient,
    DataCollection,
    bbox_to_dimensions,
    DownloadRequest,
    MosaickingOrder,
)
import geopandas
import pandas as pd
from shapely.geometry import Polygon
from fiona.crs import from_epsg
import shutil


# set working directory
os.chdir('C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/environmental_data/')


# calculate slope and aspect
cmd1 = "gdaldem slope dem1.tif slope1.tif -compute_edges"
cmd2 = "gdaldem aspect dem1.tif aspect1.tif -compute_edges"

os.system(cmd1)
os.system(cmd2)

# calculate dom (why?)
#subprocess.call([sys.executable, 'C:\Program Files\gdal\gdal_calc.py', '-A', 'dem1.tif', '-B', 'ndom1.tif', '--outfile=dom4.tif', '--calc=A+B'])


# calculate dom
dem = gdal.Open("dem1.tif")
ndom = gdal.Open("ndom1.tif")

dem_array = dem.GetRasterBand(1).ReadAsArray()
ndom_array = ndom.GetRasterBand(1).ReadAsArray()

dom_array = dem_array + ndom_array

driver_tiff = gdal.GetDriverByName("GTiff")
ds_add = driver_tiff.CreateCopy("dom1.tif", "dem_1.tif", strict=0)

# rasterize polygons
dir_shp = "C:/0_Msc_Loek/M7_Fernerkundung/shapes"
os.chdir(dir_shp)    

for file in glob.glob('*.shp'):
    input_shp = ogr.Open(file)
    shp_layer = input_shp.GetLayer()
    pixel_size = 5
    xmin, xmax, ymin, ymax = shp_layer.GetExtent()
    ds = gdal.Rasterize(file[:-4]+'.tif', file, xRes=pixel_size, yRes=pixel_size, 
                    burnValues=255, outputBounds=[xmin, ymin, xmax, ymax], 
                    outputType=gdal.GDT_Byte)   


# calculate distance from rivers etc.   
cmd3 = "python gdal_proximity.py strassen.tif streets_ds.tif -distunits GEO"
cmd4 = "python gdal_proximity.py siedlungen.tif settlements_ds.tif -distunits GEO"
cmd5 = "python gdal_proximity.py gewaesser.tif rivers_ds.tif -distunits GEO"

for task in [cmd3,cmd4,cmd5]:
    os.system(task)


# clip distance rasters to study region
cmd6 = "gdalwarp -of GTiff -cutline nrw.shp -crop_to_cutline streets_ds.tif C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/environmental_data/streets_dist.tif"
cmd7 = "gdalwarp -of GTiff -cutline nrw.shp -crop_to_cutline settlements_ds.tif C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/environmental_data/settlements_dist.tif"
cmd8 = "gdalwarp -of GTiff -cutline nrw.shp -crop_to_cutline rivers_ds.tif C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/environmental_data/rivers_dist.tif"

for task in [cmd6,cmd7,cmd8]:
    os.system(task)


# move rasterized but not distance raster files to destination folder 
fnames = ["acker.tif","bahn_5.tif","wald.tif"]

for file in fnames:
    path_now = Path(file).resolve()
    path_then = "C:/0_Msc_Loek/M7_Fernerkundung/data_sdm_nrw/data/environmental_data/"+file
    os.rename(path_now, path_then)
          

# download ndvi data
# create bbox of the study area
nrw = geopandas.read_file("C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw.shp")
nrw = nrw.to_crs(epsg=4326)
nrw.to_file("C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw_4326.shp")

nrw_4326 = fiona.open("C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw_4326.shp")
nrw_bb = nrw_4326.bounds
coords = [(nrw_bb[0], nrw_bb[1]), (nrw_bb[0], nrw_bb[3]), (nrw_bb[2], nrw_bb[3]), (nrw_bb[2], nrw_bb[1])]
poly = Polygon(coords)

nrw_gdf = geopandas.GeoDataFrame()
nrw_gdf.loc[0,'geometry'] = poly
nrw_gdf.crs = from_epsg(4326)

os.mkdir("nrw_bbf")
nrw_gdf.to_file("C:/0_Msc_Loek/M7_Fernerkundung/shapes/nrw_bbf/nrw_bb.shp")
shutil.make_archive("nrw_bbz", 'zip', "nrw_bbf")

