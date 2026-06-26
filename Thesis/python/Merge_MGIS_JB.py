# Databricks notebook source
# MAGIC %pip install geopandas pyreadr shapely pyarrow
# MAGIC import geopandas as gpd
# MAGIC import pandas as pd
# MAGIC import pyreadr
# MAGIC
# MAGIC # load data
# MAGIC datGis = pyreadr.read_r( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/dat_prepared_MGIS.rds" )[None]
# MAGIC datJord = pyreadr.read_r( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/Jordboniteter.rds" )[None]
# MAGIC
# MAGIC # konvertér til GeoDataFrames
# MAGIC datGis = gpd.GeoDataFrame(
# MAGIC     datGis,
# MAGIC     geometry=gpd.GeoSeries.from_wkt( datGis[ "geometry" ] ),
# MAGIC     crs = "EPSG:25832"
# MAGIC )
# MAGIC datJord = gpd.GeoDataFrame(
# MAGIC     datJord,
# MAGIC     geometry=gpd.GeoSeries.from_wkt( datJord[ "geometry" ] ),
# MAGIC     crs = "EPSG:25832"
# MAGIC )
# MAGIC
# MAGIC # reducér antallet af polygoner i jordbonitetsdataen
# MAGIC bbox = datGis.total_bounds  # xmin, ymin, xmax, ymax
# MAGIC datJord_small = datJord.cx[ bbox[0]:bbox[2], bbox[1]:bbox[3] ]
# MAGIC
# MAGIC # merge GIS og JB datasæt via polygoner
# MAGIC datGisJord = gpd.overlay( datGis, datJord_small, how = "intersection" )
# MAGIC
# MAGIC # tilføj intersektionsområde
# MAGIC datGisJord[ "area_intersect" ] = datGisJord.geometry.area
# MAGIC
# MAGIC # behold kun jordtyper med det største areal per mark 
# MAGIC dominant = (
# MAGIC     datGisJord
# MAGIC     .sort_values( "area_intersect", ascending = False )
# MAGIC     .groupby( "marknr", as_index = False )
# MAGIC     .first()
# MAGIC )
# MAGIC dominant = dominant.drop( columns = [ "geometry", "area_intersect" ] )
# MAGIC
# MAGIC # Merge tilbage til original datGis for få originale geometrier
# MAGIC datGis_enriched_save = datGis.merge(
# MAGIC     dominant[ [ "marknr", "jb_kode", "jordtype" ] ],
# MAGIC     on = "marknr",
# MAGIC     how = "left"
# MAGIC )
# MAGIC
# MAGIC # gem merged datasæt som parquet (dvs. med geomtri som wkt string)
# MAGIC datGis_enriched_save[ "geometry" ] = datGis_enriched_save[ "geometry" ].astype( str )
# MAGIC
# MAGIC datGis_enriched_save.to_parquet( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/dat_MGIS_JB.parquet" )
# MAGIC