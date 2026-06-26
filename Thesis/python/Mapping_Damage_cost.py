# Databricks notebook source
# MAGIC %pip install pyreadr
# MAGIC %pip install pyreadr geopandas
# MAGIC %pip install contextily
# MAGIC %pip install pyreadr geopandas contextily folium

# COMMAND ----------

dbutils.library.restartPython()

# COMMAND ----------

import pyreadr
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from shapely import wkt
import contextily as ctx
import folium
from branca.colormap import LinearColormap

# COMMAND ----------

# MAGIC %md
# MAGIC # Generisk Referencekorts-analyse

# COMMAND ----------

import shutil
import geopandas as gpd

shutil.copy(
    "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/datMGIS6.gpkg",
    "/local_disk0/datMGIS6.gpkg"
)

dat6 = gpd.read_file("/local_disk0/datMGIS6.gpkg")

# COMMAND ----------

import geopandas as gpd
import shutil
import sqlite3

# Copy gpkg to a writable local path (SQLite requires write access even for reads)
src = "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/datMGIS6.gpkg"
dst = "/local_disk0/datMGIS6.gpkg"
shutil.copy(src, dst)

# Fix GPKG application_id (file has 0x00000000 instead of proper GPKG id)
conn = sqlite3.connect(dst)
conn.execute("PRAGMA application_id = 1196444487")  # 0x47504B47 = 'GPKG'
conn.close()

dat6 = gpd.read_file(dst)

# COMMAND ----------

# MAGIC %md
# MAGIC # Følgende er analyse for de specifikke CVR (marker) som vi har data for 

# COMMAND ----------

# indlæs data
gdat_local   = pyreadr.read_r("/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/dat_prepared_MGIS.rds")[None]
dat_ref      = pyreadr.read_r("/Volumes/ledelseoekonomi_integration_prod/ifro/output/results/datResults_conv_crops_refMap.rds")[None]
dat_ref1 = pyreadr.read_r("/Volumes/ledelseoekonomi_integration_prod/ifro/output/results/datResults_conv_crops_refMap1.rds")[None]

# COMMAND ----------

# beregn damage cost kr/ha
dat_ref1["damage_cost_kr_ha"] = dat_ref1["Uniform_D_Cost_mark_Vinterhvede"] / dat_ref1["imk_areal"]

# filtrer til kun relevante catchments
code_catchment = ["111", "232", "131", "236", "235", "136", "128", "93", "165", "35", "2"]
dat_ref1_filtered = dat_ref1[dat_ref1["KystvandID"].isin(code_catchment)].copy()

# inner join på cvr + marknr
merged = dat_ref1_filtered.merge(
    gdat_local[["cvr", "marknr", "geometry"]],
    on=["cvr", "marknr"],
    how="inner"
)

print(f"merged efter inner join: {len(merged)}")

# tjek overlap
print(f"dat_ref1 efter catchment-filter: {len(dat_ref1_filtered)}")
print(f"merged efter inner join: {len(merged)}")

# konverter WKT til geometri og lav GeoDataFrame
merged = merged.dropna(subset=["geometry"])
merged["geometry"] = merged["geometry"].apply(wkt.loads)
gdf = gpd.GeoDataFrame(merged, geometry="geometry", crs="EPSG:25832")

print(f"Antal marker til plot: {len(gdf)}")
print(f"Memory: {gdf.memory_usage(deep=True).sum() / 1e9:.2f} GB")

# COMMAND ----------



# konverter til WGS84 (folium bruger lat/lon)
gdf_wgs = gdf.to_crs(epsg=4326)

# lav farveskala
vmin = gdf_wgs["damage_cost_kr_ha"].quantile(0.05)
vmax = gdf_wgs["damage_cost_kr_ha"].quantile(0.95)
colormap = LinearColormap(
    colors=["yellow", "orange", "red", "darkred"],
    vmin=vmin,
    vmax=vmax,
    caption="Damage cost (kr/ha/y)"
)

# centrer kortet på data
center_lat = gdf_wgs.geometry.centroid.y.mean()
center_lon = gdf_wgs.geometry.centroid.x.mean()

m = folium.Map(location=[center_lat, center_lon], zoom_start=8)

# tilføj marker
folium.GeoJson(
    gdf_wgs[["marknr", "KystvandNa", "damage_cost_kr_ha", "geometry"]],
    style_function=lambda feature: {
        "fillColor": colormap(feature["properties"]["damage_cost_kr_ha"] or 0),
        "color": "black",
        "weight": 0.3,
        "fillOpacity": 0.8
    },
    tooltip=folium.GeoJsonTooltip(
        fields=["marknr", "KystvandNa", "damage_cost_kr_ha"],
        aliases=["Mark", "Kystvandopland", "Damage cost (kr/ha/y)"]
    )
).add_to(m)

colormap.add_to(m)
m

# COMMAND ----------

import folium
from branca.colormap import LinearColormap

# konverter til WGS84
gdf_wgs = gdf.to_crs(epsg=4326)

# lav farveskala
vmin = gdf_wgs["damage_cost_kr_ha"].quantile(0.05)
vmax = gdf_wgs["damage_cost_kr_ha"].quantile(0.95)
colormap = LinearColormap(
    colors=["yellow", "orange", "red", "darkred"],
    vmin=vmin,
    vmax=vmax,
    caption="Damage cost (kr/ha/y)"
)

# centrer kortet
center_lat = gdf_wgs.geometry.centroid.y.mean()
center_lon = gdf_wgs.geometry.centroid.x.mean()

# grå baggrundskort
m = folium.Map(
    location=[center_lat, center_lon], 
    zoom_start=8,
    tiles="CartoDB positron"
)

# tilføj marker med farve efter damage cost
folium.GeoJson(
    gdf_wgs[["cvr", "marknr", "KystvandNa", "damage_cost_kr_ha", "geometry"]],
    style_function=lambda feature: {
        "fillColor": colormap(feature["properties"]["damage_cost_kr_ha"] or 0),
        "color": "none",
        "weight": 0,
        "fillOpacity": 0.85
    },
    tooltip=folium.GeoJsonTooltip(
        fields=["marknr", "KystvandNa", "damage_cost_kr_ha"],
        aliases=["Mark", "Kystvandopland", "Damage cost (kr/ha/y)"]
    )
).add_to(m)

colormap.add_to(m)
m