# Databricks notebook source
# MAGIC %pip install geopandas matplotlib shapely pyreadr
# MAGIC
# MAGIC import pandas as pd
# MAGIC import geopandas as gpd
# MAGIC from shapely.geometry import Polygon, MultiPolygon
# MAGIC from shapely import wkt
# MAGIC import matplotlib.pyplot as plt
# MAGIC import pyreadr

# COMMAND ----------

from shapely import wkt

# Tæl marker med gyldig WKT
def can_parse_wkt(wkt_str):
    try:
        wkt.loads(wkt_str)
        return True
    except:
        return False

mdat_pd['valid_wkt'] = mdat_pd['geometry'].apply(can_parse_wkt)

total_markers = len(mdat_pd)
num_valid = mdat_pd['valid_wkt'].sum()
num_invalid = total_markers - num_valid

print(f"Samlet antal marknr: {total_markers}")
print(f"Antal marker med gyldig geometri (kan parses med WKT): {num_valid}")
print(f"Antal marker med ugyldig geometri: {num_invalid}")

# Hvis du vil se ugyldige marknr
invalid_markers = mdat_pd.loc[~mdat_pd['valid_wkt'], 'marknr']
print("Marker med ugyldig geometri (kun hvis nogen):")
print(invalid_markers.to_list())

# COMMAND ----------


# 3️⃣ Læs R-datasæt
edat_res = pyreadr.read_r("/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared_SEGES.rds")
gdat_res = pyreadr.read_r("/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared_MGIS.rds")

edat_pd = edat_res[None]  # første objekt i rds
gdat_pd = gdat_res[None]

# 4️⃣ Merge på CVR → svarer til mdat
mdat_pd = pd.merge(edat_pd, gdat_pd, on="cvr", how="inner")

# 5️⃣ Sikker WKT-parser: tvinger Polygon til MultiPolygon, logger fejl
def safe_parse_wkt(row):
    geom_str = row['geometry']
    marknr = row['marknr']
    try:
        geom = wkt.loads(geom_str)
        # Hvis det er Polygon, lav til MultiPolygon
        if isinstance(geom, Polygon):
            geom = MultiPolygon([geom])
        return geom
    except Exception as e:
        print(f"FEJL i marknr {marknr}: {e}")
        return None

mdat_pd['geometry'] = mdat_pd.apply(safe_parse_wkt, axis=1)

# Drop marknr med ugyldig geometri
gdf_sample = gpd.GeoDataFrame(mdat_pd.dropna(subset=['geometry']), geometry='geometry', crs="EPSG:25832")

# 6️⃣ Tegn kort med alle marknr
fig, ax = plt.subplots(figsize=(12,12))
gdf_sample.plot(ax=ax, color='darkgreen', edgecolor='black', alpha=0.5)
ax.set_title("Kort over alle marknr i mdat")
plt.show()