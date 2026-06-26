# Databricks notebook source
# indlæs R pakker - basale
install.packages( "readxl" )
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "stringr" )
library( "ggplot2" )
library( "readxl" )

# indlæs R pakker - spatial
system( "sudo apt-get update -qq && sudo apt-get install -y -qq libudunits2-dev libgdal-dev libgeos-dev # libproj-dev && sudo ldconfig", intern = TRUE )
install.packages( c( "Rcpp", "units" ), repos = "https://cloud.r-project.org" )
install.packages( "sf" )
library( "sf" )
install.packages( "terra" )
library( "terra" )



# COMMAND ----------

# MAGIC %md
# MAGIC # Generisk Referencekort for alle marker i vandoplande med Uniform/specific damage cost estimater 

# COMMAND ----------

sum( dat6$imk_areal )

# COMMAND ----------

# loader relevante datasæt #### OBS vi mangler jorbundskortet her!! ###

# definér stivej 
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro"

# indlæs miljoegis datasæt via Spark
ab <- spark_connect(method = "databricks")
gdat <- tbl(ab, "ledelseoekonomi_integration_prod.ifro.miljoegis_mark")

# indlæs raster kort
datRet   <- rast(file.path(dirPath, "input/retention/TotalRetention_regioner.tif"))
datHvede <- rast(file.path(dirPath, "input/IfroVisa180_j.tif"))

# indlæs kystvandoplande og filtrer til de 6 relevante
datKyst  <- st_read(file.path(dirPath, "input/Kystvandoplande_VP3_2024/Kystvandoplande_VP3_2024.shp"))
datKyst6 <- datKyst[datKyst$KystvandID %in% c("2", "136", "128", "93", "165", "131"), ]

# indlæs damage costs
costNames <- c("D_Cost_2024", "ENA_lower_2024", "ENA_upper_2024", "ENA_mean_2024")
datCosts  <- read_excel(file.path(dirPath, "output/uploaded/Damage cost estimates.xlsx"))
datCosts  <- datCosts[, c("Kystvand_ID", costNames)]

# COMMAND ----------

#### Forbered Mark Data ####
# hent gdat som lokal dataframe
gdat <- sdf_collect(gdat) %>%
  st_as_sf(wkt = "geometry", crs = 25832)

# sørg for samme CRS
datKyst6 <- st_transform(datKyst6, crs = 25832)

# lav GIS datasæt til 'sf' dataframe for at merge med kystvandoplande
gdat <- st_as_sf( gdat, wkt = "geometry", crs = st_crs( datKyst6 ) )

# merge GIS datasæt og kystvandoplande via geometri og behold kun obs i 6 relevante vandoplande
dat6 <- st_intersection( gdat, datKyst6[, c( "KystvandID", "KystvandNa" ) ] )



# trimmer retentionskortet så det kun dækker relevante kystvandsoplande marker
datRet <- crop( datRet, vect( dat6 ) ) 
datHvede <- crop( datHvede, vect( dat6 ) )


# ekstrahere og tilføj retentionsdataen for hver mark til GIS datasæt
dat6$retention <- extract( datRet, vect( dat6 ), fun = mean, na.rm = TRUE )[,2]
dat6$udv_hvede <- extract( datHvede, vect( dat6 ), fun = mean, na.rm = TRUE )[,2]
# dat6$udv_byg <- extract( datByg, vect( datFields ), fun = mean, na.rm = TRUE )[,2]

# Tilføjer og behandler Damage costs

library(dplyr)

dat6 <- dat6 %>%
  left_join(datCosts, by = c("KystvandID" = "Kystvand_ID")) %>%
  dplyr::mutate(
    ENA_mean_2024 = as.numeric(ENA_mean_2024),
    D_Cost_2024   = as.numeric(D_Cost_2024),

    # N udvaskning markniveau
    udv_Vinterhvede_N_kg_mark    = imk_areal * udv_hvede,
    kystvand_N_kg_Vinterhvede    = udv_Vinterhvede_N_kg_mark * (1 - retention/100),

    # Damage costs per mark
    Uniform_D_Cost  = kystvand_N_kg_Vinterhvede * ENA_mean_2024,
    Specific_D_Cost = kystvand_N_kg_Vinterhvede * D_Cost_2024,

    # Damage costs per hektar
    Uniform_D_Cost_ha    = Uniform_D_Cost / imk_areal,
    Specific_D_Cost_ha   = Specific_D_Cost / imk_areal
  )

cat("Rækker:", nrow(dat6), "\n")
summary(dat6$Uniform_D_Cost)

#trimmer datasæt til kun relevante variable:
dat6 <- dat6 %>%
  select(
    marknr,
    imk_areal,
    afgkode,
    cvr,
    KystvandNa,
    retention,
    D_Cost_2024,
    udv_Vinterhvede_N_kg_mark,
    kystvand_N_kg_Vinterhvede,
    Uniform_D_Cost,
    Specific_D_Cost,
    Uniform_D_Cost_ha,
    Specific_D_Cost_ha,
    KystvandID,
    geometry,
    ENA_lower_2024,
    ENA_upper_2024,
    ENA_mean_2024
  )

# gem
saveRDS(dat6, file.path(dirPath, "output/dat_prepared/datMGIS6.rds"))

# COMMAND ----------

st_write(dat6, "/tmp/datMGIS6.gpkg", delete_dsn = TRUE)
file.copy("/tmp/datMGIS6.gpkg", file.path(dirPath, "output/dat_prepared/datMGIS6.gpkg"), overwrite = TRUE)

# COMMAND ----------

file.exists("/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/datMGIS6.gpkg")

# COMMAND ----------

names(dat6)

# COMMAND ----------

hist_vinterhvede <- ggplot(dat6, aes(x = Specific_D_Cost_ha)) +
  geom_histogram(binwidth = 100, color = "black", fill = "steelblue") +
  labs(
    x = "Specific damage cost (kr/ha)",
    y = "Antal marker",
    title = "Fordeling af specifik skadeomkostning pr. ha vinterhvede"
  )

print(hist_vinterhvede)

# COMMAND ----------

hist_vinterhvede <- ggplot(dat6, aes(x = Uniform_D_Cost_ha)) +
  geom_histogram(binwidth = 100, color = "black", fill = "steelblue") +
  labs(
    x = "Uniform damage cost (kr/ha)",
    y = "Antal marker",
    title = "Fordeling af Uniform skadeomkostning pr. ha vinterhvede"
  )

print(hist_vinterhvede)

# COMMAND ----------

table_kystvand <- dat6 %>%
  st_drop_geometry() %>%
  group_by(KystvandNa) %>%
  summarise(
    mean_Specific_D_Cost_ha = mean(Specific_D_Cost_ha, na.rm = TRUE),
    mean_Uniform_D_Cost_ha  = mean(Uniform_D_Cost_ha, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

print(table_kystvand)

# COMMAND ----------

# MAGIC %md
# MAGIC # referencekort kun for CVR i datasæt for konventionelle planteproducenter

# COMMAND ----------

# stiveje
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# vælg model specifikation
obs <- "conv_crops"

# indlæs klargjorte farm-level datasæt
datFarms <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, ".rds" ) ) ) 

# indlæs klarggjort field-level datasæt
datFields <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, "_fields.rds" ) ) ) 

# behold kun observationer i farm-level datasættet som også er i field-level datasættet
datFarms <- datFarms %>% semi_join( datFields, by = "cvr" ) 

dat_ref_Vinterhvede <- readRDS(file.path( dirPath, paste0( "results/datResults_", obs, "_refMap.rds" ) ) )


# COMMAND ----------

display(dat_ref_Vinterhvede1)

# COMMAND ----------

dat_ref_Vinterhvede %>%
summarise(n = n_distinct(marknr))

# COMMAND ----------

# udvælger kun relevante marker i relevante kystvandoplande:
dat_ref_Vinterhvede <- dat_ref_Vinterhvede %>%
  filter(KystvandID %in% c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)) 
  
  
  # Vfilter(crop_kat %in% c( "Byg", "Hvede", "Havre_blandet_korn", "Rug_Triticale", "Frø", "Kartofler", "Raps",
  #                "Ærter_mv", "Fodermajs" ))
  

# COMMAND ----------

# Beregner damage cost Uniform og Specific for hver mark

dat_ref_Vinterhvede1 <- dat_ref_Vinterhvede %>% mutate(
  ENA_mean_2024 = as.numeric(ENA_mean_2024),
  D_Cost_2024   = as.numeric(D_Cost_2024),

  # N udvaskning markniveau
  # Vinterhvede
  udv_Vinterhvede_N_kg_mark    = imk_areal * udv_hvede,
  kystvand_N_kg_Vinterhvede    = udv_Vinterhvede_N_kg_mark * (1 - retention/100),

  #Vårbyg
  udv_Vårbyg_N_kg_mark    = imk_areal * udv_byg,
  kystvand_N_kg_Vårbyg    = udv_Vårbyg_N_kg_mark * (1 - retention/100),

  # Damage costs
  # Vinterhvede
  Uniform_D_Cost_mark_Vinterhvede  = kystvand_N_kg_Vinterhvede * ENA_mean_2024,
  Specific_D_Cost_mark_Vinterhvede = kystvand_N_kg_Vinterhvede * D_Cost_2024,

  # Vårbyg
  Uniform_D_Cost_mark_Vårbyg  = kystvand_N_kg_Vårbyg * ENA_mean_2024,
  Specific_D_Cost_mark_Vårbyg = kystvand_N_kg_Vårbyg * D_Cost_2024,

)

# tilskriver marker, der ligger i to vandoplande til det vandopland hvor cvr har størst areal

# trin 1: find dominant kystvandopland per CVR (flest marker) # bør være størts samlet markareal?
cvr_dominant <- dat_ref_Vinterhvede1 %>%
  group_by(cvr, KystvandID) %>%
  summarise(n_marker = n(), .groups = "drop") %>%
  group_by(cvr) %>%
  slice_max(n_marker, n = 1, with_ties = FALSE) %>%
  select(cvr, dominant_kystvand = KystvandID)

# trin 2: tilskriv alle marker til dominant kystvandopland # bør oprettes ii variablen KystvandNA1
dat_ref_Vinterhvede1 <- dat_ref_Vinterhvede1 %>%
  left_join(cvr_dominant, by = "cvr") %>%
  group_by(marknr) %>%
  slice(which(KystvandID == dominant_kystvand)[1]) %>%
  ungroup()







# COMMAND ----------

# tjek
cat("Rækker:", nrow(dat_ref_Vinterhvede1), "\n")
cat("Unikke marknr:", n_distinct(dat_ref_Vinterhvede1$marknr), "\n")

# COMMAND ----------

# gemmeropdateret referencekort

### gemmer reference kort 
#saveRDS( dat_ref_Vinterhvede1, file.path( dirPath, paste0( "results/datResults_", obs, "_refMap1.rds" ) ) )

# COMMAND ----------

cat("Antal rækker:", nrow(dat_ref_Vinterhvede1), "\n")
cat("Unikke marknr:", n_distinct(dat_ref_Vinterhvede1$marknr), "\n")
cat("Dubletter:", nrow(dat_ref_Vinterhvede1) - n_distinct(dat_ref_Vinterhvede1$marknr), "\n")

# vis hvad der skaber dubletter
dat_ref_Vinterhvede1 %>%
  group_by(marknr) %>%
  filter(n() > 1) %>%
  select(marknr, KystvandID, jb3_kat) %>%
  arrange(marknr) %>%
  head(20)

# COMMAND ----------

# MAGIC %md
# MAGIC # Histogrammer over fordeling af **SPECIFIC** damage costs kr/ha/y

# COMMAND ----------

# tabel over gennemsnitlige damage costs i kr/ha/y


table_kystvand <- plot_data %>%
  group_by(KystvandNa) %>%
  summarise(
    mean_vinterhvede = mean(damage_cost_kr_ha_hvede, na.rm = TRUE),
    
    mean_ler = mean(damage_cost_kr_ha[jb3_kat == "ler"], na.rm = TRUE),
    mean_sand_fin = mean(damage_cost_kr_ha[jb3_kat == "sand_fin"], na.rm = TRUE),
    mean_sand_grov = mean(damage_cost_kr_ha[jb3_kat == "sand_grov"], na.rm = TRUE),

    mean_vårbyg = mean(damage_cost_kr_ha_byg, na.rm = TRUE),
    
    n = n()
  ) %>%
  ungroup()

  print(table_kystvand)

# COMMAND ----------

### histogram over Damage cost i kr/ha/y for vinterhvedereferencekort i relevante kystvandsoplande

plot_data <- dat_ref_Vinterhvede1 %>%
  mutate(
    damage_cost_kr_ha = Specific_D_Cost_mark_Vinterhvede / imk_areal
  )

hist_vinterhvede <- ggplot(plot_data, aes(x = damage_cost_kr_ha)) +
  geom_histogram(binwidth = 100, color = "black", fill = "steelblue") +
  labs(
    x = "Specific damage cost (kr/ha)",
    y = "Antal marker",
    title = "Fordeling af specifik skadeomkostning pr. ha vinterhvede"
  )



# vis begge outputs

print(hist_vinterhvede)

# COMMAND ----------

# Hvedeplot Specific damage cost per CVR


plot_data <- dat_ref_Vinterhvede1 %>%
  group_by(cvr) %>%
  summarise(
    total_Specific_D_Cost_mark =
      sum(Specific_D_Cost_mark_Vinterhvede, na.rm = TRUE)
  ) %>%
  ungroup()

# Intervaller á 50.000 kr
max_val <- max(
  plot_data$total_Specific_D_Cost_mark,
  na.rm = TRUE
)

breaks_seq <- seq(
  0,
  ceiling(max_val / 50000) * 50000,
  by = 50000
)

hist_DC_cvr_vinterhvede <- ggplot(
  plot_data,
  aes(x = total_Specific_D_Cost_mark)
) +
  geom_histogram(
    breaks = breaks_seq,
    color = "black",
    fill = "steelblue"
  ) +
  scale_x_continuous(
    breaks = breaks_seq,
    labels = function(x) paste0(x / 1000, " t.kr.")
  ) +
  labs(
    x = "Total Specific Damage Cost pr. CVR",
    y = "Antal CVR",
    title = "Fordeling af skadeomkostninger pr. CVR"
  ) +
  theme_minimal()

print(hist_DC_cvr_vinterhvede)

# COMMAND ----------

# MAGIC %md # Histogrammer over fordeling af **UNIFORM** damage costs kr/ha/y
# MAGIC

# COMMAND ----------

plot_data %>%
  mutate(damage_cost_kr_ha = Uniform_D_Cost_mark_Vinterhvede / imk_areal) %>%
  group_by(KystvandID, jb3_kat) %>%
  summarise(
    mean_udv       = mean(udv_hvede,        na.rm = TRUE),
    mean_retention = mean(retention,         na.rm = TRUE),
    mean_cost      = mean(damage_cost_kr_ha, na.rm = TRUE),
    n = n()
  )

# COMMAND ----------

### histogram over Damage cost i kr/ha/y for alle marker i vinterhvedereferencekort - alle kystvandsoplande

plot_data <- dat_ref_Vinterhvede1 %>%
  mutate(
    damage_cost_kr_ha = Uniform_D_Cost_mark_Vinterhvede / imk_areal
  )

hist_vinterhvede <- ggplot(plot_data, aes(x = damage_cost_kr_ha)) +
  geom_histogram(binwidth = 100, color = "black", fill = "steelblue") +
  labs(
    x = "Uniform damage cost (kr/ha)",
    y = "Antal marker",
    title = "Fordeling af specifik skadeomkostning pr. ha vinterhvede"
  )

# tabel over gennemsnitlige damage costs i kr/ha/y


table_kystvand <- plot_data %>%
  group_by(KystvandNa) %>%
  summarise(
    mean_all = mean(damage_cost_kr_ha, na.rm = TRUE),
    
    mean_ler = mean(damage_cost_kr_ha[jb3_kat == "ler"], na.rm = TRUE),
    mean_sand_fin = mean(damage_cost_kr_ha[jb3_kat == "sand_fin"], na.rm = TRUE),
    mean_sand_grov = mean(damage_cost_kr_ha[jb3_kat == "sand_grov"], na.rm = TRUE),
    
    n = n()
  ) %>%
  ungroup()

# vis begge outputs
print(table_kystvand)
print(hist_vinterhvede)

# COMMAND ----------

# Hvedeplot Uniform damage cost per CVR
# kun for vinterhvedemarker

plot_data <- dat_ref_Vinterhvede1 %>%
  group_by(cvr) %>%
  summarise(
    total_Uniform_D_Cost_mark =
      sum(Uniform_D_Cost_mark_Vinterhvede, na.rm = TRUE)
  ) %>%
  ungroup()

# Intervaller á 300.000 kr
max_val <- max(plot_data$total_Uniform_D_Cost_mark, na.rm = TRUE)

breaks_seq <- seq(
  0,
  ceiling(max_val / 300000) * 300000,
  by = 300000
)

hist_DC_cvr_vinterhvede_uniform <- ggplot(
  plot_data,
  aes(x = total_Uniform_D_Cost_mark)
) +
  geom_histogram(
    breaks = breaks_seq,
    color = "black",
    fill = "steelblue"
  ) +
  scale_x_continuous(
    breaks = breaks_seq,
    labels = function(x) paste0(x / 1000, " t.kr.")
  ) +
  labs(
    x = "Total Uniform Damage Cost pr. CVR",
    y = "Antal CVR",
    title = "Fordeling af skadeomkostninger pr. CVR"
  ) +
  theme_minimal()

print(hist_DC_cvr_vinterhvede_uniform)

# COMMAND ----------

# MAGIC %md
# MAGIC # Beregner DB II fra samfundsperspektiv 

# COMMAND ----------

dat_ref_Vinterhvede <- dat_ref_Vinterhvede1 %>% 
  mutate(
    Social_DBII_Uniform = 
      (DBII_ref_Vinterhvede*imk_areal) - Uniform_D_Cost_mark_Vinterhvede,
   
  )

# COMMAND ----------

### Histogram over Social DBII på markniveau
### vinterhvedemarker

# Beregn Social DBII
plot_data <- dat_ref_Vinterhvede1 %>%
  mutate(
    Social_DBII_Uniform =
      (DBII_ref_Vinterhvede * imk_areal) -
      Uniform_D_Cost_mark_Vinterhvede
  )

# Find symmetrisk akse omkring 0
max_abs <- max(
  abs(plot_data$Social_DBII_Uniform),
  na.rm = TRUE
)

# Afrund til nærmeste 100.000
axis_limit <- ceiling(max_abs / 100000) * 100000

breaks_seq <- seq(
  -axis_limit,
   axis_limit,
   by = 100000
)

# Histogram
hist_social_dbii <- ggplot(
  plot_data,
  aes(x = Social_DBII_Uniform)
) +
  geom_histogram(
    breaks = breaks_seq,
    color = "black",
    fill = "steelblue"
  ) +
  scale_x_continuous(
    breaks = breaks_seq,
    labels = function(x) paste0(x/1000 , " t.kr.")
  ) +
  labs(
    x = "Social DBII pr. mark",
    y = "Antal marker",
    title = "Fordeling af Social DBII for vinterhvedemarker"
  ) +
  theme_minimal()

print(hist_social_dbii)

# COMMAND ----------

### Histogram over private DBII på markniveau
### vinterhvedemarker

# Beregn private DBII
plot_data <- dat_ref_Vinterhvede %>%
  mutate(
    DBII_private =
      DBII_ref_Vinterhvede * imk_areal
  )

# Find symmetrisk akse omkring 0
max_abs <- max(
  abs(plot_data$DBII_private),
  na.rm = TRUE
)

# Afrund til nærmeste 10.000
axis_limit <- ceiling(max_abs / 100000) * 100000

breaks_seq <- seq(
  -axis_limit,
   axis_limit,
   by = 100000
)

# Histogram
hist_dbii_pri <- ggplot(
  plot_data,
  aes(x = DBII_private)
) +
  geom_histogram(
    breaks = breaks_seq,
    color = "black",
    fill = "orange"
  ) +
  scale_x_continuous(
    breaks = breaks_seq,
    labels = function(x) paste0(x/1000, "t.kr.")
  ) +
  labs(
    x = "Private DBII pr. mark",
    y = "Antal marker",
    title = "Fordeling af private DBII for vinterhvedemarkref"
  ) +
  theme_minimal()

print(hist_dbii_pri)

# COMMAND ----------

library(sf)

# join geometri fra datFields på dat_ref_Vinterhvede via marknr
dat_ref_Vinterhvede_sf <- dat_ref_Vinterhvede1 %>%
  mutate(damage_cost_kr_ha = Uniform_D_Cost_mark_Vinterhvede / imk_areal) %>%
  left_join(
    datFields %>% select(marknr, geometry),
    by = "marknr"
  ) %>%
  st_as_sf()

# plot kortet
ggplot(dat_ref_Vinterhvede_sf) +
  geom_sf(aes(fill = damage_cost_kr_ha), color = NA) +
  scale_fill_viridis_c(
    name = "Damage cost\n(kr/ha/y)",
    option = "magma",
    direction = -1
  ) +
  labs(
    title = "Samfundsomkostning pr. ha vinterhvede",
    subtitle = "Uniform damage cost baseret på N-udvaskning og ENA"
  ) +
  theme_minimal()

# COMMAND ----------

display(dat_ref_Vinterhvede)

# COMMAND ----------

datFields1<- datFields  %>% mutate ( 
    # normkvoter
N_Norm_VinterHv_kg_ha = case_when(

 jb_kode %in% c(1,3) ~ 171,
 jb_kode %in% c(2,4,10) ~ 178,
 jb_kode %in% c(5,6) ~ 206,
 jb_kode %in% c(7,8,9) ~ 219,
 jb_kode == 11 ~ 128,
TRUE ~ NA_real_
)
)


datFields1<- datFields1 %>% mutate ( 
    # totale N_kvote_mark

    N_kvote_mark = N_Norm_VinterHv_kg_ha * imk_areal)






# COMMAND ----------

