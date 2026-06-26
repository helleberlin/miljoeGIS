# Databricks notebook source
# Udbyttemodel Test for Hvede

# indlæs R pakker - basale

library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
install.packages( "readxl" )
library( "readxl" )
library( "ggplot2" )
install.packages("fixest")
library(fixest)

# indlæs R pakker - spatial
system( "sudo apt-get update -qq && sudo apt-get install -y -qq libudunits2-dev libgdal-dev libgeos-dev # libproj-dev && sudo ldconfig", intern = TRUE )
install.packages( c( "Rcpp", "units" ), repos = "https://cloud.r-project.org" )
install.packages( "sf" )
library( "sf" )
install.packages( "terra" )
library( "terra" )


# stiveje
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# vælg model specifikation
obs <- "conv_crops" # "eco_crops" # "eco_mix" # "conv_mix" # "all"

# indlæs bedriftsniveau datasæt bestående af observationer fra estimeringen: henter datasæt med resultater for CVR’er hvor TYPEKODE = 4 (konventionelle planteproducenter)
datFarms <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, ".rds" ) ) )

# vælg observationer der skal bruges i estimeringen
if( obs == "all" ){
  datFarms <- datFarms
} else if( obs %in% c( "eco_crops", "eco_mix", "conv_mix", "conv_crops" ) ){
  datFarms <- subset( datFarms, TYPEKAT == obs )
} else {
  stop( "Ukendt model-specifikation. Brug obs = 'eco_crops', 'eco_mix', 'conv_mix', 'conv_crops', eller 'all'" )
}


# indlæs klarggjort markniveau datasæt
datFields <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_fields.rds" ) )

# behold kun observationer i markniveau datasættet der er brugt i estimeringen
datFields <- datFields %>% semi_join( datFarms, by = "cvr" )

# dropper geometrien
datFields <- sf::st_drop_geometry(datFields)

# COMMAND ----------

# Opretter datasæt for hvede: Vinterhvede (11), vinterhvede brødhvede (13), Vårhvede (2), Vårhvede, brødhvede (6) (helsædshvede er ikke inkluderet!)

dat_hvede <- datFields %>%
  filter(afgkode %in% c(2, 6, 11, 13)) %>% # hvedetyper
  mutate(
    hvede_type = case_when(
      afgkode %in% c(2, 6) ~ "Vårhvede",
      afgkode %in% c(11, 13) ~ "vinterhvede"
      
    )
  )

# Tilknytter U_2 (udbytte hvede)
dat_hvede <- dat_hvede %>%
  left_join(
    datFarms %>% select(cvr, U_2),
    by = "cvr"
  )

# Identificerer bedrifter uden output (U_2 = 0) # 4 CVR ud af 289 cvr med hvedemarker
CVR_U2_0 <- dat_hvede %>%
  filter(U_2 == 0 | is.na(U_2)) %>%
  distinct(cvr) %>%
  pull(cvr)

# tjekker CVR
# CVR_U2_0



# Fjerner disse bedrifter fra datasættet
dat_hvede <- dat_hvede %>%
  filter(!cvr %in% CVR_U2_0)

n_distinct(dat_hvede$cvr)

# COMMAND ----------

# Her ses jordtypefordeling i hvede-data
jb_counts <- dat_hvede %>%
  count(jb_kode, name = "n_obs") %>%
  tidyr::complete(jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(jb_kode)

jb_counts

# Konstruerer 3 jorbonitetsklasser ud fra Jb_kode # NB fordi "JB_7_og_8", "JB_11" er så små at de giver problemer i estimeringen!

dat_hvede <- dat_hvede %>%
  mutate(
    jb3 = case_when(
      jb_kat == "JB_1_og_3" ~ "sand_let",
      jb_kat == "JB_2_og_4" ~ "sand_grov",
      jb_kat %in% c("JB_5_og_6", "JB_7_og_8", "JB_11") ~ "ler",
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------
# (valgfrit men anbefalet) Tjek for missing
# -------------------------------------------------

table(dat_hvede$jb3, useNA = "ifany")

table(dat_hvede$jb_kode, dat_hvede$jb_kat, useNA = "ifany")

# COMMAND ----------

# overblik over de marker, der ligger indenfor de 11 vandoplande **HVEDE** 212 marker ligger uden for de 11 kystvandsoplande 1721 ligger inden for

library(tidyr)

ids_to_keep <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

jb_kyst_hvede <- datFields %>%
  filter(KystvandID %in% ids_to_keep) %>%   
  count(KystvandID, jb_kode, name = "n_obs") %>%
  complete(KystvandID, jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(KystvandID, jb_kode)

jb_kyst_hvede_wide <- jb_kyst_hvede %>%
  pivot_wider(
    names_from = jb_kode,
    values_from = n_obs,
    names_prefix = "JB_"
  )

print(jb_kyst_hvede_wide, n = Inf, width = Inf)

n_obs_hvede <- dat_hvede %>%
  filter(KystvandID %in% ids_to_keep) %>%
  summarise(n_obs = n())

n_obs_hvede

# COMMAND ----------



# -------------------------------------------------
# 1. Kollapser markdata til farm × afgrøde × jordtype
# -> én række pr. cvr × afgrøde × jordtype
# dvs. hvis en bedrift har flere marker med samme afgrøde
# på samme jordtype, summeres arealet til én observation
# 
# -------------------------------------------------

farm_level <- dat_hvede%>%
  group_by(cvr, hvede_type, jb3) %>%
  summarise(
    area = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------
# 2. TOTAL byg (vår + vinter) pr. farm
# -> summerer areal på tværs af jordtyper og
#    både vårbyg og vinterbyg
# -> giver samlet byg-areal pr. cvr
# -------------------------------------------------

farm_totals <- farm_level %>%
  group_by(cvr) %>%
  summarise(
    total_area = sum(area),
    .groups = "drop"
  )

# -------------------------------------------------
# 3. shares
# -> beregner andelen af hver cvr's areal i hver
#    afgrøde × jordtype-kombination relativt til
#    samlet byg-areal (vår + vinter)
# -------------------------------------------------

farm_shares <- farm_level %>%
  left_join(farm_totals, by = "cvr") %>%
  mutate(
    share = area / total_area
  )


# -------------------------------------------------
# 4. Sikrer unikhed (kritisk trin)
# -> Da der er dubletter i cvr × afgrøde × jordtype, aggregeres shares igen, så der er
#    præcis én række pr. cvr × afgrøde × jordtype
# -------------------------------------------------

farm_shares <- farm_shares %>%
  group_by(cvr, hvede_type, jb3) %>%
  summarise(
    share = sum(share),
    .groups = "drop"
  )

# -------------------------------------------------
# 5. Omdanner til wide format (til regression)
# -> én række pr. cvr
# -> kolonner = (afgrøde × jordtype)
# -> værdier = andele (shares)
# -------------------------------------------------

farm_shares_wide <- farm_shares %>%
  pivot_wider(
    names_from = c(hvede_type, jb3),
    values_from = share,
    values_fn = sum
  
  )

# -------------------------------------------------
# 6. Tjek: sikrer at der kun er én række per cvr
# -------------------------------------------------

farm_shares_wide %>%
  summarise(
    n_rows = n(),
    n_cvr = n_distinct(cvr)
  )


# COMMAND ----------




# -------------------------------------------------
# 7. KONSTRUER FARM-LEVEL YIELD 
# -------------------------------------------------
dat_yield <- dat_hvede %>%
  
  # bevarer én U_2 pr farm (ingen aggregation af output)
  distinct(cvr, U_2) %>%
  
  # beregner total areal pr farm med hvede
  left_join(
    dat_hvede %>%
      group_by(cvr) %>%
      summarise(
        total_areal = sum(imk_areal, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "cvr"
  ) %>%
  
  # beregner yield (afhængige variabel)
  mutate(
    yield_per_ha = U_2 / total_areal,
    ln_yield = log(yield_per_ha)
  )



# -------------------------------------------------
# 8. JOIN TIL ESTIMATIONSDATA 
# -------------------------------------------------
dat_est <- farm_shares_wide %>%
  left_join(
    dat_yield %>% select(cvr, yield_per_ha, ln_yield),
    by = "cvr"
  )

# -------------------------------------------------
# 9. vælg kun de 11 kystvande
# -------------------------------------------------
valid_catchments <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

dat_hvede_sub <- dat_hvede %>%
  filter(KystvandID %in% valid_catchments)

# -------------------------------------------------
# 2. lav entydig cvr → kystvand mapping (KUN fra filtreret data)
# -------------------------------------------------
kyst_map <- dat_hvede_sub %>%
  group_by(cvr) %>%
  summarise(
    KystvandID = first(KystvandID),
    .groups = "drop"
  )

# -------------------------------------------------
# 3. byg estimation dataset
# -------------------------------------------------
dat_est <- farm_shares_wide %>%
  left_join(dat_yield, by = "cvr") %>%
  left_join(kyst_map, by = "cvr")

display(dat_est)

# COMMAND ----------

str(dat_est)

# COMMAND ----------

dat_est <- dat_est %>%
  mutate(across(
    c(
      Vinterhvede_ler,
      Vinterhvede_sand_grov,
      Vinterhvede_sand_let,
      Vårhvede_ler,
      Vårhvede_sand_grov,
      Vårhvede_sand_let
    ),
    ~ replace_na(.x, 0)
  ))

# COMMAND ----------

# Estimerer for hvede:
model_hvede <- feols(
  ln_yield ~ 
    Vinterhvede_sand_grov +
    Vinterhvede_sand_let +
    
    Vårhvede_ler +
    Vårhvede_sand_grov +
    Vårhvede_sand_let 
    
  | KystvandID,
  
  cluster = ~ cvr,
  data = dat_est
)

summary(model_hvede)

# COMMAND ----------

dat_hvede %>%
  filter(hvede_type == "Vårhvede") %>%
  count(jb3, name = "n_obs") %>%
  tidyr::complete(jb3, fill = list(n_obs = 0)) %>%
  arrange(desc(n_obs))

  dat_hvede %>%
  filter(hvede_type == "Vårhvede", jb3 %in% c("sand_grov","sand_let", "ler")) %>%
  summarise(
    n_cvr = n_distinct(cvr),
    n_obs = n()
  )

# COMMAND ----------

# MAGIC %md
# MAGIC **pga MEGET få vårhvede obs, der giver biased ekstreme estimater, køres analysen nu med en samlet hvedekategori**

# COMMAND ----------

# Opretter datasæt for hvede: Vinterhvede (11), vinterhvede brødhvede (13), Vårhvede (2), Vårhvede, brødhvede (6) (helsædshvede er ikke inkluderet!)

dat_hvede <- datFields %>%
  filter(afgkode %in% c(2, 6, 11, 13)) %>% # hvedetyper
  mutate(
    hvede_type = case_when(
      afgkode %in% c(2, 6, 11, 13) ~ "Hvede"
      
      
    )
  )

# Tilknytter U_2 (udbytte hvede)
dat_hvede <- dat_hvede %>%
  left_join(
    datFarms %>% select(cvr, U_2),
    by = "cvr"
  )

# Identificerer bedrifter uden output (U_2 = 0) # 4 CVR ud af 289 cvr med hvedemarker
CVR_U2_0 <- dat_hvede %>%
  filter(U_2 == 0 | is.na(U_2)) %>%
  distinct(cvr) %>%
  pull(cvr)

# tjekker CVR
# CVR_U2_0



# Fjerner disse bedrifter fra datasættet
dat_hvede <- dat_hvede %>%
  filter(!cvr %in% CVR_U2_0)

n_distinct(dat_hvede$cvr)

# COMMAND ----------

# Her ses jordtypefordeling i hvede-data
jb_counts <- dat_hvede %>%
  count(jb_kode, name = "n_obs") %>%
  tidyr::complete(jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(jb_kode)

jb_counts

# Konstruerer 3 jorbonitetsklasser ud fra Jb_kode # NB fordi "JB_7_og_8", "JB_11" er så små at de giver problemer i estimeringen!

dat_hvede <- dat_hvede %>%
  mutate(
    jb3 = case_when(
      jb_kat == "JB_1_og_3" ~ "sand_let",
      jb_kat == "JB_2_og_4" ~ "sand_grov",
      jb_kat %in% c("JB_5_og_6", "JB_7_og_8", "JB_11") ~ "ler",
      TRUE ~ NA_character_
    )
  )

# -------------------------------------------------
# (valgfrit men anbefalet) Tjek for missing
# -------------------------------------------------

table(dat_hvede$jb3, useNA = "ifany")

table(dat_hvede$jb_kode, dat_hvede$jb_kat, useNA = "ifany")

# COMMAND ----------

# -------------------------------------------------
# 1. Kollapser markdata til farm × afgrøde × jordtype
# -> én række pr. cvr × afgrøde × jordtype
# dvs. hvis en bedrift har flere marker med samme afgrøde
# på samme jordtype, summeres arealet til én observation
# 
# -------------------------------------------------

farm_level <- dat_hvede%>%
  group_by(cvr, hvede_type, jb3) %>%
  summarise(
    area = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------
# 2. TOTAL byg (vår + vinter) pr. farm
# -> summerer areal på tværs af jordtyper og
#    både vårbyg og vinterbyg
# -> giver samlet byg-areal pr. cvr
# -------------------------------------------------

farm_totals <- farm_level %>%
  group_by(cvr) %>%
  summarise(
    total_area = sum(area),
    .groups = "drop"
  )

# -------------------------------------------------
# 3. shares
# -> beregner andelen af hver cvr's areal i hver
#    afgrøde × jordtype-kombination relativt til
#    samlet byg-areal (vår + vinter)
# -------------------------------------------------

farm_shares <- farm_level %>%
  left_join(farm_totals, by = "cvr") %>%
  mutate(
    share = area / total_area
  )


# -------------------------------------------------
# 4. Sikrer unikhed (kritisk trin)
# -> Da der er dubletter i cvr × afgrøde × jordtype, aggregeres shares igen, så der er
#    præcis én række pr. cvr × afgrøde × jordtype
# -------------------------------------------------

farm_shares <- farm_shares %>%
  group_by(cvr, hvede_type, jb3) %>%
  summarise(
    share = sum(share),
    .groups = "drop"
  )

# -------------------------------------------------
# 5. Omdanner til wide format (til regression)
# -> én række pr. cvr
# -> kolonner = (afgrøde × jordtype)
# -> værdier = andele (shares)
# -------------------------------------------------

farm_shares_wide <- farm_shares %>%
  pivot_wider(
    names_from = c(hvede_type, jb3),
    values_from = share,
    values_fn = sum
  
  )

# -------------------------------------------------
# 6. Tjek: sikrer at der kun er én række per cvr
# -------------------------------------------------

farm_shares_wide %>%
  summarise(
    n_rows = n(),
    n_cvr = n_distinct(cvr)
  )

# COMMAND ----------

# -------------------------------------------------
# 7. KONSTRUER FARM-LEVEL YIELD 
# -------------------------------------------------
dat_yield <- dat_hvede %>%
  
  # bevarer én U_2 pr farm (ingen aggregation af output)
  distinct(cvr, U_2) %>%
  
  # beregner total areal pr farm med hvede
  left_join(
    dat_hvede %>%
      group_by(cvr) %>%
      summarise(
        total_areal = sum(imk_areal, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "cvr"
  ) %>%
  
  # beregner yield (afhængige variabel)
  mutate(
    yield_per_ha = U_2 / total_areal,
    ln_yield = log(yield_per_ha)
  )



# -------------------------------------------------
# 8. JOIN TIL ESTIMATIONSDATA 
# -------------------------------------------------
dat_est <- farm_shares_wide %>%
  left_join(
    dat_yield %>% select(cvr, yield_per_ha, ln_yield),
    by = "cvr"
  )

# -------------------------------------------------
# 9. vælg kun de 11 kystvande
# -------------------------------------------------
valid_catchments <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

dat_hvede_sub <- dat_hvede %>%
  filter(KystvandID %in% valid_catchments)

# -------------------------------------------------
# 2. lav entydig cvr → kystvand mapping (KUN fra filtreret data)
# -------------------------------------------------
kyst_map <- dat_hvede_sub %>%
  group_by(cvr) %>%
  summarise(
    KystvandID = first(KystvandID),
    .groups = "drop"
  )

# -------------------------------------------------
# 3. byg estimation dataset
# -------------------------------------------------
dat_est <- farm_shares_wide %>%
  left_join(dat_yield, by = "cvr") %>%
  left_join(kyst_map, by = "cvr")

display(dat_est)

# COMMAND ----------

dat_est <- dat_est %>%
  mutate(across(
    c(
      Hvede_ler,
      Hvede_sand_grov,
      Hvede_sand_let
      
    ),
    ~ replace_na(.x, 0)
  ))

# Estimerer for hvede:
model_hvede1 <- feols(
  ln_yield ~ 
    Hvede_sand_grov +
    Hvede_sand_let
  | KystvandID,
  
  cluster = ~ cvr,
  data = dat_est
)

summary(model_hvede1)