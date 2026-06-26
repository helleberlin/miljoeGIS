# Databricks notebook source
# Udbyttemodel Test for Byg

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
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro"

# vælg model specifikation
obs <- "conv_crops" # 

# indlæs bedriftsniveau datasæt bestående af observationer fra estimeringen: henter datasæt med resultater for CVR’er hvor TYPEKODE = 4 (konventionelle planteproducenter)
datFarms <- readRDS( file.path( dirPath, "output/dat_prepared/dat_prepared_farms.rds" ) )

# vælg observationer der skal bruges i estimeringen
if( obs == "all" ){
  datFarms <- datFarms
} else if( obs %in% c( "eco_crops", "eco_mix", "conv_mix", "conv_crops" ) ){
  datFarms <- subset( datFarms, TYPEKAT == obs )
} else {
  stop( "Ukendt model-specifikation. Brug obs = 'eco_crops', 'eco_mix', 'conv_mix', 'conv_crops', eller 'all'" )
}


# indlæs klarggjort markniveau datasæt
datFields <- readRDS( file.path( dirPath, "output/dat_prepared/dat_prepared_fields.rds" ) )

# behold kun observationer i markniveau datasættet der er brugt i estimeringen
datFields <- datFields %>% semi_join( datFarms, by = "cvr" ) 




# COMMAND ----------

datFields <- sf::st_drop_geometry(datFields)

# COMMAND ----------

# Opretter datasæt for byg (vår/vinter)

dat_byg <- datFields %>%
  filter(afgkode %in% c(1, 10))   # vår + vinter byg

# Tilknytter U_1 (udbytte byg)
dat_byg <- dat_byg %>%
  left_join(
    datFarms %>% select(cvr, U_1),
    by = "cvr"
  )

# Identificerer bedrifter uden output (U_1 = 0)
CVR_U1_0 <- dat_byg %>%
  filter(U_1 == 0) %>%
  distinct(cvr) %>%
  pull(cvr)

# Fjerner disse bedrifter fra datasættet
dat_byg <- dat_byg %>%
  filter(!cvr %in% CVR_U1_0)


# COMMAND ----------

display(dat_byg)

# COMMAND ----------

# MAGIC %md
# MAGIC **opbygger datasæt  til modelestimering**

# COMMAND ----------

# -------------------------------------------------
# KONSTRUER FARM-LEVEL YIELD 
# -------------------------------------------------
dat_yield <- dat_byg %>%
  
  # bevarer én U_1 pr farm (ingen aggregation af output)
  distinct(cvr, U_1) %>%
  
  # beregner total areal pr farm
  left_join(
    dat_byg %>%
      group_by(cvr) %>%
      summarise(
        total_areal = sum(imk_areal, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "cvr"
  ) %>%
  
  # beregner yield
  mutate(
    yield_per_ha = U_1 / total_areal,
    ln_yield = log(yield_per_ha)
  )


# -------------------------------------------------
# 6. JOIN TIL ESTIMATIONSDATA (dat_byg2 / farm_shares_wide)
# -------------------------------------------------
dat_est <- farm_shares_wide %>%
  left_join(
    dat_yield %>% select(cvr, yield_per_ha, ln_yield),
    by = "cvr"
  )

# COMMAND ----------

# -------------------------------------------------
# 1. Kollapser markdata til farm × afgrøde × jordtype
# -------------------------------------------------

farm_level <- dat_byg %>%
  group_by(cvr, afgroede, jb_kat) %>%
  summarise(
    area = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------
# 2. TOTAL byg (vår + vinter) pr farm
# -------------------------------------------------
farm_totals <- farm_level %>%
  group_by(cvr) %>%
  summarise(
    total_area = sum(area),   # A_iu (samlet byg)
    .groups = "drop"
  )

# -------------------------------------------------
# 3. shares (paper-korrekt)
# -------------------------------------------------
farm_shares <- farm_level %>%
  left_join(farm_totals, by = "cvr") %>%
  mutate(
    share = area / total_area
  )


# -------------------------------------------------
# 4. Omdanner til wide format (til regression)
# -------------------------------------------------
farm_shares_wide <- farm_shares %>%
  
  # Gør data bred: én række pr farm
  # Kolonner = (afgrøde × jordtype)
  tidyr::pivot_wider(
    names_from = c(afgroede, jb_kat),  # kombinerer crop + jordtype i kolonnenavn
    values_from = share,               # indsætter shares som værdier
    values_fill = 0                    # manglende kombinationer = 0
  )

dat_est <- farm_shares_wide %>%
  left_join(dat_yield, by = "cvr")

# COMMAND ----------

# -------------------------------------------------
# 1. vælg kun de 11 kystvande
# -------------------------------------------------
valid_catchments <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

dat_byg_sub <- dat_byg %>%
  filter(KystvandID %in% valid_catchments)

# -------------------------------------------------
# 2. lav entydig cvr → kystvand mapping (KUN fra filtreret data)
# -------------------------------------------------
kyst_map <- dat_byg_sub %>%
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

# COMMAND ----------

model_kyst <- feols(
  ln_yield ~ 
    Vårbyg_JB_1_og_3 +
    Vårbyg_JB_2_og_4 +
    Vårbyg_JB_7_og_8 +
    Vårbyg_JB_11 +
    Vinterbyg_JB_1_og_3 +
    Vinterbyg_JB_2_og_4 +
    Vinterbyg_JB_5_og_6 +
    Vinterbyg_JB_7_og_8 +
    Vinterbyg_JB_11
  | KystvandID,
  cluster = ~ cvr,
  data = dat_est
)

summary(model_kyst)

# COMMAND ----------

dat_est %>%
  filter(
    is.na(ln_yield) |
    if_any(starts_with("Vårbyg"), is.na) |
    if_any(starts_with("Vinterbyg"), is.na)
  )

etable(model_kyst, fitstat = "n")

# COMMAND ----------

table(dat_est$KystvandID)

# COMMAND ----------

display(dat_est)

# COMMAND ----------

dat_est %>%
  group_by(KystvandID, Vinterbyg_JB_7_og_8) %>%
  summarise(n = n())

# COMMAND ----------




model_fe <- feols(
  ln_yield ~ 
    Vårbyg_JB_1_og_3 +
    Vårbyg_JB_2_og_4 +
    Vårbyg_JB_7_og_8 +
    Vårbyg_JB_11 +
    Vinterbyg_JB_1_og_3 +
    Vinterbyg_JB_2_og_4 +
    Vinterbyg_JB_5_og_6 +
    Vinterbyg_JB_7_og_8 +
    Vinterbyg_JB_11
  | cvr,
  cluster = ~ cvr,
  data = dat_est
)

summary(model_fe)

# COMMAND ----------

display(farm_shares_wide )

# COMMAND ----------

# MAGIC %md
# MAGIC Det følgende tjekker missing observations for U_1, der er ingen missing ob for imk_mark

# COMMAND ----------

# tjekker om alle obs for U_1 > 0, det viser sig at 192 er 0 hvorimod der ikke er NA's. 
dat_byg %>%
  summarise(
    total = n(),
    na_U1 = sum(is.na(U_1)),
    zero_U1 = sum(U_1 == 0, na.rm = TRUE),
    na_or_zero_U1 = sum(is.na(U_1) | U_1 == 0),
    
    pct_na_U1 = na_U1 / total * 100,
    pct_zero_U1 = zero_U1 / total * 100,
    pct_na_or_zero_U1 = na_or_zero_U1 / total * 100
  )
 

# COMMAND ----------

# undersøger marker med U_1=0
u1_zero <- dat_byg %>%
  filter(U_1 == 0)

u1_zero %>%
  select(cvr, marknr, imk_areal, jb_kat, afgroede)
 
display(u1_zero)

# Hvor mange cvr har missing U_1 # 31
u1_zero %>%
  count(cvr, sort = TRUE)

# COMMAND ----------

# Undersøger cvr i datfields
datcvr <-datFarms %>%
  filter(cvr == "20311878")

  display(datcvr)

# COMMAND ----------

# antal cvr med byg-marker #  321 
# antal marker med byg #  684

dat_byg %>%
  summarise(
    antal_cvr = n_distinct(cvr),
    antal_marker = n_distinct(marknr)
  )

# COMMAND ----------

#afgroedetype = kun vårbyg
u1_zero %>%
  count(afgroede, sort = TRUE)

#jordtype:
u1_zero %>%
  count(jb_kat, sort = TRUE)

# COMMAND ----------

# ingen missing markobs. 
dat_byg %>%
  summarise(
    total = n(),
    zero_imk = sum(imk_areal == 0, na.rm = TRUE),
    pct_zero_imk = zero_imk / total * 100
  )

# COMMAND ----------

# areal: resultat - helt almindelige markstr.  
u1_zero %>%
  summarise(
    mean_area = mean(imk_areal, na.rm = TRUE),
    median_area = median(imk_areal, na.rm = TRUE),
    min_area = min(imk_areal, na.rm = TRUE)
  )