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

# Dropper geometrien
datFields <- sf::st_drop_geometry(datFields)

# COMMAND ----------

# overblik over de marker, der ligger indenfor de 11 vandoplande
ids_to_keep <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

jb_kyst <- datFields %>%
  filter(KystvandID %in% ids_to_keep) %>%   
  count(KystvandID, jb_kode, name = "n_obs") %>%
  complete(KystvandID, jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(KystvandID, jb_kode)

jb_kyst_wide <- jb_kyst %>%
  pivot_wider(
    names_from = jb_kode,
    values_from = n_obs,
    names_prefix = "JB_"
  )

print(jb_kyst_wide, n = Inf, width = Inf)

# COMMAND ----------

# overblik over de marker, der ligger udenfor de 11 kystvandsoplande

ids_to_exclude <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

jb_kyst_rest <- datFields %>%
  filter(!KystvandID %in% ids_to_exclude) %>%   # 👈 modsatte filter
  count(KystvandID, jb_kode, name = "n_obs") %>%
  complete(KystvandID, jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(KystvandID, jb_kode)

jb_kyst_rest_wide <- jb_kyst_rest %>%
  pivot_wider(
    names_from = jb_kode,
    values_from = n_obs,
    names_prefix = "JB_"
  )

print(jb_kyst_rest_wide, n = Inf, width = Inf)


n_obs_rest <- datFields %>%
  filter(!KystvandID %in% ids_to_exclude) %>%
  summarise(n_obs = n())

n_obs_rest

# COMMAND ----------

# jordtypefordeling i hele datasættet 
# samlet antal marker i hele datasættet: 12783

# Her ses jordtypefordeling i data
jb_counts <- datFields %>%
  count(jb_kode, name = "n_obs") %>%
  tidyr::complete(jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(jb_kode)

jb_counts
# antal marker i hele datasættet:
jb_counts %>%
  summarise(total_n_obs = sum(n_obs))

# COMMAND ----------

# MAGIC %md
# MAGIC **Kun BYG**

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

# Konstruerer 3 jorbonitetsklasser ud fra Jb_kode # NB fordi "JB_7_og_8", "JB_11" er så små at de giver problemer i estimeringen!

dat_byg <- dat_byg %>%
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

table(dat_byg$jb3, useNA = "ifany")

# COMMAND ----------

# Her ses jordtypefordeling i byg-data
jb_counts <- dat_byg %>%
  count(jb_kode, name = "n_obs") %>%
  tidyr::complete(jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(jb_kode)

jb_counts

# COMMAND ----------

# overblik over de marker, der ligger indenfor de 11 vandoplande **BYG**

ids_to_keep <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

jb_kyst_byg <- datFields %>%
  filter(KystvandID %in% ids_to_keep) %>%   
  count(KystvandID, jb_kode, name = "n_obs") %>%
  complete(KystvandID, jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(KystvandID, jb_kode)

jb_kyst_byg_wide <- jb_kyst_byg %>%
  pivot_wider(
    names_from = jb_kode,
    values_from = n_obs,
    names_prefix = "JB_"
  )

print(jb_kyst_byg_wide, n = Inf, width = Inf)

n_obs_byg <- dat_byg %>%
  filter(KystvandID %in% ids_to_keep) %>%
  summarise(n_obs = n())

n_obs_byg

# COMMAND ----------

# overblik over de marker, der ligger udenfor de 11 kystvandsoplande **BYG**

ids_to_exclude <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

jb_kyst_rest_byg <- dat_byg %>%
  filter(!KystvandID %in% ids_to_exclude) %>%   # 
  count(KystvandID, jb_kode, name = "n_obs") %>%
  complete(KystvandID, jb_kode = 1:11, fill = list(n_obs = 0)) %>%
  arrange(KystvandID, jb_kode)

jb_kyst_rest_byg_wide <- jb_kyst_rest %>%
  pivot_wider(
    names_from = jb_kode,
    values_from = n_obs,
    names_prefix = "JB_"
  )

print(jb_kyst_rest_byg_wide, n = Inf, width = Inf)


n_obs_rest_byg <- dat_byg %>%
  filter(!KystvandID %in% ids_to_exclude) %>%
  summarise(n_obs = n())

n_obs_rest_byg

# COMMAND ----------

display (dat_byg)

# COMMAND ----------

library(dplyr)

datFields%>%
  summarise(n_cvr = n_distinct(cvr))

# COMMAND ----------


# -------------------------------------------------
# 1. Kollapser markdata til farm × afgrøde × jordtype
# -> én række pr. cvr × afgrøde × jordtype
# dvs. hvis en bedrift har flere marker med samme afgrøde
# på samme jordtype, summeres arealet til én observation
# 
# -------------------------------------------------

farm_level <- dat_byg %>%
  group_by(cvr, afgroede, jb3) %>%
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
  group_by(cvr, afgroede, jb3) %>%
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
    names_from = c(afgroede, jb3),
    values_from = share,
    values_fill = 0
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
# KONSTRUER FARM-LEVEL YIELD 
# -------------------------------------------------
dat_yield <- dat_byg %>%
  
  # bevarer én U_1 pr farm (ingen aggregation af output)
  distinct(cvr, U_1) %>%
  
  # beregner total areal pr farm med byg
  left_join(
    dat_byg %>%
      group_by(cvr) %>%
      summarise(
        total_areal = sum(imk_areal, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "cvr"
  ) %>%
  
  # beregner yield (afhængige variabel)
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

display(farm_shares_wide)

# COMMAND ----------

display(dat_est)

# COMMAND ----------

model_kyst <- feols(
  ln_yield ~ 
    Vårbyg_sand_let +
    Vårbyg_sand_grov +
    Vårbyg_ler +
    

    Vinterbyg_sand_let +
    Vinterbyg_sand_grov +
    Vinterbyg_ler
  | KystvandID,
  
  cluster = ~ cvr,
  data = dat_est
)

summary(model_kyst)

# COMMAND ----------

model_test <- lm(
  ln_yield ~ 
    Vårbyg_sand_let +
    Vårbyg_sand_grov +
    Vårbyg_ler +
    Vinterbyg_sand_let +
    Vinterbyg_sand_grov,
  ,
  data = dat_est
)

summary(model_test)

# COMMAND ----------

names(coefs)

# COMMAND ----------

# Henter coefficienter fra model_kyst:
coefs <- coef(model_kyst)

vars <- c(
  "Vårbyg_sand_let",
  "Vårbyg_sand_grov",
  "Vårbyg_ler",
  "Vinterbyg_sand_let",
  "Vinterbyg_sand_grov",
  "Vinterbyg_ler"
)

tau <- coefs[vars]
names(tau) <- vars

tau

# COMMAND ----------

display(dat_byg)

# COMMAND ----------

# konstruerer gamma (for hver mark)

dat_byg <- dat_byg %>%
  mutate(
    gamma = case_when(
      afgkode == 1 & jb3 == "sand_let"  ~ tau["Vårbyg_sand_let"],
      afgkode == 1 & jb3 == "sand_grov" ~ tau["Vårbyg_sand_grov"],
      afgkode == 1 & jb3 == "ler"       ~ tau["Vårbyg_ler"],

      afgkode == 10 & jb3 == "sand_let"  ~ tau["Vinterbyg_sand_let"],
      afgkode == 10 & jb3 == "sand_grov" ~ tau["Vinterbyg_sand_grov"],
      afgkode == 10 & jb3 == "ler"       ~ tau["Vinterbyg_ler"],

      TRUE ~ 0
    ),
    
    theta = exp(gamma)
  )

# COMMAND ----------

# beregner effective area af mark - tæller!
dat_byg <- dat_byg %>%
  mutate(eff_area = imk_areal * theta)

# COMMAND ----------

# beregner nævner - dvs totalt dyrket bygareal
dat_byg <- dat_byg %>%
  group_by(cvr) %>%
  mutate(total_eff_area = sum(eff_area, na.rm = TRUE)) %>%
  ungroup()

# COMMAND ----------


#allokerer Yields

dat_byg <- dat_byg %>%
  mutate(
    y_hat = U_1 * (eff_area / total_eff_area)
  )

# COMMAND ----------

display(dat_byg)

# COMMAND ----------

# tjekker om summen af Y_hat er lig U_1 på CVR_niveau
dat_byg %>%
  group_by(cvr) %>%
  summarise(
    check = sum(y_hat),
    farm_total = first(U_1),
    .groups = "drop"
  ) %>%
  summarise(max_diff = max(abs(check - farm_total)))

# COMMAND ----------

dat_est <- farm_shares_wide %>%
  left_join(dat_yield, by = "cvr")