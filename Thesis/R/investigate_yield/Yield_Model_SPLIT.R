# Databricks notebook source
# Udbyttemodel Test crop fixed effects

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

# indlæs klarggjort markniveau datasæt
datFields <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_fields.rds" ) )

# behold kun observationer i markniveau datasættet der er brugt i estimeringen
datFields <- datFields %>% semi_join( datFarms, by = "cvr" )

# dropper geometrien
datFields <- sf::st_drop_geometry(datFields)

# COMMAND ----------

# Opretter dataframe dat_all, tilføjer nye variabelnavne, der matcher estimering - Hvede aggregeres over 4 afgrødekoder, fordi 3 ud af 4 er for små til at kunne bære selvstændig estimering. For raps, er vårraps ikke medtaget, da vi har nul observationer i vores datasæt. vårbyg og vinterbyg aggregeres for nu, men bør beholdes i seperate kategorier (brug evt. dummy her), da der er tilstrækkelige observationer. Jeg mangler at undersøge om rug_triticale kan disagregeres (om der er nok obs i de individuelle afgrødekategorier) - derudover kan vi medtage flere afgrøder fx: havre, majs, roer, kartofler kunne være relevante evt. flere. (tjek gerne hvilke afgrøder der er størst i data, så vi får de vigtigste med) 

dat_all <- datFields %>%
  mutate(
    crop_cat = case_when(
      afgkode %in% c(2, 6, 11, 13) ~ "Hvede",
      afgkode %in% c(1) ~ "Vår_Byg",
      afgkode %in% c(10) ~ "Vinter_Byg",  # bør splittes - men så skal vi finde ud af, hvordan vi fordeler udbytte
      afgkode %in% c(22) ~ "Raps", # NB ingen observationer i vores data med Raps_vår! derfor er denne ikke med
      afgkode %in% c(14, 15, 16) ~ "Rug_Triticale",
      afgkode %in% c(3, 57) ~ "Havre",  # obs ikke tjekket - tjek om både cropcat og udbytte passer
      afgkode %in% c(160, 280) ~ "Roer", # obs ikke tjekket - tjek om både cropcat og udbytte passer
       afgkode %in% c(5, 19, 216, 218) ~ "Majs", # obs ikke tjekket - tjek om både cropcat og udbytte passer
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(!is.na(crop_cat)) # reducerer datasæt så vi kun har observationer for relevante crop_cat


# konstruerer aggregerede jordklassifikationer, fordi vi har for få obs i "JB_7_og_8", "JB_11" - undersøg lige om de er den korrekte klassificering

dat_all <- dat_all %>%
  mutate(
    jb3 = case_when(
      jb_kat == "JB_1_og_3" ~ "sand_fin",
      jb_kat == "JB_2_og_4" ~ "sand_grov",
      jb_kat %in% c("JB_5_og_6", "JB_7_og_8", "JB_11") ~ "ler", # overvej at opdele kategorier, fx JB 5 og 6 for sig også
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(crop_cat)) 

# Tilknytter de relevante udbyttekategorier

#### (havre U_3 men ikke ren roer U_5 (men både sukker og foder, majs U_13 men kun helsæd)####

  dat_farm <- dat_all %>%
  left_join(
    datFarms %>% select(cvr, U_1, U_2, U_3, U_4, U_5, U_8, U_16), # vi skal sørge for at U_1 kun tilknyttes marker med byg osv. 
    by = "cvr"
  )

  #dat_farm <- dat_all %>%
  #left_join(
  #  datFarms %>% select(cvr, U_1, U_2, U_4, U_8), # vi skal sørge for at U_1 kun tilknyttes marker med byg osv. mmåske denne blot er overflødig
  #  by = "cvr"
  #)

  
  dat_farm <- dat_farm %>%
  mutate(
    output = case_when(
      crop_cat == "Hvede" ~ U_2,
      crop_cat == "Vår_Byg" ~ U_1,
      crop_cat == "Vinter_Byg" ~ U_1,
      crop_cat == "Raps" ~ U_4,
      crop_cat == "Rug_Triticale" ~ U_8,
      crop_cat == "Havre" ~ U_3,
      crop_cat == "Roer" ~ U_5,
      crop_cat == "Majs" ~ U_16,  #OBS på om dette er den korrekte
      TRUE ~ NA_real_
    )
  )

# -------------------------------------------------
# Tjek fordeling på jordtyper 
# -------------------------------------------------

table(dat_farm$jb3, useNA = "ifany")

# COMMAND ----------

# Undersøger hvor mange marker, der har relevant afgrøde men 0 eller na i relevante U: # OBS 774 marker med 0 eller na i udbytte, disse frasorteres i sidste kode! (13% af obs!).
u_vars <- c("output")

dat_farm %>%
  mutate(
    all_u_zero_or_na = if_all(all_of(u_vars), ~ is.na(.) | . == 0)
  ) %>%
  summarise(
    n_total_obs = n(),
    n_all_u_zero_or_na = sum(all_u_zero_or_na, na.rm = TRUE),
    share = mean(all_u_zero_or_na, na.rm = TRUE)
  )

dat_farm<- dat_farm %>%
  filter(
    !if_any(all_of(u_vars), ~ is.na(.) | . == 0)
  )

# COMMAND ----------

# -------------------------------------------------
# Vores 11 relevante vandoplande
# -------------------------------------------------
valid_catchments <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

# -------------------------------------------------
# Tildel hvert cvr ét hoved-opland (kun valid catchments) - dvs. alle bedriftens marker tilskrives det (af vores 11) kystvandsoplande, det har størst areal inden for
# -------------------------------------------------
main_catchment_df <- dat_farm %>%
  filter(KystvandID %in% valid_catchments) %>%
  group_by(cvr, KystvandID) %>%
  summarise(
    total_area = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cvr, desc(total_area)) %>%
  group_by(cvr) %>%
  slice(1) %>%
  ungroup() %>%
  rename(main_catchment = KystvandID)

# -------------------------------------------------
# Merge tilbage
# -------------------------------------------------
dat_farm <- dat_farm %>%
  left_join(main_catchment_df, by = "cvr")

# -------------------------------------------------
# Tjek korrekt variabel
# -------------------------------------------------
check <- dat_farm %>%
  group_by(cvr) %>%
  summarise(n_main = n_distinct(main_catchment)) %>%
  count(n_main)

print(check)

# COMMAND ----------

## Tjek fordeling af cvr, antal marker, total areal per kystvandopland (obs 4997 marker ialt)
dat_farm %>%
  group_by(main_catchment) %>%
  summarise(
    n_cvr = n_distinct(cvr),
    n_marker = n(),
    total_area = sum(imk_areal, na.rm = TRUE)
  ) %>%
  arrange(desc(n_cvr))

# -------------------------------------------------
# MODEL DATA (korrekt definition)
# -------------------------------------------------
dat_model <- dat_farm %>%
  filter(!is.na(main_catchment))

# Endelig fordeling pr. opland
dat_model %>%
  group_by(main_catchment) %>%
  summarise(
    n_cvr = n_distinct(cvr),
    n_marker = n(),
    total_area = sum(imk_areal, na.rm = TRUE)
  ) %>%
  arrange(desc(n_cvr))

# COMMAND ----------

# Forsøg vinter/vårsplit

# -------------------------------------------------
# 1. Sikrer, at vi kun vælger et U_ for hver cvr for hver afgrødekategori (da vi i data har aggregerede udbytter for hver mark=række)
# -------------------------------------------------
output_farm_crop <- dat_model %>%
  group_by(cvr, crop_cat) %>%
  summarise(
    output = first(output),
    .groups = "drop"
  )

# -------------------------------
# 1. AREAL (detaljeniveau)
# -------------------------------
area_detail <- dat_model %>%
  group_by(cvr, crop_cat, jb3, main_catchment) %>%
  summarise(
    area_j = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  )


# -------------------------------
# 2. CROP GROUP (byg samles)
# -------------------------------
area_detail <- area_detail %>%
  mutate(
    crop_group = case_when(
      crop_cat %in% c("Vår_Byg", "Vinter_Byg") ~ "Byg",
      TRUE ~ crop_cat
    )
  )


# -------------------------------
# 3. TOTAL AREAL PR CROP GROUP
# -------------------------------
area_total <- dat_model %>%
  mutate(
    crop_group = case_when(
      crop_cat %in% c("Vår_Byg", "Vinter_Byg") ~ "Byg",
      TRUE ~ crop_cat
    )
  ) %>%
  group_by(cvr, crop_group, main_catchment) %>%
  summarise(
    total_area = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  )


# -------------------------------
# 4. OUTPUT PÅ CROP GROUP (U_1 fælles for byg)
# -------------------------------
output_group <- output_farm_crop %>%
  mutate(
    crop_group = case_when(
      crop_cat %in% c("Vår_Byg", "Vinter_Byg") ~ "Byg",
      TRUE ~ crop_cat
    )
  ) %>%
  group_by(cvr, crop_group) %>%
  summarise(
    output = first(output),
    .groups = "drop"
  )


# -------------------------------
# 5. SAMLET DATASET
# -------------------------------
dat_est <- area_detail %>%
  left_join(area_total,
            by = c("cvr", "crop_group", "main_catchment")) %>%
  mutate(
    share = area_j / total_area
  ) %>%
  left_join(output_group,
            by = c("cvr", "crop_group"))


#--------------------------------------------------
# Afhængig variabel oprettes
#--------------------------------------------------

# Først tjekket, at vi har gyldige obs for alle observationer:

dat_est %>%
  summarise(
    n_total = n(),
    
    n_invalid_output = sum(is.na(output) | output == 0, na.rm = TRUE),
    n_invalid_area   = sum(is.na(total_area) | total_area == 0, na.rm = TRUE),
    
    n_any_invalid = sum(
      is.na(output) | output == 0 |
      is.na(total_area) | total_area == 0,
      na.rm = TRUE
    ),
    
    share_invalid = mean(
      is.na(output) | output == 0 |
      is.na(total_area) | total_area == 0,
      na.rm = TRUE
    )
  )

# oprettet afhængig variabel:
dat_est <- dat_est %>%
  mutate(
    ln_output_per_ha = log(output / total_area)
  )

# COMMAND ----------

# estimering model vår/vintersplit

dat_est <- dat_est %>%
  mutate(
    crop_cat = relevel(as.factor(crop_cat), ref = "Vår_Byg"),
    jb3 = relevel(as.factor(jb3), ref = "ler")
  )


model_ny <- feols(
  ln_output_per_ha ~ i(crop_cat, jb3) | cvr + main_catchment,
  data = dat_est
)

etable(model_ny)