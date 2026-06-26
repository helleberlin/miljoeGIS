# Databricks notebook source
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

# indlæs klargjorte datasæt
datFarms <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )
 
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

display(datFields)

# COMMAND ----------

# Tjekker fordeling af afgrøder på jordtyper i jb3

#datFields %>%
#  count(afgkode) %>%
#  print(n = Inf)

datFields %>%
  mutate(
   crop_cat = case_when(
      afgkode %in% c(2, 11, 13) ~ "Hvede",
      afgkode %in% c(1, 10) ~ "Byg",
      afgkode %in% c(22) ~ "Raps", 
      afgkode %in% c(14, 15, 16) ~ "Rug_Triticale",
      afgkode %in% c(149, 150, 151, 152, 155, 156, 157) ~ "Kartofler", 
      afgkode %in% c(3, 4) ~ "Havre_blandet_korn",
      afgkode %in% c(101, 102, 105, 107, 108, 111, 112, 113, 117, 120, 124, 655, 666, 668) ~ "Frø",
      #afgkode %in% c(250, 251, 252, 254, 255, 256, 257, 276, 286, 172, 174, 247, 260, 261, 263, 264, 266, 267, 268, 270, 285, 327, 210, 212, 213, 214, 701, 703, 709) ~ "Grovfoder", 
      afgkode %in% c(216, 218) ~ "Fodermajs", 
      afgkode %in% c(30, 31, 32) ~ "Ærter_mv", 
      afgkode %in% c(160, 280) ~ "Sukkerroer",
      afgkode %in% c(592, 593, 591, 596) ~ "Energiafgrøder", 
      afgkode %in% c(429, 450, 401, 513, 524, 528) ~ "Gartneri", 
      afgkode %in% c(180, 24, 5, 19) ~ "Andre_industriafgrøder", 
      afgkode %in% c(180, 24, 5, 19) ~ "Andre_landbrugsindtægter", 

      TRUE ~ NA_character_
    ),
    jb3 = case_when(
      jb_kat == "JB_1_og_3" ~ "sand_fin",
      jb_kat == "JB_2_og_4" ~ "sand_grov",
      jb_kat %in% c("JB_5_og_6", "JB_7_og_8", "JB_11") ~ "ler",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(crop_cat), !is.na(jb3)) %>%
  count(crop_cat, jb3) %>%
  arrange(crop_cat, jb3) %>%
  print(n = Inf)

# COMMAND ----------


# Opretter dataframe dat_all, tilføjer nye variabelnavne, der matcher estimering - Hvede aggregeres over 4 afgrødekoder, fordi 3 ud af 4 er for små til at kunne bære selvstændig estimering. For raps, er vårraps ikke medtaget, da vi har nul observationer i vores datasæt. vårbyg og vinterbyg aggregeres for nu, men bør beholdes i seperate kategorier (brug evt. dummy her), da der er tilstrækkelige observationer. Jeg mangler at undersøge om rug_triticale kan disagregeres (om der er nok obs i de individuelle afgrødekategorier) - derudover kan vi medtage flere afgrøder fx: havre, majs, roer, kartofler kunne være relevante evt. flere. (tjek gerne hvilke afgrøder der er størst i data, så vi får de vigtigste med) 

dat_all <- datFields %>%
  mutate(
    crop_cat = case_when(
      afgkode %in% c(2, 11, 13) ~ "Hvede",
      afgkode %in% c(1, 10) ~ "Byg",
      afgkode %in% c(22) ~ "Raps", 
      afgkode %in% c(14, 15, 16) ~ "Rug_Triticale",
      afgkode %in% c(149, 150, 151, 152, 155, 156, 157) ~ "Kartofler", 
      afgkode %in% c(3) ~ "Havre", # omnavngivet fra "Havre_blandet_korn" til havre, pga kun 4 vårsåede art obs. med U_
      afgkode %in% c(101, 102, 105, 107, 108, 111, 112, 113, 117, 120, 124, 655, 666, 668) ~ "Frø",
      # afgkode %in% c(250, 251, 252, 254, 255, 256, 257, 276, 286, 172, 174, 247, 260, 261, 263, 264, 266, 267, 268, 270, 285, 327, 210, 212, 213, 214, 701, 703, 709) ~ "Grovfoder", # fjernet pga. enormt mange missing U_
      afgkode %in% c(216, 218) ~ "Fodermajs", 
      afgkode %in% c(30, 31, 32) ~ "Ærter_mv", 
      # afgkode %in% c(160, 280) ~ "Sukkerroer", # for få obs
      # afgkode %in% c(592, 593) ~ "Energiafgrøder", #  596 og 519 fjerne på 1:1 missing U_
      # afgkode %in% c(429, 450, 401, 513, 524, 528) ~ "Gartneri", # fjernet få obs + meget heterogen
      #afgkode %in% c(180, 24, 5, 19) ~ "Andre_industriafgrøder", 
      #afgkode %in% c(180, 24, 5, 19) ~ "Andre_landbrugsindtægter", 

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
      jb_kat %in% c( "JB_5_og_6", "JB_7_og_8", "JB_11") ~ "ler", # overvej at opdele kategorier, fx JB 5 og 6 for sig også
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(crop_cat)) 

# Tilknytter de relevante udbyttekategorier

#### Tilknytter udbyttevariable####

  dat_farm <- dat_all %>%
  left_join(
    datFarms %>% select(cvr, U_1, U_2, U_3, U_4, U_5, U_6, U_7, U_8, U_9, U_10, U_11, U_12, U_13, U_14, U_15), # vi skal sørge for at U_1 kun tilknyttes marker medbyg osv. 
    by = "cvr"
  )

 

  
  dat_farm <- dat_farm %>%
  mutate(
    output = case_when(
      crop_cat == "Hvede" ~ U_2,
      crop_cat == "Byg" ~ U_1,
      crop_cat == "Raps" ~ U_8,
      crop_cat == "Havre" ~ U_3,
      crop_cat == "Rug_Triticale" ~ U_4,
      crop_cat == "Frø" ~ U_6,
      crop_cat == "Kartofler" ~ U_7,
      #crop_cat == "Grovfoder" ~ U_10,
      crop_cat == "Fodermajs" ~ U_11,  
      crop_cat == "Ærter_mv" ~ U_9,
      # crop_cat == "Sukkerroer" ~ U_5,
      # crop_cat == "Energiafgrøder" ~ U_12,
      # crop_cat == "Gartneri" ~ U_13,
      #crop_cat == "Andre_industriafgrøder" ~ U_14,
      #crop_cat == "Andre_landbrugsindtægter" ~ U_15,
      
      TRUE ~ NA_real_ 
    )
  )

# -------------------------------------------------
# Tjek fordeling på jordtyper 
# -------------------------------------------------

table(dat_farm$jb3, useNA = "ifany")

# COMMAND ----------

str( dat_farm )

# COMMAND ----------

# Undersøger afgkode/udbytte mismatch 
afgkode_mismatch <- dat_farm %>%
  mutate(
    missing_output = ifelse(is.na(output) | output <= 0, 1, 0),
    has_output = ifelse(!is.na(output) & output > 0, 1, 0)
  ) %>%
  
  group_by(afgkode, crop_cat, jb3) %>%
  summarise(
    
    # antal observationer
    n_obs = n(),
    
    # afgkode findes men output mangler
    n_missing_output = sum(missing_output, na.rm = TRUE),
    share_missing_output = n_missing_output / n_obs,
    
    # output findes men crop_cat/afgkode er problematisk
    n_orphan_output = sum(has_output & is.na(crop_cat)),
    share_orphan_output = n_orphan_output / n_obs,
    
    # samlet problem-score
    mismatch_score = n_missing_output + n_orphan_output
    
  ) %>%
  
  arrange(crop_cat)
  print(afgkode_mismatch, n = Inf)

# COMMAND ----------

library(dplyr)

afgkode_count <- dat_farm %>%
  filter(!is.na(crop_cat), !is.na(afgkode)) %>%
  
  group_by(cvr, crop_cat) %>%
  summarise(
    n_afgkode = n_distinct(afgkode),
    .groups = "drop"
  ) %>%
  
  group_by(crop_cat, n_afgkode) %>%
  summarise(
    n_cvr = n(),
    .groups = "drop"
  ) %>%
  
  group_by(crop_cat) %>%
  mutate(
    share_cvr = n_cvr / sum(n_cvr)
  ) %>%
  
  arrange(crop_cat, n_afgkode)

print(afgkode_count, n = Inf)

# COMMAND ----------



multi_afgkode <- dat_farm %>%
  filter(crop_cat == "Ærter_mv",
         !is.na(afgkode)) %>%
  
  group_by(cvr) %>%
  summarise(
    n_afgkode = n_distinct(afgkode),
    afgkoder = paste(sort(unique(afgkode)), collapse = ", "),
    .groups = "drop"
  ) %>%
  
  filter(n_afgkode > 1) %>%
  
  arrange(desc(n_afgkode))

print(multi_afgkode, n = Inf)

# COMMAND ----------

# Undersøger afgkode/Udbytte mismatch

cvr_missing_output <- dat_farm %>%
  mutate(
    missing_output = ifelse(is.na(output) | output <= 0, 1, 0),
    has_output = ifelse(!is.na(output) & output > 0, 1, 0)
  ) %>%
  
  group_by(cvr) %>%
  summarise(
    
    # 1. afgkode-observationer
    n_afgkode_obs = sum(!is.na(afgkode)),
    
    # 2. manglende output (afgkode findes men output mangler)
    n_missing_output = sum(missing_output, na.rm = TRUE),
    share_missing_output = n_missing_output / n_afgkode_obs,
    
    # 3. output findes (U_kategori registreret)
    n_output_obs = sum(has_output, na.rm = TRUE),
    
    # 4. OBS: output uden “relevant crop_cat” (ekstra diagnostik)
    n_orphan_output = sum(has_output & is.na(crop_cat)),
    
    # 5. samlet mismatch-score
    mismatch_score = n_missing_output + n_orphan_output
    
  ) %>%
  
  arrange(desc(mismatch_score))

   print(cvr_missing_output, n = Inf)

# COMMAND ----------

options(tibble.width = Inf)

dat_farm %>%
  filter(cvr == 32819591) %>%
  print(n = Inf, width = Inf)

# COMMAND ----------

display(dat_farm)

# COMMAND ----------

#diagnostik af 25% mismatch

dat_farm %>%
  summarise(
    n_total = n(),
    n_na = sum(is.na(output)),
    n_zero = sum(output == 0, na.rm = TRUE),
    share_na = mean(is.na(output)),
    share_zero = mean(output == 0, na.rm = TRUE)
  )

  dat_farm %>%
  mutate(
    missing = is.na(output) | output == 0
  ) %>%
  group_by(crop_cat) %>%
  summarise(
    n = n(),
    n_missing = sum(missing),
    share_missing = mean(missing)
  ) %>%
  arrange(desc(share_missing))



# COMMAND ----------

# Undersøger hvor mange marker, der har relevant afgrøde men 0 eller na i relevante U:9334 marker # OBS 2345 marker med 0 eller na i udbytte (25%), disse frasorteres i sidste kode! (13% af obs!).
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

display(dat_model)

# COMMAND ----------

### Klargøring til estimering ### (uden vår vinter split)
# -------------------------------------------------
# 1. Sikrer, at vi kun vælger et U_ for hver cvr for hver afgrødekategori (da vi i data har aggregerede udbytter for hver mark=række)
# -------------------------------------------------
output_farm_crop <- dat_model %>%
  group_by(cvr, crop_cat) %>%
  summarise(
    output = first(output),
    .groups = "drop"
  )

# -------------------------------------------------
# 2. KLARGØRING AF DATA TIL ESTIMERING
# -------------------------------------------------
dat_est <- dat_model %>%

  
  # areal pr. (cvr × crop × jordtype )
  group_by(cvr, crop_cat, jb3, main_catchment) %>% 
  summarise(
    area_j = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # total crop-areal pr. (cvr × crop)
  group_by(cvr, crop_cat, main_catchment) %>%
  mutate(
    total_area = sum(area_j),
    share = area_j / total_area
  ) %>%
  ungroup() %>%
  
  # join output (cvr × crop)
  left_join(output_farm_crop, by = c("cvr", "crop_cat"))

# -------------------------------------------------
# TÆL ANTAL OBSERVATIONER I DAT_EST # nu 1472 (Vi har nu et datasæt, hvor hver cvr har en obs for hver crop x jordtype)
# -------------------------------------------------
nrow(dat_est)

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

display(dat_est)

# COMMAND ----------

# -------------------------------------------------
# 4. Interactions FIXED EFFECTS MODEL 
# -------------------------------------------------
library(fixest)

modelny <- feols(
  ln_output_per_ha ~ i(crop_cat, jb3) | cvr + main_catchment,
  data = dat_est
)

etable(modelny)

# COMMAND ----------

par( mfrow = c(2,2) )
hist( dat_est$ln_output_per_ha, 50 )
hist( dat_est$ln_output_per_ha[ dat_est$crop_cat == "Hvede" ], 50 )
hist( dat_est$ln_output_per_ha[ dat_est$crop_cat == "Kartofler" ], 50 )