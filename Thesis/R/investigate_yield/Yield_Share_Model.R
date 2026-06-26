# Databricks notebook source
# indlæs R pakker - basale

library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library(purrr)
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
obs <- "conv_crops" # "eco_crops" #  # "eco_mix" # "conv_mix" # "all"

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

# Opretter dataframe dat_all, tilføjer nye variabelnavne, der matcher estimering - Hvede aggregeres over 4 afgrødekoder, fordi 3 ud af 4 er for små til at kunne bære selvstændig estimering. For raps, er vårraps ikke medtaget, da vi har nul observationer i vores datasæt. vårbyg og vinterbyg aggregeres for nu, men bør beholdes i seperate kategorier (brug evt. dummy her), da der er tilstrækkelige observationer. Jeg mangler at undersøge om rug_triticale kan disagregeres (om der er nok obs i de individuelle afgrødekategorier) - derudover kan vi medtage flere afgrøder fx: havre, majs, roer, kartofler kunne være relevante evt. flere. (tjek gerne hvilke afgrøder der er størst i data, så vi får de vigtigste med) 

dat_all <- datFields %>%
  mutate(
    crop_cat = case_when(
      afgkode %in% c(2, 11, 13) ~ "Hvede",
      afgkode %in% c(1, 10) ~ "Byg",
      afgkode %in% c(22) ~ "Raps", 
      afgkode %in% c(14, 15, 16) ~ "Rug_Triticale",
      afgkode %in% c(149, 150, 151, 152, 155, 156, 157) ~ "Kartofler", 
      # afgkode %in% c(3, 4) ~ "Havre_blandet_korn",
      afgkode %in% c(101, 102, 105, 107, 108, 111, 112, 113, 117, 120, 124, 655, 666, 668) ~ "Frø",
      #afgkode %in% c(250, 251, 252, 254, 255, 256, 257, 276, 286, 172, 174, 247, 260, 261, 263, 264, 266, 267, 268, 270, 285, 327, 210, 212, 213, 214, 701, 703, 709) ~ "Grovfoder", 
      #afgkode %in% c(216, 218) ~ "Fodermajs", 
      #afgkode %in% c(30, 31, 32) ~ "Ærter_mv", 
      #afgkode %in% c(160, 280) ~ "Sukkerroer",
      #afgkode %in% c(592, 593, 591, 596) ~ "Energiafgrøder", 
      #afgkode %in% c(429, 450, 401, 513, 524, 528) ~ "Gartneri", 
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
      #crop_cat == "Havre_blandet_korn" ~ U_3,
      crop_cat == "Rug_Triticale" ~ U_4,
      crop_cat == "Frø" ~ U_6,
      crop_cat == "Kartofler" ~ U_7,
      #crop_cat == "Grovfoder" ~ U_10,
      #crop_cat == "Fodermajs" ~ U_11,  
      #crop_cat == "Ærter_mv" ~ U_9,
      #crop_cat == "Sukkerroer" ~ U_5,
      #crop_cat == "Energiafgrøder" ~ U_12,
      #crop_cat == "Gartneri" ~ U_13,
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

### datasæt til estimering ###
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


dat_wide <- dat_est %>%
  select(cvr, crop_cat, main_catchment, jb3, share, ln_output_per_ha) %>%
  pivot_wider(
    names_from = jb3,
    values_from = share,
    names_prefix = "share_",
    values_fill = 0
  )

# COMMAND ----------

display(dat_wide)

# COMMAND ----------



dat_model %>%
  group_by(crop_cat, jb3) %>%
  summarise(area = sum(imk_areal, na.rm = TRUE), .groups = "drop") %>%
  group_by(crop_cat) %>%
  mutate(share = area / sum(area)) %>%
  arrange(crop_cat, desc(share))



# COMMAND ----------

mp <- feols(
  ln_output_per_ha ~ crop_cat | cvr,
  data = dat_wide,
  cluster = ~cvr
)

summary(mp)

# COMMAND ----------

mc <- feols(
  ln_output_per_ha ~ share_sand_fin + share_sand_grov + crop_cat | cvr,
  data = dat_wide,
  cluster = ~cvr
)

summary(mc)

# COMMAND ----------

# DBTITLE 1,e
dat_wide <- dat_wide %>%
  mutate(
    share_sand_fin  = as.numeric(share_sand_fin),
    share_sand_grov = as.numeric(share_sand_grov),
    crop_cat        = as.factor(crop_cat)
  )

m<-feols(
  ln_output_per_ha ~ (share_sand_fin + share_sand_grov) * crop_cat | cvr,
  data = dat_wide,
  cluster = ~cvr
)
summary(m)

# COMMAND ----------

dat_wide <- dat_wide %>%
  mutate(
    share_sand_fin  = as.numeric(share_sand_fin),
    share_sand_grov = as.numeric(share_sand_grov),
    crop_cat        = as.character(crop_cat)  # keep as character, not factor
  )

m <- lm(
  ln_output_per_ha ~ (share_sand_fin + share_sand_grov) * crop_cat + factor(cvr),
  data = dat_wide
)

# Marginal effects
crops <- c("Frø", "Hvede", "Kartofler", "Raps", "Rug_Triticale")

results <- lapply(crops, function(crop) {
  hyp <- c(
    paste0("share_sand_fin + share_sand_fin:crop_cat", crop, " = 0"),
    paste0("share_sand_grov + share_sand_grov:crop_cat", crop, " = 0")
  )
  test <- linearHypothesis(m, hyp,
    vcov = vcovCL(m, cluster = ~cvr, data = dat_wide))
  data.frame(
    crop         = crop,
    me_sand_fin  = coef(m)["share_sand_fin"] + coef(m)[paste0("share_sand_fin:crop_cat", crop)],
    me_sand_grov = coef(m)["share_sand_grov"] + coef(m)[paste0("share_sand_grov:crop_cat", crop)],
    F_stat       = test$F[2],
    p_value      = round(test$`Pr(>F)`[2], 5)
  )
})

result_table <- do.call(rbind, results)
rownames(result_table) <- NULL
result_table

# COMMAND ----------

m_restricted <- feols(
  ln_output_per_ha ~ crop_cat | cvr,
  data = dat_wide,
  cluster = ~cvr
)

summary(m_restricted)

# COMMAND ----------




models <- dat_wide %>%
  group_split(crop_cat) %>%
  map(~ lm(
    ln_output_per_ha ~ share_sand_fin + share_sand_grov,
    data = .x
  ))



models %>% map(summary)
