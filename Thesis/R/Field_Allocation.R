# Databricks notebook source
# installer R pakker
install.packages( "gtools" )
install.packages( "lmtest" )
install.packages( "miscTools" )
install.packages( "car" )
install.packages( "sandwich" )
install.packages( "plm" )
install.packages( "stargazer" )
install.packages( "patchwork" )

# indlæs R pakker
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "ggplot2" )
library( "gtools" )
library( "lmtest" )
library( "miscTools" )
library( "car" )
library( "sandwich" )
library( "plm" )
library( "stargazer" )
library( "patchwork" )

# indlæs R pakker - spatial analysis
system( "sudo apt-get update -qq && sudo apt-get install -y -qq libudunits2-dev libgdal-dev libgeos-dev # libproj-dev && sudo ldconfig", intern = TRUE )
install.packages( c( "Rcpp", "units" ), repos = "https://cloud.r-project.org" )
install.packages( "sf" )
library( "sf" )
install.packages( "terra" )
library( "terra" )

# COMMAND ----------

# stiveje
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# vælg model specifikation
obs <- "conv" # "eco" # "conv_crops" # "eco_crops" # "eco_mix" # "conv_mix" # "all"
iter <- 2000

# indlæs estimeret inputs og outputs
# datFarms <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )
datFarms <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, "_iter", iter, ".rds" ) ) ) # Serie B

# indlæs klarggjort markniveau datasæt
datFields <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_fields.rds" ) ) 
datFields <- st_drop_geometry( datFields )

# behold kun observationer i datasættene der er brugt i estimeringen af inputs
datFields <- datFields %>% semi_join( datFarms, by = "cvr" ) 

#### Forbereder kystvandoplande ####

## koder for de 11 kystvandoplande vi har data for:
# (111) Lister dyb, 
# (232) Nissum bredning
# (131) Nissum fjord, feldsted kog
# (236) Thisted bredning
# (235) Nibe bredning og Langerak
# (136) Randers fjord, indre
# (128) Horsens, Fj., indre
# (93) Odense Fj., Seden St.
# (165) Isefjord, indre
# (35) Karrebæk fjord
# (2) Roskilde fjord, indre
codeCatchment <- c( 111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2 )

# tilskriv ét hovedkystvandopland for alle marker inden for hvert cvr (dvs. det opland med størst mark areal)
datCatchment <- datFields %>%
filter( KystvandID %in% codeCatchment ) %>%
group_by( cvr, KystvandID ) %>%
summarise( total_area = sum( imk_areal, na.rm = TRUE ), 
           .groups = "drop" ) %>%
arrange( cvr, desc( total_area ) ) %>%
group_by( cvr ) %>%
slice( 1 ) %>%
ungroup() %>%
rename( main_catchment = KystvandID ) %>%
select( -total_area )

# COMMAND ----------

# full data set
nrow( datFields ) # 41781 fields
length( unique( datFields$cvr ) ) #  901 farms

# COMMAND ----------

#### Forbereder datasæt til estimering af udbytter ####

# definér mapping fra afgrødekategori til U_ variable
uToCrop <- tibble( uVar = paste0( "U_", 1:15 ),
                   crop_kat = c( "Byg", "Hvede", "Havre_blandet_korn",
                                 "Rug_Triticale", "Sukkerroer", "Frø", "Kartofler",
                                 "Raps", "Ærter_mv", "Grovfoder", "Fodermajs", "Energiafgrøder",
                                 "Gartneri", "Andre_industriafgrøder",
                                 "Andre_landbrugsindtægter" ) )

# beregn areal, jordtype-andele og udbytte per hektar
datYield <- datFields %>%
  group_by( cvr, crop_kat ) %>%
  summarise( total_area      = sum( imk_areal, na.rm = TRUE ),
             share_sand_fin  = sum( imk_areal[ jb3_kat == "sand_fin"  ], na.rm = TRUE ) / total_area,
             share_sand_grov = sum( imk_areal[ jb3_kat == "sand_grov" ], na.rm = TRUE ) / total_area,
             .groups = "drop" ) %>%
  left_join( datFarms %>%
               select( cvr, all_of( uToCrop$uVar ) ) %>%
               pivot_longer( all_of( uToCrop$uVar ), names_to = "uVar", values_to = "yield" ) %>%
               left_join( uToCrop, by = "uVar" ) %>%
               filter( yield > 0 ),
             by = c( "cvr", "crop_kat" ) ) %>%
  mutate( yield_ha = yield / total_area ) %>%
  select( cvr, crop_kat, share_sand_fin, share_sand_grov, yield, yield_ha, total_area ) %>%
  left_join( datCatchment, by = "cvr" )

# tilføj udbytter til mark-niveau datasæt
datFields <- datFields %>%
  left_join( datYield %>% select( cvr, crop_kat, yield, yield_ha ),
             by = c( "cvr", "crop_kat" ) )
datFields <- subset( datFields, !is.na( yield_ha ) & yield_ha > 0 )

# COMMAND ----------

nrow( datFields ) #  26417 fields 
length( unique( datFields$cvr ) ) # 896 farms

# COMMAND ----------

# beregn crop sub-kategori andele
datDummy <- datFields %>%
  group_by( cvr, crop_kat ) %>%
  summarise( ha_tot               = sum( imk_areal, na.rm = TRUE ),
             share_hesteboenner   = sum( imk_areal[ afgkode == 31 ], na.rm = TRUE ) / ha_tot,
             share_vinterbyg      = sum( imk_areal[ afgkode == 10 ], na.rm = TRUE ) / ha_tot,
             share_laegge_tidlige = sum( imk_areal[ afgkode %in% c( 149, 150, 157 ) ], na.rm = TRUE ) / ha_tot,
             share_spise          = sum( imk_areal[ afgkode == 152 ], na.rm = TRUE ) / ha_tot,
             share_havefroe       = sum( imk_areal[ afgkode %in% c( 113, 124, 666, 668 ) ], na.rm = TRUE ) / ha_tot,
             share_vaarhvede      = sum( imk_areal[ afgkode == 2 ], na.rm = TRUE ) / ha_tot,
             .groups = "drop" ) %>%
  select( -ha_tot )

datYield <- datYield %>%
  left_join( datDummy, by = c( "cvr", "crop_kat" ) ) %>%
  mutate( across( starts_with( "share_" ), ~ replace_na( .x, 0 ) ) )

# klargør datasæt til estimering
datYieldEst <- subset( datYield, !is.na( main_catchment ) & yield_ha < 600000 )

# definér rækkefølge efter crop area (faldende) med Hvede som base (første = reference)
crop_order <- c( "Hvede", "Byg", "Raps", "Fodermajs", "Rug_Triticale",
                "Grovfoder", "Frø", "Havre_blandet_korn", "Kartofler",
                "Ærter_mv", "Sukkerroer", "Energiafgrøder" )

# sæt faktor med Hvede som første niveau (= base kategori i R)
datYieldEst$crop_kat <- factor( datYieldEst$crop_kat, levels = crop_order )

# definér base (hvede) og øvrige kategorier 
cropBase <- levels( datYieldEst$crop_kat )[1] # hvede
cropRest <- levels( datYieldEst$crop_kat )[-1] # resten

# COMMAND ----------

# MAGIC %md
# MAGIC ## Descriptive Statistics

# COMMAND ----------

# beregn sd for alle variable
vars_sd <- c("share_sand_fin", "share_sand_grov",
             "share_hesteboenner", "share_vinterbyg", "share_laegge_tidlige",
             "share_spise", "share_havefroe", "share_vaarhvede")

sd_results <- sapply( vars_sd, function( v ) round( sd( datYieldEst[[ v ]], na.rm = TRUE ), 4 ) )

# inkl. log yield
sd_log_yield <- round( sd( log( datYieldEst$yield_ha ), na.rm = TRUE ), 2 )

# gem summary statistik som RDS
desc_stats <- data.frame(
  variable = c( "Log yield per hectare",
                "Fine sand", "Coarse sand",
                "Winter barley", "Fava beans",
                "Early/seed potatoes", "Consumption potatoes",
                "Horticultural seeds", "Spring wheat" ),
  mean = c(
    round( mean( log( datYieldEst$yield_ha ), na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_sand_fin,          na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_sand_grov,         na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_vinterbyg,         na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_hesteboenner,      na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_laegge_tidlige,    na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_spise,             na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_havefroe,          na.rm = TRUE ), 2 ),
    round( mean( datYieldEst$share_vaarhvede,         na.rm = TRUE ), 2 )
  ),
  median = c(
    round( median( log( datYieldEst$yield_ha ), na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_sand_fin,          na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_sand_grov,         na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_vinterbyg,         na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_hesteboenner,      na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_laegge_tidlige,    na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_spise,             na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_havefroe,          na.rm = TRUE ), 2 ),
    round( median( datYieldEst$share_vaarhvede,         na.rm = TRUE ), 2 )
  ),
  sd = c(
    sd_log_yield,
    round( sd( datYieldEst$share_sand_fin,          na.rm = TRUE ), 2 ),
    round( sd( datYieldEst$share_sand_grov,         na.rm = TRUE ), 2 ),
    round( sd( datYieldEst$share_vinterbyg,         na.rm = TRUE ), 2 ),
    round( sd( datYieldEst$share_hesteboenner,      na.rm = TRUE ), 2 ),
    round( sd( datYieldEst$share_laegge_tidlige,    na.rm = TRUE ), 2 ),
    round( sd( datYieldEst$share_spise,             na.rm = TRUE ), 2 ),
    round( sd( datYieldEst$share_havefroe,          na.rm = TRUE ), 2 ),
    round( sd( datYieldEst$share_vaarhvede,         na.rm = TRUE ), 2 )
  ),
  min = c(
    round( min( log( datYieldEst$yield_ha ), na.rm = TRUE ), 2 ),
    0, 0, 0, 0, 0, 0, 0, 0
  ),
  max = c(
    round( max( log( datYieldEst$yield_ha ), na.rm = TRUE ), 2 ),
    1, 1, 1, 1, 1, 1, 1, 1
  )
)

print( desc_stats )


# densitets fordelingen af log yield per hectare
histLogYield <- ggplot( datYield, aes(x = log(yield_ha))) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "grey", color = "white") +
   geom_density( color = "orange", linewidth = 0.5 ) +
  labs( title = "", x = "log( yield / ha )", y = "Density" ) +
  theme_bw()
ggsave( filename = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_logYield_", obs, ".png" ), plot = histLogYield, device = "png" )

# gem som RDS
saveRDS( desc_stats, paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table_stats_", obs, ".rds" )
)


# COMMAND ----------

# beregn stikprøve sammensætning
summary_by_crop <- datYieldEst %>%
  group_by(crop_kat) %>%
  summarise(
    `Obs.`                   = n(),
    `Crop area`              = round(sum(total_area, na.rm = TRUE), 0),
    `Avg. yield per hectare` = round(sum(yield_ha * total_area, na.rm = TRUE) /
                                     sum(total_area, na.rm = TRUE), 0)
  ) %>%
  arrange(desc(`Obs.`))

# tilføj totallinje i bunden
total_row <- datYieldEst %>%
  summarise(
    crop_kat                 = "Total",
    `Obs.`                   = n(),
    `Crop area`              = round(sum(total_area, na.rm = TRUE), 0),
    `Avg. yield per hectare` = round(sum(yield_ha * total_area, na.rm = TRUE) /
                                     sum(total_area, na.rm = TRUE), 0)
  )

sample_composition <- bind_rows(summary_by_crop, total_row)

# vis tabel
print(sample_composition, n = 25)

# gem som RDS
saveRDS( sample_composition,
  paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/sample_composition_", obs, ".rds" )
)
cat( "Stikprøve sammensætning gemt.\n" )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Test farm-level fixed effects vs. catchment fixed effects

# COMMAND ----------

#### Estimering: catchment FE vs CVR FE ####

# klargør panel datasæt
datYieldEst_plm  <- pdata.frame(  datYieldEst, c( "cvr", "crop_kat" )  )

# klargør panel datasæt med catchment som individual index
datYieldEst_plm_catch <- pdata.frame( datYieldEst, c( "main_catchment", "crop_kat" ) )

# fuld model med catchment FE
mYield_full_catch <- plm(
  log( yield_ha ) ~ ( share_sand_fin + share_sand_grov ) * crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm_catch,
  model  = "within",
  effect = "individual"
)

# fuld model uden catchment FE (pooling)
mYield_full_noCatch <- plm(
  log( yield_ha ) ~ ( share_sand_fin + share_sand_grov ) * crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm_catch,
  model  = "pooling"
)

# fuld model med CVR FE (datYieldEst_plm med cvr som individual index)
mYield_full_cvr <- plm(
  log( yield_ha ) ~ ( share_sand_fin + share_sand_grov ) * crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm,   # index = c("cvr", "crop_kat")
  model  = "within",
  effect = "individual"
)

# fuld model uden CVR FE (pooling)
mYield_full_noCvr <- plm(
  log( yield_ha ) ~ ( share_sand_fin + share_sand_grov ) * crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm,
  model  = "pooling"
)

#### Tests ####

# Hausman test: catchment FE vs CVR FE
# (tester om fixed effects er konsistente relativt til pooling)
cat( "\n--- Hausman test: Catchment FE ---\n" )
print( phtest( mYield_full_catch, mYield_full_noCatch ) )

cat( "\n--- Hausman test: CVR FE ---\n" )
print( phtest( mYield_full_cvr, mYield_full_noCvr ) )

# Hausman tests
hausman_catch <- phtest( mYield_full_catch, mYield_full_noCatch )
hausman_cvr   <- phtest( mYield_full_cvr,   mYield_full_noCvr   )

# saml resultater i en dataframe
hausman_results <- data.frame(
  test      = c( "Catchment fixed effects", "Farm fixed effects" ),
  chisq     = round( c( hausman_catch$statistic, hausman_cvr$statistic ), 3 ),
  df        = c( hausman_catch$parameter, hausman_cvr$parameter ),
  p_value   = round( c( hausman_catch$p.value,   hausman_cvr$p.value   ), 3 )
)

print( hausman_results )

# gem som RDS
saveRDS(
  hausman_results,
  paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hausman_feTests_", obs, ".rds" ) )
cat( "Hausman test resultater gemt.\n" )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Estimering af 4 udbytte modeller

# COMMAND ----------

#### Model estimering af udbytter - plm ####

# interaktionsled og CVR FE (within = demeaning over CVR)
mYield_int_cvr <- plm(
  log(yield_ha) ~ (share_sand_fin + share_sand_grov) * crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm,
  model  = "within",
  effect = "individual"
)

# interaktionsled og ingen CVR FE (pooled OLS)
mYield_int_noCvr <- plm(
  log(yield_ha) ~ (share_sand_fin + share_sand_grov) * crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm,
  model  = "pooling"
)

# ingen interaktionsled og CVR FE (within = demeaning over CVR)
mYield_noInt_cvr <- plm(
  log(yield_ha) ~ share_sand_fin + share_sand_grov + crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm,
  model  = "within",
  effect = "individual"
)

# ingen interaktionsled og ingen CVR FE (pooled OLS)
mYield_noInt_noCvr <- plm(
  log(yield_ha) ~ share_sand_fin + share_sand_grov + crop_kat +
    share_hesteboenner + share_vinterbyg + share_laegge_tidlige +
    share_spise + share_havefroe + share_vaarhvede,
  data   = datYieldEst_plm,
  model  = "pooling"
)


# funktion til at beregne nu værdier for en given model
calc_nu <- function( model, cropBase, cropRest ) {

  # base crop (Hvede) på de tre jordtyper
  base <- data.frame(
    crop_kat     = cropBase,
    nu_sand_fin  = exp( coef( model )[ "share_sand_fin"  ] ),
    nu_sand_grov = exp( coef( model )[ "share_sand_grov" ] ),
    nu_sand_ler  = exp( 0 )  # = 1 per definition
  )

  # øvrige afgrøder
  rest <- lapply( cropRest, function( crop ) {
    alpha_k <- coef( model )[ paste0( "crop_kat", crop ) ]

    # interaktionsled (NA hvis model uden interaktioner)
    delta_fin  <- coef( model )[ paste0( "share_sand_fin:crop_kat",  crop ) ]
    delta_grov <- coef( model )[ paste0( "share_sand_grov:crop_kat", crop ) ]

    data.frame(
      crop_kat     = crop,
      nu_sand_fin  = exp( alpha_k + coef( model )[ "share_sand_fin"  ] + delta_fin  ),
      nu_sand_grov = exp( alpha_k + coef( model )[ "share_sand_grov" ] + delta_grov ),
      nu_sand_ler  = exp( alpha_k )
    )
  })

  rbind( base, do.call( rbind, rest ) )
}

# funktion til at konvertere nu til procentvise ændringer relativt til
# wheat on loamy soil (= global baseline, nu = 1)
calc_pct <- function( nu_df ) {
  nu_df %>%
    mutate(
      pct_fin  = round( ( nu_sand_fin  - 1 ) * 100, 1 ),
      pct_grov = round( ( nu_sand_grov - 1 ) * 100, 1 ),
      pct_ler  = round( ( nu_sand_ler - 1 ) * 100, 1 )
    ) %>%
    select( crop_kat, pct_fin, pct_grov, pct_ler )
}

# beregn relative forventede udbytter (nu) for alle fire modeller
nu_int_cvr    <- calc_nu( mYield_int_cvr,    cropBase, cropRest )
nu_int_noCvr  <- calc_nu( mYield_int_noCvr,  cropBase, cropRest )
nu_noInt_cvr  <- calc_nu( mYield_noInt_cvr,  cropBase, cropRest )
nu_noInt_noCvr <- calc_nu( mYield_noInt_noCvr, cropBase, cropRest )

# beregn procent ændring i relative forventede udbytter (nu - 1)
pct_int_cvr    <- calc_pct( nu_int_cvr    )
pct_int_noCvr  <- calc_pct( nu_int_noCvr  )
pct_noInt_cvr  <- calc_pct( nu_noInt_cvr  )
pct_noInt_noCvr <- calc_pct( nu_noInt_noCvr )

# saml tabel 6: interaction models (svarer til Table 6 i billedet)
tab6 <- pct_int_cvr %>%
  rename( Fine_FE = pct_fin, Coarse_FE = pct_grov, Loamy_FE = pct_ler ) %>%
  left_join(
    pct_int_noCvr %>%
      rename( Fine_noFE = pct_fin, Coarse_noFE = pct_grov, Loamy_noFE = pct_ler ),
    by = "crop_kat"
  ) %>%
  filter( crop_kat != cropBase )  # fjern Hvede (= 0 for alle)

cat( "\nTable 6: Interaction Terms\n" )
print( tab6 )

# saml tabel 7: no interaction models (svarer til Table 7 i billedet)
# soil type effekter er de samme for alle afgrøder i no-interaction model
tab7_soil <- data.frame(
  crop_kat      = "All crop categories",
  Fine_FE       = round( ( exp( coef( mYield_noInt_cvr   )[ "share_sand_fin"  ] ) - 1 ) * 100, 1 ),
  Coarse_FE     = round( ( exp( coef( mYield_noInt_cvr   )[ "share_sand_grov" ] ) - 1 ) * 100, 1 ),
  Fine_noFE     = round( ( exp( coef( mYield_noInt_noCvr )[ "share_sand_fin"  ] ) - 1 ) * 100, 1 ),
  Coarse_noFE   = round( ( exp( coef( mYield_noInt_noCvr )[ "share_sand_grov" ] ) - 1 ) * 100, 1 )
)

tab7_crops <- pct_noInt_cvr %>%
  rename( Loamy_FE = pct_ler ) %>%
  select( crop_kat, Loamy_FE ) %>%
  left_join(
    pct_noInt_noCvr %>%
      rename( Loamy_noFE = pct_ler ) %>%
      select( crop_kat, Loamy_noFE ),
    by = "crop_kat"
  ) %>%
  filter( crop_kat != cropBase )

cat( "\nTable 7: No Interaction Terms - Soil effects\n" )
print( tab7_soil )
cat( "\nTable 7: No Interaction Terms - Crop intercepts\n" )
print( tab7_crops )


# Model 1: Interaction + CVR FE
stargazer(
  mYield_int_cvr,
  type          = "latex",
  out           = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/mYield_int_cvr_", obs, ".tex" ),
  title         = "Yield Model: Interaction Terms and CVR Fixed Effects",
  label         = "tab:yield-int-cvr",
  dep.var.label = "log(Yield per ha)",
  omit          = "cvr",
  omit.stat     = c("f", "ser"),
  add.lines     = list(
    c("CVR fixed effects",        "Yes"),
    c("Interaction: soil x crop", "Yes")
  ),
  notes     = "CVR fixed effects omitted from table.",
  float     = TRUE,
  font.size = "small"
)

# Model 2: Interaction, no CVR FE
stargazer(
  mYield_int_noCvr,
  type          = "latex",
  out           = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/mYield_int_noCvr_", obs, ".tex" ),
  title         = "Yield Model: Interaction Terms, No CVR Fixed Effects",
  label         = "tab:yield-int-nocvr",
  dep.var.label = "log(Yield per ha)",
  omit.stat     = c("f", "ser"),
  add.lines     = list(
    c("CVR fixed effects",        "No"),
    c("Interaction: soil x crop", "Yes")
  ),
  float     = TRUE,
  font.size = "small"
)

# Model 3: No interaction, CVR FE
stargazer(
  mYield_noInt_cvr,
  type          = "latex",
  out           = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/mYield_noInt_cvr_", obs, ".tex" ),
  title         = "Yield Model: No Interaction Terms, CVR Fixed Effects",
  label         = "tab:yield-noint-cvr",
  dep.var.label = "log(Yield per ha)",
  omit          = "cvr",
  omit.stat     = c("f", "ser"),
  add.lines     = list(
    c("CVR fixed effects",        "Yes"),
    c("Interaction: soil x crop", "No")
  ),
  notes     = "CVR fixed effects omitted from table.",
  float     = TRUE,
  font.size = "small"
)

# Model 4: No interaction, no CVR FE
stargazer(
  mYield_noInt_noCvr,
  type          = "latex",
  out           = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/mYield_noInt_noCvr_", obs, ".tex" ),
  title         = "Yield Model: No Interaction Terms, No CVR Fixed Effects",
  label         = "tab:yield-noint-nocvr",
  dep.var.label = "log(Yield per ha)",
  omit.stat     = c("f", "ser"),
  add.lines     = list(
    c("CVR fixed effects",        "No"),
    c("Interaction: soil x crop", "No")
  ),
  float     = TRUE,
  font.size = "small"
)

# gem tabeller med relative forventede udbytter for hver model
saveRDS( tab6,
  paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table_pred_yields_int_", obs, ".rds" )
)
saveRDS( list( soil = tab7_soil, crops = tab7_crops ),
  paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table_pred_yields_noInt_", obs, ".rds" )
)

# COMMAND ----------

summary( mYield_int_cvr )

# COMMAND ----------

summary( mYield_noInt_cvr )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Estimering af valgte udbytte model

# COMMAND ----------

#### Beregn predicted relative yields for mYield_int_cvr og allokér udbytter ####

# vælg den fortrukne model
mYield <- mYield_noInt_cvr

# beregn forventede udbytter for base kategorien K = hvede
resultsBase <- data.frame(
  crop_kat = cropBase,
  alpha_k = 0,
  me_fin   = round( coef( mYield )[ "share_sand_fin"  ], 4 ),
  me_grov  = round( coef( mYield )[ "share_sand_grov" ], 4 )
)

# beregn forventede udbytter for de resterende kategorier
results <- lapply( cropRest, function( crop ) {
  data.frame(
    crop_kat = crop,
    alpha_k = coef( mYield )[ paste0( "crop_kat", crop ) ],
    me_fin = coef( mYield )[ "share_sand_fin" ],
    me_grov = coef( mYield )[ "share_sand_grov" ]
  )
 } 
)

# saml resultater
resultsYield <- rbind( resultsBase, do.call( rbind, results ) )
rownames( resultsYield ) <- NULL
print( resultsYield )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Allokere udbytter

# COMMAND ----------

#### Beregn predicted relative yields og allokér udbytter ####

# beregn theta (predicted relative yields) fra resultsYield
datThetaLong <- resultsYield %>%
  mutate( theta_sand_fin  = exp( alpha_k + me_fin  ),
          theta_sand_grov = exp( alpha_k + me_grov ),
          theta_ler       = exp( alpha_k ) ) %>%
  select( crop_kat, theta_sand_fin, theta_sand_grov, theta_ler ) %>%
  pivot_longer(
    cols      = c( theta_sand_fin, theta_sand_grov, theta_ler ),
    names_to  = "jb3_kat_theta",
    values_to = "theta" ) %>%
  mutate( jb3_kat = case_when(
    jb3_kat_theta == "theta_sand_fin"  ~ "sand_fin",
    jb3_kat_theta == "theta_sand_grov" ~ "sand_grov",
    jb3_kat_theta == "theta_ler"       ~ "ler" ) ) %>%
  select( crop_kat, jb3_kat, theta )

# tilføj theta til mark-niveau datasæt
# NB: yield og yield_ha er allerede i datFields - ingen join med datYield nødvendig
datFields <- datFields %>%
  left_join( datThetaLong, by = c( "crop_kat", "jb3_kat" ) ) %>%
  rename( theta_yield = theta )

# ekstrahér rho koefficienter (crop sub-kategori effekter)
rhoVars <- c( "share_hesteboenner", "share_vinterbyg", "share_laegge_tidlige",
              "share_spise", "share_havefroe", "share_vaarhvede" )
rhoCoef <- coef( mYield )[ rhoVars ]
rhoCoef[ is.na( rhoCoef ) ] <- 0

# mapping fra afgkode til rho variabel
afgkodeToRho <- data.frame(
  afgkode = c( 10, 2, 31, 149, 150, 157, 152, 113, 124, 666, 668 ),
  rhoVar  = c( "share_vinterbyg", "share_vaarhvede", "share_hesteboenner",
               "share_laegge_tidlige", "share_laegge_tidlige", "share_laegge_tidlige",
               "share_spise", "share_havefroe", "share_havefroe",
               "share_havefroe", "share_havefroe" ),
  stringsAsFactors = FALSE )

# tilføj exp(rho) per mark baseret på afgkode
datFields <- datFields %>%
  left_join( afgkodeToRho, by = "afgkode" ) %>%
  mutate(
    rho_yield     = ifelse( !is.na( rhoVar ), rhoCoef[ rhoVar ], 0 ),
    exp_rho_yield = exp( rho_yield ) ) %>%
  select( -rhoVar )

# alloker udbytter til mark-niveau:
# hat{y}_{ikf} = y_{ik} * ( A_{ikf} * theta_{sf,k} * exp(rho_f) ) /
#                          sum_{f'} ( A_{ikf'} * theta_{sf',k} * exp(rho_{f'}) )
# hvor y_{ik} er det observerede totale udbytte (DKK) for farm i og afgrøde k
datFields <- datFields %>%
  group_by( cvr, crop_kat ) %>%
  mutate(
    alloc_yield    = yield *
                     ( imk_areal * theta_yield * exp_rho_yield /
                       sum( imk_areal * theta_yield * exp_rho_yield,
                            na.rm = TRUE ) ),
    alloc_yield_ha = alloc_yield / imk_areal ) %>%
  ungroup()

# tjek at allokering summerer til observeret farm-niveau udbytte
check <- datFields %>%
  group_by( cvr, crop_kat ) %>%
  summarise(
    alloc_sum  = sum( alloc_yield,  na.rm = TRUE ),
    farm_total = first( yield ),
    diff       = abs( alloc_sum - farm_total ),
    .groups    = "drop" ) %>%
  summarise(
    max_diff  = max(  diff, na.rm = TRUE ),
    mean_diff = mean( diff, na.rm = TRUE ),
    n_nonzero = sum(  diff > 0.01, na.rm = TRUE ) )

cat( "Tjek allokering - max_diff:",  check$max_diff,
                      " mean_diff:", check$mean_diff,
                      " n_nonzero:", check$n_nonzero, "\n" )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Tests for interaktionsled

# COMMAND ----------

# Wald tests for interaktionsled
wald_int_cvr   <- waldtest( mYield_noInt_cvr,   mYield_int_cvr   )
wald_int_noCvr <- waldtest( mYield_noInt_noCvr, mYield_int_noCvr )

# udtræk værdier baseret på faktisk struktur (Chisq, ikke F)
waldtest_results <- data.frame(
  test      = c( "Interaction terms, CVR FE", "Interaction terms, no CVR FE" ),
  chisq     = round( c( as.numeric( wald_int_cvr[2,   "Chisq"      ] ),
                        as.numeric( wald_int_noCvr[2, "Chisq"      ] ) ), 3 ),
  df        =        c( as.numeric( wald_int_cvr[2,   "Df"         ] ),
                        as.numeric( wald_int_noCvr[2, "Df"         ] ) ),
  p_value   = round( c( as.numeric( wald_int_cvr[2,   "Pr(>Chisq)" ] ),
                        as.numeric( wald_int_noCvr[2, "Pr(>Chisq)" ] ) ), 3 )
)

print( waldtest_results )

# gem som RDS
saveRDS(
  waldtest_results,
  paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/waldtest_intTerms_", obs, ".rds" )
)
cat( "Wald test resultater gemt.\n" )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Model evaluering

# COMMAND ----------

# udtræk residualer og fitted værdier
resid_df <- data.frame(
  fitted    = as.numeric(fitted(mYield_int_cvr)),
  residuals = as.numeric(residuals(mYield_int_cvr))
)

# Residuals vs Fitted
p1 <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Fitted values", y = "Residuals") +
  theme_bw()

# QQ-plot
p2 <- ggplot(resid_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.3, size = 0.8) +
  stat_qq_line(color = "red") +
  labs(title = "",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw()

# vis side om side
plotModel <- p1 + p2
ggsave( filename = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/figure_int_cvr_", obs, ".png" ), plot = plotModel, device = "png" )
plotModel

# COMMAND ----------

# udtræk residualer og fitted værdier
resid_df <- data.frame(
  fitted    = as.numeric(fitted(mYield_int_noCvr)),
  residuals = as.numeric(residuals(mYield_int_noCvr))
)

# Residuals vs Fitted
p1 <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Fitted values", y = "Residuals") +
  theme_bw()

# QQ-plot
p2 <- ggplot(resid_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.3, size = 0.8) +
  stat_qq_line(color = "red") +
  labs(title = "",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw()

# vis side om side
plotModel <- p1 + p2
ggsave( filename = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/figure_int_noCvr_", obs, ".png" ), plot = plotModel, device = "png" )
plotModel

# COMMAND ----------

# udtræk residualer og fitted værdier
resid_df <- data.frame(
  fitted    = as.numeric(fitted(mYield_noInt_cvr)),
  residuals = as.numeric(residuals(mYield_noInt_cvr))
)

# Residuals vs Fitted
p1 <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Fitted values", y = "Residuals") +
  theme_bw()

# QQ-plot
p2 <- ggplot(resid_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.3, size = 0.8) +
  stat_qq_line(color = "red") +
  labs(title = "",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw()

# vis side om side
plotModel <- p1 + p2
ggsave( filename = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/figure_noInt_cvr_", obs, ".png" ), plot = plotModel, device = "png" )
plotModel

# COMMAND ----------

# udtræk residualer og fitted værdier
resid_df <- data.frame(
  fitted    = as.numeric(fitted(mYield_noInt_noCvr)),
  residuals = as.numeric(residuals(mYield_noInt_noCvr))
)

# Residuals vs Fitted
p1 <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "",
       x = "Fitted values", y = "Residuals") +
  theme_bw()

# QQ-plot
p2 <- ggplot(resid_df, aes(sample = residuals)) +
  stat_qq(alpha = 0.3, size = 0.8) +
  stat_qq_line(color = "red") +
  labs(title = "",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_bw()

# vis side om side
plotModel <- p1 + p2
ggsave( filename = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/figure_noInt_noCvr_", obs, ".png" ), plot = plotModel, device = "png" )
plotModel

# COMMAND ----------

# MAGIC %md
# MAGIC ## Estimerer omkostninger.

# COMMAND ----------

#### Forbered datasæt til estimering af omkostninger/input ####

omkNames <- grep( "^OMK_\\d+_PG_\\d+$", names( datFarms ), value = TRUE )
datFarms  <- datFarms[ , c( "cvr", omkNames ) ]

datFarmsLong <- datFarms %>%
  pivot_longer( cols         = starts_with( "OMK_" ),
                names_to     = c( "inp_kat", "pg_kat" ),
                names_pattern = "^(OMK_\\d+)_(PG_\\d+)$",
                values_to    = "inp" ) %>%
  select( cvr, pg_kat, inp_kat, inp ) %>%
  left_join( aggregate( imk_areal ~ cvr + pg_kat, data = datFields, FUN = sum ) %>%
               rename( pg_area = imk_areal ),
             by = c( "cvr", "pg_kat" ) ) %>%
  mutate( pg_area = replace_na( pg_area, 0 ),
          inp_ha  = inp / pg_area )

# lav datasæt til wide format
datFarmsWide <- datFarmsLong %>%
  select( cvr, pg_kat, inp_kat, inp ) %>%
  pivot_wider( names_from = inp_kat, values_from = inp )

# tilføj omkostninger til markniveau datasæt
datFields <- datFields %>% left_join( datFarmsWide, by = c( "cvr", "pg_kat" ) )

# beregn jordandele for hver produktionsgren inden for hvert cvr
datInp <- datFields %>%
  group_by( cvr, pg_kat ) %>%
  summarise( total_area      = sum( imk_areal ),
             share_sand_fin  = sum( imk_areal[ jb3_kat == "sand_fin"  ] ) / total_area,
             share_sand_grov = sum( imk_areal[ jb3_kat == "sand_grov" ] ) / total_area,
             .groups = "drop" ) %>%
  left_join( datFarmsLong, by = c( "cvr", "pg_kat" ) ) %>%
  left_join( datCatchment, by = "cvr" ) %>%
  select( cvr, pg_kat, share_sand_fin, share_sand_grov, inp_kat, inp_ha, main_catchment )


#### Estimering af omkostninger/inputs ####

# fjern observationer med NA eller nul
datInp <- subset( datInp, !is.na( inp_ha ) & inp_ha > 0 )

# liste til at gemme resultater
allResults <- list()

# estimér model for hvert input med plm within transformation
for( inp in unique( datInp$inp_kat ) ) {

  datEst <- subset( datInp, inp_kat == inp )

  # sæt PG_1 som base kategori
  datEst$pg_kat <- relevel( factor( datEst$pg_kat ), ref = "PG_1" )

  # klargør panel datasæt
  datEst_plm <- pdata.frame( datEst, index = c( "cvr", "pg_kat" ) )

  # estimér model med within transformation
  mInp <- plm( log( inp_ha ) ~ pg_kat + share_sand_fin + share_sand_grov,
               data   = datEst_plm,
               model  = "within",
               effect = "individual" )

  # definér base og øvrige kategorier
  pgBase <- "PG_1"
  pgRest <- setdiff( levels( datEst$pg_kat ), pgBase )

  # beregn marginale effekter for base kategorien (alpha_j = 0 per definition)
  resultsBase <- data.frame(
    pg_kat  = pgBase,
    alpha_j = 0,
    me_fin  = round( coef( mInp )[ "share_sand_fin"  ], 4 ),
    me_grov = round( coef( mInp )[ "share_sand_grov" ], 4 ) )

  # beregn marginale effekter for øvrige kategorier
  results <- lapply( pgRest, function( pg ) {
    alpha_j <- coef( mInp )[ paste0( "pg_kat", pg ) ]
    me_fin  <- coef( mInp )[ "share_sand_fin"  ] 
    me_grov <- coef( mInp )[ "share_sand_grov" ] 

    data.frame( pg_kat  = pg,
                alpha_j = round( alpha_j, 4 ),
                me_fin  = round( me_fin,  4 ),
                me_grov = round( me_grov, 4 ) )
  } )

  # saml resultater
  resultTable <- rbind( resultsBase, do.call( rbind, results ) )
  rownames( resultTable ) <- NULL
  allResults[[ inp ]] <- resultTable

  cat( "\nInput:", inp, "\n" )
  print( resultTable )
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Allokere omkostninger

# COMMAND ----------

#### Alloker omkostninger/inputs ####

# theta = predicted relative input use (nu) per pg_kat og jordtype
# loamy soil (base): nu_{i3j} = exp( alpha_j ), nu_{i3J} = exp(0) = 1
# fine sand:         nu_{i1j} = exp( alpha_j + me_fin  )
# coarse sand:       nu_{i2j} = exp( alpha_j + me_grov )
datThetaLong <- bind_rows(
  lapply( names( allResults ), function( inp ) {
    allResults[[ inp ]] %>%
      mutate( inp_kat         = inp,
              theta_sand_fin  = exp( alpha_j + me_fin  ),
              theta_sand_grov = exp( alpha_j + me_grov ),
              theta_ler       = exp( alpha_j ) ) %>%
      select( inp_kat, pg_kat, theta_sand_fin, theta_sand_grov, theta_ler ) %>%
      pivot_longer( cols      = c( theta_sand_fin, theta_sand_grov, theta_ler ),
                    names_to  = "jb3_kat_theta",
                    values_to = "theta" ) %>%
      mutate( jb3_kat = case_when(
        jb3_kat_theta == "theta_sand_fin"  ~ "sand_fin",
        jb3_kat_theta == "theta_sand_grov" ~ "sand_grov",
        jb3_kat_theta == "theta_ler"       ~ "ler" ) ) %>%
      select( inp_kat, pg_kat, jb3_kat, theta )
  } )
)

# tilføj theta til mark-niveau datasæt
datFields <- datFields %>%
  left_join( datThetaLong %>%
               pivot_wider( names_from   = inp_kat,
                            values_from  = theta,
                            names_prefix = "theta_" ),
             by = c( "pg_kat", "jb3_kat" ) )

# allokér omkostninger
for( inp in names( allResults ) ) {

  datFields$theta_tmp <- datFields[[ paste0( "theta_", inp ) ]]

  datFields <- datFields %>%
    group_by( cvr, pg_kat ) %>%
    mutate( alloc_tmp = .data[[ inp ]] *
                        ( imk_areal * theta_tmp ) /
                        sum( imk_areal * theta_tmp ) ) %>%
    ungroup()

  datFields[[ paste0( "alloc_", inp ) ]] <- datFields$alloc_tmp
  datFields$alloc_tmp <- NULL
  datFields$theta_tmp <- NULL

  # tjek om mark-niveau estimater summerer til observerede omkostninger
  result <- datFields %>%
    group_by( cvr, pg_kat ) %>%
    summarise( check      = sum( .data[[ paste0( "alloc_", inp ) ]], na.rm = TRUE ),
               farm_total = first( .data[[ inp ]] ),
               diff       = abs( check - farm_total ),
               .groups    = "drop" ) %>%
    summarise( max_diff  = max(  diff, na.rm = TRUE ),
               mean_diff = mean( diff, na.rm = TRUE ),
               n_nonzero = sum(  diff > 0.01, na.rm = TRUE ) )

  cat( inp, " max_diff:", result$max_diff,
            " mean_diff:", result$mean_diff,
            " n_nonzero:", result$n_nonzero, "\n" )
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Beregner udbytter, omkostninger og DBII 'per hektar', og gemmer markniveau datasættet

# COMMAND ----------

# beregn omkostninger og udbytter per hektar på markniveau
qNames <- grep( "^alloc_OMK", names(datFields), value = TRUE )
for ( var in qNames ) {
 datFields[[ paste0( var, "_ha" ) ]] <- datFields[[ var ]] / datFields$imk_areal
}

# totale omkostninger og DBII per hektar per mark
datFields$alloc_costs <- with( datFields, alloc_OMK_1 + alloc_OMK_2 + alloc_OMK_3 + 
                                             alloc_OMK_4 + alloc_OMK_5 + alloc_OMK_6 + alloc_OMK_7 + alloc_OMK_8 + alloc_OMK_9 + alloc_OMK_11 )
datFields$alloc_costs_ha <- with( datFields, alloc_OMK_1_ha + alloc_OMK_2_ha + alloc_OMK_3_ha + 
                                             alloc_OMK_4_ha + alloc_OMK_5_ha + alloc_OMK_6_ha + alloc_OMK_7_ha + alloc_OMK_8_ha + alloc_OMK_9_ha + alloc_OMK_11_ha )
datFields$DBI_ha <- with( datFields, alloc_yield_ha - alloc_OMK_1_ha - alloc_OMK_2_ha - alloc_OMK_3_ha - 
                                     alloc_OMK_4_ha )                                           
datFields$DBII_ha <- with( datFields, alloc_yield_ha - alloc_costs_ha )

### gem data mark niveau data set
saveRDS( datFields, file.path( dirPath, paste0( "results/datResults_", obs, "_fields.rds" ) ) )