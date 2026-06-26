# Databricks notebook source
# installer R pakker
install.packages( "gtools" )
install.packages( "lmtest" )
install.packages( "miscTools" )
install.packages( "car" )
install.packages( "sandwich" )
install.packages("patchwork")

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
library( "gt" )
library( "patchwork" )

# indlæs R pakker - spatial analysis
# system( "sudo apt-get update -qq && sudo apt-get install -y -qq libudunits2-dev libgdal-dev libgeos-dev # libproj-dev && sudo ldconfig", intern = TRUE )
# install.packages( c( "Rcpp", "units" ), repos = "https://cloud.r-project.org" )
# install.packages( "sf" )
# library( "sf" )
# install.packages( "terra" )
# library( "terra" )

# COMMAND ----------

# stiveje
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# vælg model specifikation
obs <- "conv" # "conv_mix"  # "eco_crops" # "eco_mix"   # "all"

# indlæs klarggjort field-level datasæt med allokeret inputs og outputs (marker 26417; cvr 896 )
datFields <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, "_fields.rds" ) ) ) 

# indlæs vinterhvede genereret DBII mark data (marker 22036; cvr 841)
datVinter <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, "_refMap_vinter.rds" ) ) )

# indlæs vårbyg genereret DBII mark data (marker 22370; cvr  858)
datVaarbyg <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, "_refMap_vaarbyg.rds" ) ) )

# COMMAND ----------

nrow( datFields %>% filter( KystvandID %in% c( 131, 136, 128, 93, 165, 2 ) ) )

# COMMAND ----------

#### Beregner damage costs ####

# koder på hovede relevante kystvandoplande
codeCatchment <- c( 111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2 )
# 2 136 93 131 165 128

# brug kun observationer fra de 11 kystvandsoplande
datFields <- datFields %>% filter( KystvandID %in% codeCatchment )

## beregn damage costs på markniveau for vinterhvedeudvaskningskort ##
datFields <- datFields %>% mutate( 

  # andelen af udvasket kg N på hvedemarker
  udv_hvede_N_kg = imk_areal * udv_hvede,
 
  # andelen af udvasket kg N fra hvede marker til kystvande
  kystvand_N_kg_hvede = udv_hvede_N_kg * ( 1 - retention/100 ),
  
  # laver 'damage costs' til numeriske kolonner
  D_Cost_2024 = as.numeric( D_Cost_2024 ),
  ENA_mean_2024 = as.numeric( ENA_mean_2024 ),
  
  # beregner mark-specifikke 'damage costs' ved brug af variable- og uniforme sociale priser
  Specific_D_Cost_mark_hvede = kystvand_N_kg_hvede * D_Cost_2024,
  Uniform_D_Cost_mark_hvede = kystvand_N_kg_hvede * ENA_mean_2024,

  ## beregninger for Vårbyg - beregner damage costs på markniveau for vårbygudvaskningskort
  # andelen af udvasket kg N på vårbygmarker
  
  udv_byg_N_kg = imk_areal * ( udv_byg / (1-retention/100) ), # justér 'udv_byg' til 'kyst_udv_byd' i dat_prepared_analysis.R
  
  # andelen af udvasket kg N fra vårbyg-marker til kystvande
  
  kystvand_N_kg_byg = udv_byg_N_kg * ( 1 - retention/100 ),
  
  # beregner mark-specifikke 'damage costs' ved brug af variable- og uniforme sociale priser
  Specific_D_Cost_mark_byg = kystvand_N_kg_byg * D_Cost_2024,
  Uniform_D_Cost_mark_byg = kystvand_N_kg_byg * ENA_mean_2024
)

# tjek om udregning er korrekt
  stopifnot( all.equal( datFields$udv_byg * datFields$imk_areal, datFields$kystvand_N_kg_byg ) )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Vinterhvede og vaarbyg reference scenarier

# COMMAND ----------

par( mfrow = c(1,2))
hist( datVinter$specific_DBII_ha, 30 
hist( datVinter$uniform_DBII_ha, 30 )

# COMMAND ----------

# beregn sociale DBII med vinterhvede reference scenarie
datVinter$specific_DBII_ha <- datVinter$DBII_ref_Vinterhvede - datVinter$Specific_D_Cost_mark_hvede / datVinter$imk_areal
datVinter$uniform_DBII_ha <- datVinter$DBII_ref_Vinterhvede - datVinter$Uniform_D_Cost_mark_hvede / datVinter$imk_areal

# beregn sociale DBII med vårbyg reference scenarie
datVaarbyg$specific_DBII_ha <- datVaarbyg$DBII_ref_Vaarbyg - datVaarbyg$Specific_D_Cost_mark_byg / datVaarbyg$imk_areal
datVaarbyg$uniform_DBII_ha <- datVaarbyg$DBII_ref_Vaarbyg - datVaarbyg$Uniform_D_Cost_mark_byg / datVaarbyg$imk_areal

# antal minimum marker
nFields <- 20

# hjælpefunktion til areal-vægtet gennemsnit
weightedMean <- function( vals, area ) {
  obs <- !is.na( vals ) & !is.na( area )
  if ( sum(obs) == 0 ) return( NA )
  round( sum( vals[obs] * area[obs] ) / sum( area[obs] ), 2 )
}

# hjælpefunktion til areal-vægtet gennemsnit per jordtype
weightedMeanJB <- function( sub, colNames ) {
  sapply( c( "sand_fin", "sand_grov", "ler" ), function( soil )  {
    obs <- sub$jb3_kat == soil & !is.na( sub[[ colNames ]] )
    weightedMean( sub[[ colNames ]][ obs ], sub$imk_areal[ obs ])
  }
 )
}

# funktion til at beregne opsummeringstabel per kystvandopland
createTable <- function( dat ) {
  # fjerner marker med nul udbytte per hektar og fjerner ikke-relevante kystvandoplande
  dat <- subset( dat, alloc_yield_ha > 0 )

  # laver data frame med resultater per kystvandopland
  result <- do.call( rbind, lapply( codeCatchment, function( id ) {
    sub <- subset( dat, KystvandID == id )
    if ( nrow( sub ) == 0 ) return(NULL)
    
    # total number of fields
    n_total <- nrow( sub )
    
    # number of fields per soil type
    n_fin  <- sum( sub$jb3_kat == "sand_fin",  na.rm = TRUE )
    n_grov <- sum( sub$jb3_kat == "sand_grov", na.rm = TRUE )
    n_ler  <- sum( sub$jb3_kat == "ler",       na.rm = TRUE )
    
    # data frame
    data.frame(
      Kystvandopland          = unique( sub$KystvandNa ),
      n_fields                = nrow( sub ),
      n_finsand               = sum( sub$jb3_kat == "sand_fin",  na.rm = TRUE ),
      n_grovsand              = sum( sub$jb3_kat == "sand_grov", na.rm = TRUE ),
      n_lerjord               = sum( sub$jb3_kat == "ler",       na.rm = TRUE ),
      specific_DBII        = if ( n_total >= nFields ) weightedMean( sub$specific_DBII_ha, sub$imk_areal ) else NA,
      uniform_DBII        = if ( n_total >= nFields ) weightedMean( sub$uniform_DBII_ha, sub$imk_areal ) else NA,
      specific_finsand         = if ( n_fin  >= nFields ) weightedMeanJB( sub, "specific_DBII_ha" )[[ "sand_fin"  ]] else NA,
      specific_grovsand        = if ( n_grov >= nFields ) weightedMeanJB( sub, "specific_DBII_ha" )[[ "sand_grov" ]] else NA,
      specific_lerjord         = if ( n_ler  >= nFields ) weightedMeanJB( sub, "specific_DBII_ha" )[[ "ler"       ]] else NA,
      uniform_finsand         = if ( n_fin  >= nFields ) weightedMeanJB( sub, "uniform_DBII_ha" )[[ "sand_fin"  ]] else NA,
      uniform_grovsand        = if ( n_grov >= nFields ) weightedMeanJB( sub, "uniform_DBII_ha" )[[ "sand_grov" ]] else NA,
      uniform_lerjord         = if ( n_ler  >= nFields ) weightedMeanJB( sub, "uniform_DBII_ha" )[[ "ler"       ]] else NA
    )
  }))
  rownames(result) <- NULL
  return(result)
}

# beregn oplande-specifikke DBII for hver afgrøde
datVinterResult <- createTable( datVinter )
datVaarbygResult <- createTable( datVaarbyg )

# gem resultater
saveRDS( datVinterResult, paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/social_DBII_ref_vinterhvede_", obs, ".rds" ) )
saveRDS( datVinterResult, paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/social_DBII_ref_vaarbyg_", obs, ".rds" ) )

# COMMAND ----------

display( datVinterResult )
print( datVinterResult )

# COMMAND ----------

display( datVaarbygResult )
print( datVaarbygResult )

# COMMAND ----------

# "Red dashed line: catchment-level average private DBII (winter wheat reference)"
# "Red dashed line: catchment-level average private DBII (spring barley reference)"

#### Definer ønsket rækkefølge for oplande ####

order_uniform_hvede <- c(
  "Lister Dyb",
  "Nissum Bredning",
  "Nissum Fjord, Felsted Kog",
  "Thisted Bredning",
  "Nibe Bredning og Langerak",
  "Randers Fjord, indre",
  "Horsens Fjord, indre",
  "Odense Fjord, Seden Strand",
  "Isefjord, indre",
  "Karrebæk Fjord",
  "Roskilde Fjord, indre"
)

order_specific_hvede <- c(
  "Nissum Fjord, Felsted Kog",
  "Randers Fjord, indre",
  "Horsens Fjord, indre",
  "Odense Fjord, Seden Strand",
  "Isefjord, indre",
  "Roskilde Fjord, indre"
)

order_uniform_byg <- c(
  "Lister Dyb",
  "Nissum Bredning",
  "Nissum Fjord, Felsted Kog",
  "Thisted Bredning",
  "Nibe Bredning og Langerak",
  "Randers Fjord, indre",
  "Horsens Fjord, indre",
  "Odense Fjord, Seden Strand",
  "Isefjord, indre",
  "Karrebæk Fjord",
  "Roskilde Fjord, indre"
)

order_specific_byg <- c(
  "Nissum Fjord, Felsted Kog",
  "Randers Fjord, indre",
  "Horsens Fjord, indre",
  "Odense Fjord, Seden Strand",
  "Isefjord, indre",
  "Roskilde Fjord, indre"
)

#### Vinterhvede ####

# beregn gennemsnitlig privat DBII per opland (bruges som lodret linje)
datVinter_ref <- datVinter %>%
  group_by( KystvandNa ) %>%
  summarise(
    ref_DBII = sum( DBII_ref_Vinterhvede * imk_areal, na.rm = TRUE ) /
               sum( imk_areal, na.rm = TRUE ),
    .groups = "drop"
  )

# identificer oplande med mindst ét ikke-NA observationer
catchments_specific_hvede <- datVinter %>%
  filter( !is.na( specific_DBII_ha ) ) %>%
  distinct( KystvandNa )

# plot 'specifikke' fordelinger af DBII vinterhvede reference for hvert opland
hist_specific_DBII_hvede <- ggplot(
    datVinter %>%
      filter( !is.na( specific_DBII_ha ) ) %>%
      semi_join( catchments_specific_hvede, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_specific_hvede ) ),
    aes( x = specific_DBII_ha ) ) +
  geom_histogram( binwidth = 200, color = "black", fill = "steelblue" ) +
  geom_vline(
    data = datVinter_ref %>%
      semi_join( catchments_specific_hvede, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_specific_hvede ) ),
    aes( xintercept = ref_DBII ),
    color = "red", linetype = "dashed", linewidth = 0.8 ) +
  facet_wrap( ~ KystvandNa, scales = "free_y", axes = "all" ) +
  labs(
    x = "Profitability (DKK/ha/y)",
    y = "Number of Fields"
  ) +
  theme_bw()

# identificer oplande med mindst ét ikke-NA observationer
catchments_uniform_hvede <- datVinter %>%
  filter( !is.na( uniform_DBII_ha ) ) %>%
  distinct( KystvandNa )

# plot 'uniforme' fordelinger af DBII vinterhvede reference for hvert opland
hist_uniform_DBII_hvede <- ggplot(
    datVinter %>%
      filter( !is.na( uniform_DBII_ha ) ) %>%
      semi_join( catchments_uniform_hvede, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_uniform_hvede ) ),
    aes( x = uniform_DBII_ha ) ) +
  geom_histogram( binwidth = 400, color = "black", fill = "lightsteelblue" ) +
  geom_vline(
    data = datVinter_ref %>%
      semi_join( catchments_uniform_hvede, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_uniform_hvede ) ),
    aes( xintercept = ref_DBII ),
    color = "red", linetype = "dashed", linewidth = 0.8 ) +
  facet_wrap( ~ KystvandNa, scales = "free_y", ncol = 3, axes = "all" ) +
  labs(
    x = "Profitability (DKK/ha/y)",
    y = "Number of Fields"
  ) +
  theme_bw()

#### Vaarbyg ####

# beregn gennemsnitlig privat DBII per opland (bruges som lodret linje)
datVaarbyg_ref <- datVaarbyg %>%
  group_by( KystvandNa ) %>%
  summarise(
    ref_DBII = sum( DBII_ref_Vaarbyg * imk_areal, na.rm = TRUE ) /
               sum( imk_areal, na.rm = TRUE ),
    .groups = "drop"
  )

# identificer oplande med mindst ét ikke-NA observationer
catchments_specific_byg <- datVaarbyg %>%
  filter( !is.na( specific_DBII_ha ) ) %>%
  distinct( KystvandNa )

# plot 'specifikke' fordelinger af DBII vaarbyg reference for hvert opland
hist_specific_DBII_byg <- ggplot(
    datVaarbyg %>%
      filter( !is.na( specific_DBII_ha ) ) %>%
      semi_join( catchments_specific_byg, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_specific_byg ) ),
    aes( x = specific_DBII_ha ) ) +
  geom_histogram( binwidth = 100, color = "black", fill = "steelblue" ) +
  geom_vline(
    data = datVaarbyg_ref %>%
      semi_join( catchments_specific_byg, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_specific_byg ) ),
    aes( xintercept = ref_DBII ),
    color = "red", linetype = "dashed", linewidth = 0.8 ) +
  facet_wrap( ~ KystvandNa, scales = "free_y", axes = "all" ) +
  labs(
    x = "Profitability (DKK/ha/y)",
    y = "Number of Fields"
  ) +
  theme_bw()

# identificer oplande med mindst ét ikke-NA observationer
catchments_uniform_byg <- datVaarbyg %>%
  filter( !is.na( uniform_DBII_ha ) ) %>%
  distinct( KystvandNa )

# plot 'uniforme' fordelinger af DBII vaarbyg reference for hvert opland
hist_uniform_DBII_byg <- ggplot(
    datVaarbyg %>%
      filter( !is.na( uniform_DBII_ha ) ) %>%
      semi_join( catchments_uniform_byg, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_uniform_byg ) ),
    aes( x = uniform_DBII_ha ) ) +
  geom_histogram( binwidth = 200, color = "black", fill = "lightsteelblue" ) +
  geom_vline(
    data = datVaarbyg_ref %>%
      semi_join( catchments_uniform_byg, by = "KystvandNa" ) %>%
      mutate( KystvandNa = factor( KystvandNa, levels = order_uniform_byg ) ),
    aes( xintercept = ref_DBII ),
    color = "red", linetype = "dashed", linewidth = 0.8 ) +
  facet_wrap( ~ KystvandNa, scales = "free_y", ncol = 3, axes = "all" ) +
  labs(
    x = "Profitability (DKK/ha/y)",
    y = "Number of Fields"
  ) +
  theme_bw()

### Gem figurer

ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DB_specific_hvede_", obs, ".png"),
  plot = hist_specific_DBII_hvede,
  device = "png",
  width = 8,
  height = 6,
  dpi = 300
)
ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DB_uniform_hvede_", obs, ".png"),
  plot = hist_uniform_DBII_hvede,
  device = "png",
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DB_specific_byg_", obs, ".png"),
  plot = hist_specific_DBII_byg,
  device = "png",
  width = 8,
  height = 6,
  dpi = 300
)
ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DB_uniform_byg_", obs, ".png"),
  plot = hist_uniform_DBII_byg,
  device = "png",
  width = 8,
  height = 6,
  dpi = 300
)

# COMMAND ----------

display( datVinter )

# COMMAND ----------

hist_specific_DBII_hvede

# COMMAND ----------

hist_uniform_DBII_hvede

# COMMAND ----------

hist_specific_DBII_byg

# COMMAND ----------

hist_uniform_DBII_byg

# COMMAND ----------

# MAGIC %md
# MAGIC
# MAGIC # Laver datasæt: (i) Alle afgrøder (ii) alleafgrøder minus grovfoder (iii) individuelle afgrøder på faktisk dyrkede marker med pågældende afgrøde

# COMMAND ----------

# datasæt med hovedafgrøder
mainCrops <- c("Byg", "Hvede", "Havre_blandet_korn", "Rug_Triticale","Sukkerroer", "Frø", "Kartofler", "Raps", "Ærter_mv", 
                "Fodermajs","Grovfoder")
datFieldsMain <- subset( datFields, crop_kat %in% mainCrops )

# datasæt uden grovfoder
datFieldsnoRoughage  <- c("Byg", "Hvede", "Havre_blandet_korn", "Rug_Triticale","Sukkerroer", "Frø", "Kartofler", "Raps", "Ærter_mv", 
                "Fodermajs" )

# datasæt kun med grovfoder
datroughage  <- c("Grovfoder")
dathvede     <- c("Hvede")
datbyg       <- c("Byg")
datraps      <- c("Raps")
datmajs      <- c("Fodermajs")
datrug       <- c("Rug_Triticale")
datkartofler <- c("Kartofler")
datærter     <- c("Ærter_mv")
dathavre     <- c("Havre_blandet_korn")
datsukkerroer <- c("Sukkerroer")
datfrø       <- c("Frø")

# COMMAND ----------

# Antal obs i alt
nrow(datFieldsMain)



# COMMAND ----------

# MAGIC %md
# MAGIC ## Tabeller og histogrammer til opgaven

# COMMAND ----------

# MAGIC %md
# MAGIC # Tabel: vinterhvede (specific) vs. vinterhvede (uniform))

# COMMAND ----------

# Area Weighted DC

DC_uni_spec <- datFieldsMain %>%
  group_by(KystvandNa) %>%
  summarise(
    `Winter Wheat (Specific)` = sum(Specific_D_Cost_mark_hvede, na.rm = TRUE) / sum(imk_areal[!is.na(Specific_D_Cost_mark_hvede)], na.rm = TRUE),
    `Winter Wheat (Uniform)`  = sum(Uniform_D_Cost_mark_hvede, na.rm = TRUE) / sum(imk_areal[!is.na(Uniform_D_Cost_mark_hvede)], na.rm = TRUE)
  ) %>%
  rename(Catchment = KystvandNa)

DC_uni_spec %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 1
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "—"
  )

saveRDS(DC_uni_spec, paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table1_DC_", obs, ".rds"))

# COMMAND ----------

print(DC_uni_spec)

# COMMAND ----------

### Uniform damage cost - Vinterhvede - per catchment ###

hist_uniform_dcost_hvede <- ggplot(datFieldsMain, 
                              aes(x = Uniform_D_Cost_mark_hvede / imk_areal)) +
  geom_histogram(binwidth = 300, color = "black", fill = "lightsteelblue") +
  facet_wrap(~ KystvandNa, scales = "free_y", ncol = 3, axes = "all" ) +
  labs(
    x = "Uniform damage cost (DKK/ha/y)",
    y = "Number of Fields"
  
  ) +
  theme_bw()

### Specific damage cost - Vinterhvede - per catchment ###
hist_specific_dcost_hvede <- ggplot(datFieldsMain %>% filter(!is.na(Specific_D_Cost_mark_hvede)), 
                               aes(x = Specific_D_Cost_mark_hvede / imk_areal)) +
  geom_histogram(binwidth = 100, color = "black", fill = "steelblue") +
  facet_wrap(~ KystvandNa, scales = "free_y", axes = "all" ) +
  labs(
    x = "Specific damage cost (DKK/ha/y)",
    y = "Number of Fields"
  ) +
  theme_bw()

  ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DC_uni_hvede_", obs, ".png"),
  plot = hist_uniform_dcost_hvede,
  device = "png",
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DC_spec_hvede_", obs, ".png"),
  plot = hist_specific_dcost_hvede,
  device = "png",
  width = 12,
  height = 8,
  dpi = 300
)

# COMMAND ----------

print(hist_uniform_hvede)

# COMMAND ----------

print(hist_specific_hvede)

# COMMAND ----------

# MAGIC %md
# MAGIC # Histogram over markdistribution for alle observationer (specific/uniform)

# COMMAND ----------

### Samlet histogram - Uniform damage cost - alle catchments ###
hist_uniform_total <- ggplot(datFieldsMain, 
                             aes(x = Uniform_D_Cost_mark_hvede / imk_areal)) +
  geom_histogram(binwidth = 300, color = "black", fill = "lightsteelblue") +
  labs(
    x = "Uniform damage cost (DKK/ha/y)",
    y = "Number of fields",
  ) +
  theme_bw()

### Samlet histogram - Specific damage cost - alle catchments ###
hist_specific_total <- ggplot(datFieldsMain %>% filter(!is.na(Specific_D_Cost_mark_hvede)), 
                              aes(x = Specific_D_Cost_mark_hvede / imk_areal)) +
  geom_histogram(binwidth = 100, color = "black", fill = "steelblue") +
  labs(
    x = "Specific damage cost (DKK/ha/y)",
    y = "Number of fields",
  ) +
  theme_bw()

  ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DC_uni_total_", obs, ".png"),
  plot = hist_uniform_total,
  device = "png",
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_DC_spec_total_", obs, ".png"),
  plot = hist_specific_total,
  device = "png",
  width = 8,
  height = 6,
  dpi = 300
)

# COMMAND ----------

hist_uniform_total + hist_specific_total

# COMMAND ----------

cor(datFieldsMain$Uniform_D_Cost_mark_hvede, 
    datFieldsMain$Specific_D_Cost_mark_hvede, 
    use = "complete.obs")

# COMMAND ----------

# MAGIC %md
# MAGIC # Robustnesscheck I: vinterhvede vs. vårbyg

# COMMAND ----------

# Arealvægtede gennemsnit
table2 <- datFieldsMain %>%
  group_by(KystvandNa) %>%
  summarise(
    `Winter Wheat (Specific)`                = sum(Specific_D_Cost_mark_hvede, na.rm = TRUE) / sum(imk_areal[!is.na(Specific_D_Cost_mark_hvede)], na.rm = TRUE),
    `Spring Barley w. Catch Crop (Specific)` = sum(Specific_D_Cost_mark_byg, na.rm = TRUE) / sum(imk_areal[!is.na(Specific_D_Cost_mark_byg)], na.rm = TRUE),
    `Winter Wheat (Uniform)`                 = sum(Uniform_D_Cost_mark_hvede, na.rm = TRUE) / sum(imk_areal[!is.na(Uniform_D_Cost_mark_hvede)], na.rm = TRUE),
    `Spring Barley w. Catch Crop (Uniform)`  = sum(Uniform_D_Cost_mark_byg, na.rm = TRUE) / sum(imk_areal[!is.na(Uniform_D_Cost_mark_byg)], na.rm = TRUE)
  ) %>%
  rename(Catchment = KystvandNa)

table2 %>%
  gt() %>%
  tab_spanner(
    label = "Specific Damage Cost (kr/ha/y)",
    columns = c(`Winter Wheat (Specific)`, `Spring Barley w. Catch Crop (Specific)`)
  ) %>%
  tab_spanner(
    label = "Uniform Damage Cost (kr/ha/y)",
    columns = c(`Winter Wheat (Uniform)`, `Spring Barley w. Catch Crop (Uniform)`)
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 1
  ) %>%
  tab_caption("Main Crops: Area-weighted average damage cost in kr/ha/y: Winter Wheat vs Spring Barley with Catch Crop")

  saveRDS(table2, paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table2_DC_sensitivity_", obs, ".rds"))


# COMMAND ----------

print( as.data.frame( table2 ) )

# COMMAND ----------

# MAGIC %md
# MAGIC # Sensitititetsanalyse II sammenhæng afgrødetype og DC

# COMMAND ----------

datFieldsMain %>%
  group_by(KystvandNa, crop_kat) %>%
  summarise(n = n()) %>%
  arrange(KystvandNa, crop_kat) %>%
  print(n = 100)

# COMMAND ----------

make_dc_table <- function(data, crop_var, cost_type, col_name, min_obs = 0) {
  data %>%
    filter(crop_kat %in% crop_var) %>%
    group_by(KystvandNa) %>%
    summarise(
      n = n(),
      !!col_name := ifelse(
        n >= min_obs,
        sum(.data[[cost_type]], na.rm = TRUE) / sum(imk_areal[!is.na(.data[[cost_type]])], na.rm = TRUE),
        NA_real_
      )
    ) %>%
    select(-n) %>%
    rename(Catchment = KystvandNa)
}

# COMMAND ----------

# ---- UNIFORM ----

u_wheat      <- make_dc_table(datFieldsMain, dathvede,      "Uniform_D_Cost_mark_hvede", "Winter Wheat")
u_barley     <- make_dc_table(datFieldsMain, datbyg,        "Uniform_D_Cost_mark_hvede", "Spring Barley")
u_rape       <- make_dc_table(datFieldsMain, datraps,       "Uniform_D_Cost_mark_hvede", "Rapeseed")
u_maize      <- make_dc_table(datFieldsMain, datmajs,       "Uniform_D_Cost_mark_hvede", "Maize")
u_rye        <- make_dc_table(datFieldsMain, datrug,        "Uniform_D_Cost_mark_hvede", "Rye/Triticale")
u_potato     <- make_dc_table(datFieldsMain, datkartofler,  "Uniform_D_Cost_mark_hvede", "Potatoes")
u_peas       <- make_dc_table(datFieldsMain, datærter,      "Uniform_D_Cost_mark_hvede", "Peas")
u_oats       <- make_dc_table(datFieldsMain, dathavre,      "Uniform_D_Cost_mark_hvede", "Oats/Mixed Cereals")
u_beet       <- make_dc_table(datFieldsMain, datsukkerroer, "Uniform_D_Cost_mark_hvede", "Sugar Beet")
u_seeds      <- make_dc_table(datFieldsMain, datfrø,        "Uniform_D_Cost_mark_hvede", "Seeds")
u_roughage   <- make_dc_table(datFieldsMain, datroughage,   "Uniform_D_Cost_mark_hvede", "Roughage")

table_uniform_full <- u_wheat %>%
  full_join(u_barley,   by = "Catchment") %>%
  full_join(u_rape,     by = "Catchment") %>%
  full_join(u_maize,    by = "Catchment") %>%
  full_join(u_rye,      by = "Catchment") %>%
  full_join(u_potato,   by = "Catchment") %>%
  full_join(u_peas,     by = "Catchment") %>%
  full_join(u_oats,     by = "Catchment") %>%
  full_join(u_beet,     by = "Catchment") %>%
  full_join(u_seeds,    by = "Catchment") %>%
  full_join(u_roughage, by = "Catchment")

# ---- SPECIFIC ----

s_wheat      <- make_dc_table(datFieldsMain, dathvede,      "Specific_D_Cost_mark_hvede", "Winter Wheat")
s_barley     <- make_dc_table(datFieldsMain, datbyg,        "Specific_D_Cost_mark_hvede", "Spring Barley")
s_rape       <- make_dc_table(datFieldsMain, datraps,       "Specific_D_Cost_mark_hvede", "Rapeseed")
s_maize      <- make_dc_table(datFieldsMain, datmajs,       "Specific_D_Cost_mark_hvede", "Maize")
s_rye        <- make_dc_table(datFieldsMain, datrug,        "Specific_D_Cost_mark_hvede", "Rye/Triticale")
s_potato     <- make_dc_table(datFieldsMain, datkartofler,  "Specific_D_Cost_mark_hvede", "Potatoes")
s_peas       <- make_dc_table(datFieldsMain, datærter,      "Specific_D_Cost_mark_hvede", "Peas")
s_oats       <- make_dc_table(datFieldsMain, dathavre,      "Specific_D_Cost_mark_hvede", "Oats/Mixed Cereals")
s_beet       <- make_dc_table(datFieldsMain, datsukkerroer, "Specific_D_Cost_mark_hvede", "Sugar Beet")
s_seeds      <- make_dc_table(datFieldsMain, datfrø,        "Specific_D_Cost_mark_hvede", "Seeds")
s_roughage   <- make_dc_table(datFieldsMain, datroughage,   "Specific_D_Cost_mark_hvede", "Roughage")

table_specific_full <- s_wheat %>%
  full_join(s_barley,   by = "Catchment") %>%
  full_join(s_rape,     by = "Catchment") %>%
  full_join(s_maize,    by = "Catchment") %>%
  full_join(s_rye,      by = "Catchment") %>%
  full_join(s_potato,   by = "Catchment") %>%
  full_join(s_peas,     by = "Catchment") %>%
  full_join(s_oats,     by = "Catchment") %>%
  full_join(s_beet,     by = "Catchment") %>%
  full_join(s_seeds,    by = "Catchment") %>%
  full_join(s_roughage, by = "Catchment")

# ---- KORT TABEL TIL RAPPORT ----
table_uniform_short <- u_wheat %>%
  full_join(u_barley,   by = "Catchment") %>%
  full_join(u_roughage, by = "Catchment")

table_specific_short <- s_wheat %>%
  full_join(s_barley,   by = "Catchment") %>%
  full_join(s_roughage, by = "Catchment")

# ---- PRINT ----

# Kort tabel - rapport
table_uniform_short %>%
  gt() %>%
  
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "—")

table_specific_short %>%
  gt() %>%
  tab_header(
    title = "Specific Damage Cost in DKK/ha/y",
    subtitle = "Under Winter Wheat Reference Scenario"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "—")

# Fuld tabel - appendix
table_uniform_full %>%
  gt() %>%
  
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "—")

table_specific_full %>%
  gt() %>%
  
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "—")


saveRDS(table_uniform_short, paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table_uniform_short_", obs, ".rds"))
saveRDS(table_specific_short, paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table_specific_short_", obs, ".rds"))
saveRDS(table_uniform_full, paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table_uniform_full_", obs, ".rds"))
saveRDS(table_specific_full, paste0("/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/table_specific_full_", obs, ".rds"))

# COMMAND ----------

# Fuld tabel - appendix
table_uniform_full %>%
  gt() %>%

  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "—")

# COMMAND ----------

table_short <- u_wheat %>%
  full_join(u_barley,   by = "Catchment") %>%
  full_join(u_rape,     by = "Catchment") %>%
  full_join(u_roughage, by = "Catchment") %>%
  full_join(
    s_wheat %>%
      full_join(s_barley,   by = "Catchment") %>%
      full_join(s_rape,     by = "Catchment") %>%
      full_join(s_roughage, by = "Catchment"),
    by = "Catchment",
    suffix = c(" ", "  ")
  )

table_short <- table_short %>%
  gt() %>%
  tab_spanner(
    label = "Uniform Damage Cost (DKK/ha/y)",
    columns = 2:5
  ) %>%
  tab_spanner(
    label = "Specific Damage Cost (DKK/ha/y)",
    columns = 6:9
  )  %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  fmt_missing(columns = everything(), missing_text = "—")

# COMMAND ----------

print( table_short )