# Databricks notebook source
# installer R pakker
install.packages( "lmtest" )
install.packages( "miscTools" )

# indlæs R pakker
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "ggplot2" )
library( "lmtest" )
library( "miscTools" )

# COMMAND ----------

# stiveje
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# vælg model specifikation
obs <- "conv" # "conv_crops"
iter <- 2000

# indlæs klarggjort field-level datasæt
datFields <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, "_fields.rds" ) ) ) 

# indlæs estimeret inputs og outputs
datFarms <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, "_iter", iter, ".rds" ) ) ) # Serie B
datFarms <- datFarms %>% semi_join( datFields, by = "cvr" ) 

# COMMAND ----------

codeCatchment <- c( 111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2 )

# full data set
nrow( datFields ) #  26417 fields
length( unique(datFields$cvr) ) # 896 farms
length( unique( datFields$KystvandID ) ) # 65 catchments

# only for our 11 catchments
nrow( subset( datFields, KystvandID %in% codeCatchment ) ) # 23336 fields
length( unique( datFields$cvr[ datFields$KystvandID %in% codeCatchment ] ) ) # 894 farms

# COMMAND ----------

# MAGIC %md
# MAGIC # Beregner areal-vægtet udbytter, omkostninger, DB og sociale DB på tværs af kystvandoplande og jordtyper.

# COMMAND ----------

# koder på hovede relevante kystvandoplande
codeCatchment <- c( 111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2 )

# brug kun observationer fra de 11 kystvandsoplande
datFields <- datFields %>% filter( KystvandID %in% codeCatchment )

# brug kun observationer med positive værdier for N_udbragt_Ialt
datFields <- datFields %>% filter( !is.na( N_udbragt_total ) & N_udbragt_total > 0 )

## beregn damage costs på markniveau for vinterhvedeudvaskningskort ##
datFields <- datFields %>% mutate( 

  # andelen af udvasket kg N på vårbygmarker
  udv_hvede_N_kg = imk_areal * udv_hvede,
  # udv_hvede_N_kg_kor = udv_hvede_N_kg + 0.17 * (alloc_Q_1 - alloc_Q_2), # marginal leaching estimate: 0.17.
 
  # andelen af udvasket kg N fra hvede marker til kystvande
  kystvand_N_kg_hvede = udv_hvede_N_kg * ( 1 - retention/100 ),
  # kystvand_N_kg_hvede_kor = udv_hvede_N_kg_kor * ( 1 - retention/100 ),
  
  # laver 'damage costs' til numeriske kolonner
  D_Cost_2024 = as.numeric( D_Cost_2024 ),
  ENA_mean_2024 = as.numeric( ENA_mean_2024 ),
  
  # beregner mark-specifikke 'damage costs' ved brug af variable- og uniforme sociale priser
  Specific_D_Cost_mark_hvede = kystvand_N_kg_hvede * D_Cost_2024,
  Uniform_D_Cost_mark_hvede = kystvand_N_kg_hvede * ENA_mean_2024,
  # Specific_D_Cost_mark_hvede_kor = kystvand_N_kg_hvede_kor * D_Cost_2024,
  # Uniform_D_Cost_mark_hvede_kor = kystvand_N_kg_hvede_kor * ENA_mean_2024,

  ## beregninger for Vårbyg - beregner damage costs på markniveau for vårbygudvaskningskort
  # andelen af udvasket kg N på vårbygmarker
  
  udv_byg_N_kg = imk_areal * ( udv_byg / (1-retention/100) ), # justér 'udv_byg' til 'kyst_udv_byd' i dat_prepared_analysis.R

  # udv_byg_N_kg_kor = udv_byg_N_kg + 0.17*(alloc_Q_1 - alloc_Q_2),
  
  # andelen af udvasket kg N fra vårbyg-marker til kystvande
  
  kystvand_N_kg_byg = udv_byg_N_kg * ( 1 - retention/100 ),
  
  # kystvand_N_kg_byg_kor = udv_byg_N_kg_kor * ( 1 - retention/100 ),

  # beregner mark-specifikke 'damage costs' ved brug af variable- og uniforme sociale priser
  Specific_D_Cost_mark_byg = kystvand_N_kg_byg * D_Cost_2024,
  Uniform_D_Cost_mark_byg = kystvand_N_kg_byg * ENA_mean_2024,
  # Specific_D_Cost_mark_byg_kor = kystvand_N_kg_byg_kor * D_Cost_2024,
  # Uniform_D_Cost_mark_byg_kor = kystvand_N_kg_byg_kor * ENA_mean_2024
)

# COMMAND ----------

# antal minimum marker
nFields <- 20

# laver individuelle datasæt for hver afgrøde kategori
datFieldsVaarByg <- subset( datFields, afgkode == 1 )
datFieldsVinterByg <- subset( datFields, afgkode == 10 )
datFieldsVinterhvede <- subset( datFields, afgkode == 11 )
datFieldsKartofler <- subset( datFields, crop_kat == "Kartofler" )
datFieldsBland <- subset( datFields, crop_kat == "Havre_blandet_korn" )
datFieldsRaps <- subset( datFields, crop_kat == "Raps" )
datFieldsFroe <- subset( datFields, crop_kat == "Frø" )
datFieldsMajs <- subset( datFields, crop_kat == "Fodermajs" )
datFieldsAerter <- subset( datFields, crop_kat == "Ærter_mv" )

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
      yield_ha                = if ( n_total >= nFields ) weightedMean( sub$alloc_yield_ha, sub$imk_areal ) else NA,
      costs_ha                = if ( n_total >= nFields ) weightedMean( sub$alloc_costs_ha, sub$imk_areal ) else NA,
      DBII_ha                 = if ( n_total >= nFields ) weightedMean( sub$DBII_ha, sub$imk_areal ) else NA,
      DBII_ha_min             = if ( n_total >= nFields ) min( sub$DBII_ha, na.rm = TRUE ) else NA,
      DBII_ha_max             = if ( n_total >= nFields ) max( sub$DBII_ha, na.rm = TRUE ) else NA,
      yield_ha_finsand        = if ( n_fin  >= nFields ) weightedMeanJB( sub, "alloc_yield_ha" )[[ "sand_fin"  ]] else NA,
      yield_ha_grovsand       = if ( n_grov >= nFields ) weightedMeanJB( sub, "alloc_yield_ha" )[[ "sand_grov" ]] else NA,
      yield_ha_lerjord        = if ( n_ler  >= nFields ) weightedMeanJB( sub, "alloc_yield_ha" )[[ "ler"       ]] else NA,
      costs_ha_finsand        = if ( n_fin  >= nFields ) weightedMeanJB( sub, "alloc_costs_ha" )[[ "sand_fin"  ]] else NA,
      costs_ha_grovsand       = if ( n_grov >= nFields ) weightedMeanJB( sub, "alloc_costs_ha" )[[ "sand_grov" ]] else NA,
      costs_ha_lerjord        = if ( n_ler  >= nFields ) weightedMeanJB( sub, "alloc_costs_ha" )[[ "ler"       ]] else NA,
      DBII_ha_finsand         = if ( n_fin  >= nFields ) weightedMeanJB( sub, "DBII_ha" )[[ "sand_fin"  ]] else NA,
      DBII_ha_grovsand        = if ( n_grov >= nFields ) weightedMeanJB( sub, "DBII_ha" )[[ "sand_grov" ]] else NA,
      DBII_ha_lerjord         = if ( n_ler  >= nFields ) weightedMeanJB( sub, "DBII_ha" )[[ "ler"       ]] else NA,
      # min and max per soil type
      DBII_ha_min_finsand     = if ( n_fin  >= nFields ) min( sub$DBII_ha[ sub$jb3_kat == "sand_fin"  ], na.rm = TRUE ) else NA,
      DBII_ha_min_grovsand    = if ( n_grov >= nFields ) min( sub$DBII_ha[ sub$jb3_kat == "sand_grov" ], na.rm = TRUE ) else NA,
      DBII_ha_min_lerjord     = if ( n_ler  >= nFields ) min( sub$DBII_ha[ sub$jb3_kat == "ler"       ], na.rm = TRUE ) else NA,
      DBII_ha_max_finsand     = if ( n_fin  >= nFields ) max( sub$DBII_ha[ sub$jb3_kat == "sand_fin"  ], na.rm = TRUE ) else NA,
      DBII_ha_max_grovsand    = if ( n_grov >= nFields ) max( sub$DBII_ha[ sub$jb3_kat == "sand_grov" ], na.rm = TRUE ) else NA,
      DBII_ha_max_lerjord     = if ( n_ler  >= nFields ) max( sub$DBII_ha[ sub$jb3_kat == "ler"       ], na.rm = TRUE ) else NA

    )
  }))
  rownames(result) <- NULL
  return(result)
}

# beregn oplande-specifikke DBII for hver afgrøde
datVaarByg <- createTable( datFieldsVaarByg )
datVinterByg <- createTable( datFieldsVinterByg )
datVinterhvede <- createTable( datFieldsVinterhvede )
datKartofler <- createTable( datFieldsKartofler )
datBland <- createTable( datFieldsBland )
datRaps <- createTable( datFieldsRaps )
datFroe <- createTable( datFieldsFroe )
datMajs <- createTable( datFieldsMajs )
datAerter <- createTable( datFieldsAerter )

# COMMAND ----------

display( datVinterhvede )
print( datVinterhvede )

# COMMAND ----------

display( datVaarByg )
print( datVaarByg )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Undersøger potentille outliers

# COMMAND ----------

# konfiguration - brug navne der er nemmere at arbejde med
cropConfig <- list(
  VaarByg            = list( datFields = "datFieldsVaarByg",   pg = "PG_1",  uVar = "U_1", afgkode = 1 ),
  VinterByg          = list( datFields = "datFieldsVinterByg", pg = "PG_2",  uVar = "U_1", afgkode = 10 ),
  VinterHvede        = list( datFields = "datFieldsVinterhvede", pg = "PG_3",  uVar = "U_2", afgkode = 11 )
)
cvrOutList <- list()

# COMMAND ----------

#### VINTERHVEDE ####
crop <- "VinterHvede"
cfg  <- cropConfig[[ crop ]]
datCrop <- subset( get( cfg$datFields ), alloc_yield_ha > 0 & !is.na( alloc_yield_ha ) )
log_omk   <- log( with( datCrop, alloc_OMK_1_ha + alloc_OMK_2_ha + alloc_OMK_3_ha + alloc_OMK_4_ha + alloc_OMK_5_ha + alloc_OMK_6_ha + alloc_OMK_7_ha + alloc_OMK_8_ha + alloc_OMK_9_ha  + alloc_OMK_11_ha ) )
log_yield <- log( datCrop$alloc_yield_ha )
ratio     <- log_yield - log_omk
obs <- !is.na( ratio ) & !is.na( log_omk )
log_omk <- log_omk[ obs ]; log_yield <- log_yield[ obs ]; ratio <- ratio[ obs ]; datCrop <- datCrop[ obs, ]
lsfenceLower <- quantile( ratio, 0.25 ) - 1.5 * IQR( ratio )
lsfenceUpper <- quantile( ratio, 0.75 ) + 1.5 * IQR( ratio )
obsOut  <- which( ratio < lsfenceLower | ratio > lsfenceUpper )
cvrOut  <- sort( unique( datCrop$cvr[ obsOut ] ) )
cvrOutList[[ crop ]] <- cvrOut

# rapportér outliers
cat( "Outliers (marker):", length( obsOut ), ". Outliers (bedrifter):", length( cvrOut ), ". Unikke CVR:\n" )
print( cvrOut )

# vis kystvandoplande for outlier marker
if ( length( obsOut ) > 0 ) {
  cat( "\nKystvandoplande for outlier marker:\n" )
  print( table( datCrop$KystvandNa[ obsOut ] ) )
  cat( "\nDetaljer for outlier marker:\n" )
  print( as.data.frame( unique( datCrop[ obsOut, c( "cvr", "KystvandNa", "jb3_kat", "imk_areal" ) ] ) ) )
}

# farm-niveau
omkNames       <- paste0( "OMK_", 1:9, "_", cfg$pg )
log_omk_farm   <- log( rowSums( datFarms[, omkNames ], na.rm = TRUE ) )
log_yield_farm <- log( datFarms[[ cfg$uVar ]] )
obsOutFarms    <- datFarms$cvr %in% cvrOut

# plot
compPlot( log_omk, log_yield, xlab = "log( costs / ha )", ylab = "log( yields / ha )", main = "" )
if ( length( obsOut ) > 0 ) points( log_omk[ obsOut ], log_yield[ obsOut ], col = "red", pch = 1 )

# COMMAND ----------

#### VINTERHVEDE: OUTLIER IDENTIFICATION ####

# set standard deviation threshold
SD <- 2

# beregn areal-vægtet catchment-gennemsnit
catchment_avg <- datFieldsVinterhvede %>%
  group_by( KystvandNa ) %>%
  summarise(
    catchment_mean_yield = weighted.mean( alloc_yield_ha, imk_areal, na.rm = TRUE ),
    catchment_mean_costs = weighted.mean( alloc_costs_ha, imk_areal, na.rm = TRUE ),
    catchment_mean_dbii  = weighted.mean( DBII_ha,        imk_areal, na.rm = TRUE ),
    .groups = "drop"
  )

# beregn farm-niveau gennemsnit og sammenlign med catchment-gennemsnit
outlier_comparison <- datFieldsVinterhvede %>%
  group_by( cvr ) %>%
  summarise(
    catchment        = first( KystvandNa ),
    n_fields         = n(),
    mean_yield_obs   = weighted.mean( yield_ha,       imk_areal, na.rm = TRUE ),
    mean_yield_alloc = weighted.mean( alloc_yield_ha, imk_areal, na.rm = TRUE ),
    mean_costs_alloc = weighted.mean( alloc_costs_ha, imk_areal, na.rm = TRUE ),
    mean_dbii        = weighted.mean( DBII_ha,        imk_areal, na.rm = TRUE ),
    .groups = "drop"
  ) %>%
  left_join( catchment_avg, by = c( "catchment" = "KystvandNa" ) ) %>%
  mutate(
    yield_diff = mean_yield_alloc - catchment_mean_yield,
    costs_diff = mean_costs_alloc - catchment_mean_costs,
    dbii_diff  = mean_dbii        - catchment_mean_dbii
  ) %>%
  arrange( desc( costs_diff ) )

# beregn cutoffs
costs_high_cutoff <- mean( outlier_comparison$costs_diff, na.rm = TRUE ) +
                     SD  * sd( outlier_comparison$costs_diff, na.rm = TRUE )
costs_low_cutoff  <- mean( outlier_comparison$costs_diff, na.rm = TRUE ) -
                     SD  * sd( outlier_comparison$costs_diff, na.rm = TRUE )
yield_high_cutoff <- mean( outlier_comparison$yield_diff, na.rm = TRUE ) +
                     SD  * sd( outlier_comparison$yield_diff, na.rm = TRUE )
yield_low_cutoff  <- mean( outlier_comparison$yield_diff, na.rm = TRUE ) -
                     SD  * sd( outlier_comparison$yield_diff, na.rm = TRUE )

cat( "Costs cutoff (+", SD, "SD):", round( costs_high_cutoff ), "\n" )
cat( "Costs cutoff (-", SD, "SD):", round( costs_low_cutoff  ), "\n" )
cat( "Yield cutoff (+", SD, "SD):", round( yield_high_cutoff ), "\n" )
cat( "Yield cutoff (-", SD, "SD):", round( yield_low_cutoff  ), "\n" )

# rapportér antal unikke CVR per retning
cat( "\nAntal unikke CVR over costs cutoff:",
     outlier_comparison %>%
       filter( costs_diff > costs_high_cutoff ) %>%
       nrow(), "\n" )
cat( "Antal unikke CVR under costs cutoff:",
     outlier_comparison %>%
       filter( costs_diff < costs_low_cutoff ) %>%
       nrow(), "\n" )
cat( "Antal unikke CVR over yield cutoff:",
     outlier_comparison %>%
       filter( yield_diff > yield_high_cutoff ) %>%
       nrow(), "\n" )
cat( "Antal unikke CVR under yield cutoff:",
     outlier_comparison %>%
       filter( yield_diff < yield_low_cutoff ) %>%
       nrow(), "\n" )

# rapportér overlap: antal CVR flagget i mere end én retning
cat( "\nAntal CVR flagget i mere end én retning:\n" )
outlier_comparison %>%
  mutate(
    n_flags = ( costs_diff > costs_high_cutoff ) +
              ( costs_diff < costs_low_cutoff  ) +
              ( yield_diff > yield_high_cutoff ) +
              ( yield_diff < yield_low_cutoff  )
  ) %>%
  count( n_flags ) %>%
  print()

# identificer unikke outlier CVR på tværs af alle fire retninger
extreme_outliers_hvede <- outlier_comparison %>%
  filter(
    costs_diff > costs_high_cutoff |
    costs_diff < costs_low_cutoff  |
    yield_diff > yield_high_cutoff |
    yield_diff < yield_low_cutoff
  )

cat( "\nAntal unikke CVR fjernet (±", SD, "SD):",
     nrow( extreme_outliers_hvede ), "\n" )
print( extreme_outliers_hvede, n = Inf )

# undersøg overlap mellem ekstreme costs og ekstreme yield observationer
extreme_outliers_hvede <- extreme_outliers_hvede %>%
  mutate(
    flag_costs_high = costs_diff > costs_high_cutoff,
    flag_costs_low  = costs_diff < costs_low_cutoff,
    flag_yield_high = yield_diff > yield_high_cutoff,
    flag_yield_low  = yield_diff < yield_low_cutoff,
    n_flags         = flag_costs_high + flag_costs_low +
                      flag_yield_high + flag_yield_low,
    flag_type       = case_when(
      flag_costs_high & flag_yield_high ~ "High costs & High revenues",
      flag_costs_high & flag_yield_low  ~ "High costs & Low revenues",
      flag_costs_low  & flag_yield_high ~ "Low costs & High revenues",
      flag_costs_low  & flag_yield_low  ~ "Low costs & Low revenues",
      flag_costs_high                   ~ "High costs only",
      flag_costs_low                    ~ "Low costs only",
      flag_yield_high                   ~ "High revenues only",
      flag_yield_low                    ~ "Low revenues only"
    )
  )

# opsummér antal per flag type
cat( "\nAntal farms per outlier-type:\n" )
extreme_outliers_hvede %>%
  count( flag_type ) %>%
  arrange( desc( n ) ) %>%
  print()

# vis farms med høje costs: har de også høje yields?
cat( "\nFarms med høje costs --- har de også høje yields?\n" )
extreme_outliers_hvede %>%
  filter( flag_costs_high ) %>%
  select( cvr, catchment, n_fields,
          mean_yield_alloc, catchment_mean_yield, yield_diff,
          mean_costs_alloc, catchment_mean_costs, costs_diff,
          flag_type ) %>%
  arrange( desc( costs_diff ) ) %>%
  print( n = Inf )

# korrelation mellem costs_diff og yield_diff for alle farms
cat( "\nKorrelation mellem costs_diff og yield_diff (alle farms):",
     round( cor( outlier_comparison$costs_diff,
                 outlier_comparison$yield_diff,
                 use = "complete.obs" ), 3 ), "\n" )

# scatter plot: costs_diff vs yield_diff
ggplot( outlier_comparison,
        aes( x = costs_diff, y = yield_diff ) ) +
  geom_point( color = "steelblue", alpha = 0.6 ) +
  geom_point(
    data = extreme_outliers_hvede,
    aes( x = costs_diff, y = yield_diff, color = flag_type ),
    size = 2.5
  ) +
  geom_vline( xintercept = c( costs_low_cutoff, costs_high_cutoff ),
              linetype = "dashed", color = "black",   linewidth = 0.6 ) +
  geom_hline( yintercept = c( yield_low_cutoff, yield_high_cutoff ),
              linetype = "dashed", color = "black", linewidth = 0.6 ) +
  labs(
    x     = "Costs deviation from catchment mean (DKK/ha/y)",
    y     = "Revenue deviation from catchment mean (DKK/ha/y)",
    color = "Outlier type"
  ) +
  theme_bw() +
  theme( legend.position = "bottom" )

# COMMAND ----------

#### VAARBYG ####
crop <- "VaarByg"
cfg  <- cropConfig[[ crop ]]
datCrop <- subset(get(cfg$datFields), alloc_yield > 0 & !is.na(alloc_yield))
log_omk   <- log(with(datCrop, alloc_OMK_1_ha + alloc_OMK_2_ha + alloc_OMK_3_ha + alloc_OMK_4_ha + alloc_OMK_5_ha + alloc_OMK_6_ha + alloc_OMK_7_ha + alloc_OMK_8_ha + alloc_OMK_9_ha + alloc_OMK_11_ha ))
log_yield <- log(datCrop$alloc_yield_ha )
ratio     <- log_yield - log_omk
obs <- !is.na(ratio) & !is.na(log_omk)
log_omk <- log_omk[obs]; log_yield <- log_yield[obs]; ratio <- ratio[obs]; datCrop <- datCrop[obs,]
lsfenceLower <- quantile(ratio, 0.25) - 1.5 * IQR(ratio)
lsfenceUpper <- quantile(ratio, 0.75) + 1.5 * IQR(ratio)
obsOut  <- which(ratio < lsfenceLower | ratio > lsfenceUpper)
cvrOut  <- sort(unique(datCrop$cvr[obsOut]))
cvrOutList[[crop]] <- cvrOut

# rapportér outliers
cat("Outliers (marker):", length(obsOut), ". Outliers (bedrifter):", length(cvrOut), ". Unikke CVR:\n")
print(cvrOut)

# vis kystvandoplande for outlier marker
if (length(obsOut) > 0) {
  cat("\nKystvandoplande for outlier marker:\n")
  print(table(datCrop$KystvandNa[obsOut]))
  cat("\nDetaljer for outlier marker:\n")
  print(unique(datCrop[obsOut, c("cvr", "KystvandNa", "jb3_kat", "imk_areal")]))
}

# farm-niveau
omkNames       <- c( paste0("OMK_", 1:9, "_", cfg$pg),paste0("OMK_", 11, "_", cfg$pg) )
log_omk_farm   <- log(rowSums(datFarms[, omkNames], na.rm = TRUE))
log_yield_farm <- log(datFarms[[cfg$uVar]])
obsOutFarms    <- datFarms$cvr %in% cvrOut

# plot
compPlot(log_omk, log_yield, xlab = "log( costs / ha )", ylab = "log( yields / ha )", main = "")
if (length(obsOut) > 0) points(log_omk[obsOut], log_yield[obsOut], col = "red", pch = 1)

# COMMAND ----------

#### VAARBYGE: OUTLIER IDENTIFICATION ####

# set standard deviation threshold
SD <- 2

# beregn areal-vægtet catchment-gennemsnit
catchment_avg <- datFieldsVaarByg %>%
  group_by( KystvandNa ) %>%
  summarise(
    catchment_mean_yield = weighted.mean( alloc_yield_ha, imk_areal, na.rm = TRUE ),
    catchment_mean_costs = weighted.mean( alloc_costs_ha, imk_areal, na.rm = TRUE ),
    catchment_mean_dbii  = weighted.mean( DBII_ha,        imk_areal, na.rm = TRUE ),
    .groups = "drop"
  )

# beregn farm-niveau gennemsnit og sammenlign med catchment-gennemsnit
outlier_comparison <- datFieldsVaarByg %>%
  group_by( cvr ) %>%
  summarise(
    catchment        = first( KystvandNa ),
    n_fields         = n(),
    mean_yield_obs   = weighted.mean( yield_ha,       imk_areal, na.rm = TRUE ),
    mean_yield_alloc = weighted.mean( alloc_yield_ha, imk_areal, na.rm = TRUE ),
    mean_costs_alloc = weighted.mean( alloc_costs_ha, imk_areal, na.rm = TRUE ),
    mean_dbii        = weighted.mean( DBII_ha,        imk_areal, na.rm = TRUE ),
    .groups = "drop"
  ) %>%
  left_join( catchment_avg, by = c( "catchment" = "KystvandNa" ) ) %>%
  mutate(
    yield_diff = mean_yield_alloc - catchment_mean_yield,
    costs_diff = mean_costs_alloc - catchment_mean_costs,
    dbii_diff  = mean_dbii        - catchment_mean_dbii
  ) %>%
  arrange( desc( costs_diff ) )

# beregn cutoffs
costs_high_cutoff <- mean( outlier_comparison$costs_diff, na.rm = TRUE ) +
                     SD  * sd( outlier_comparison$costs_diff, na.rm = TRUE )
costs_low_cutoff  <- mean( outlier_comparison$costs_diff, na.rm = TRUE ) -
                     SD  * sd( outlier_comparison$costs_diff, na.rm = TRUE )
yield_high_cutoff <- mean( outlier_comparison$yield_diff, na.rm = TRUE ) +
                     SD  * sd( outlier_comparison$yield_diff, na.rm = TRUE )
yield_low_cutoff  <- mean( outlier_comparison$yield_diff, na.rm = TRUE ) -
                     SD  * sd( outlier_comparison$yield_diff, na.rm = TRUE )

cat( "Costs cutoff (+", SD, "SD):", round( costs_high_cutoff ), "\n" )
cat( "Costs cutoff (-", SD, "SD):", round( costs_low_cutoff  ), "\n" )
cat( "Yield cutoff (+", SD, "SD):", round( yield_high_cutoff ), "\n" )
cat( "Yield cutoff (-", SD, "SD):", round( yield_low_cutoff  ), "\n" )

# rapportér antal unikke CVR per retning
cat( "\nAntal unikke CVR over costs cutoff:",
     outlier_comparison %>%
       filter( costs_diff > costs_high_cutoff ) %>%
       nrow(), "\n" )
cat( "Antal unikke CVR under costs cutoff:",
     outlier_comparison %>%
       filter( costs_diff < costs_low_cutoff ) %>%
       nrow(), "\n" )
cat( "Antal unikke CVR over yield cutoff:",
     outlier_comparison %>%
       filter( yield_diff > yield_high_cutoff ) %>%
       nrow(), "\n" )
cat( "Antal unikke CVR under yield cutoff:",
     outlier_comparison %>%
       filter( yield_diff < yield_low_cutoff ) %>%
       nrow(), "\n" )

# rapportér overlap: antal CVR flagget i mere end én retning
cat( "\nAntal CVR flagget i mere end én retning:\n" )
outlier_comparison %>%
  mutate(
    n_flags = ( costs_diff > costs_high_cutoff ) +
              ( costs_diff < costs_low_cutoff  ) +
              ( yield_diff > yield_high_cutoff ) +
              ( yield_diff < yield_low_cutoff  )
  ) %>%
  count( n_flags ) %>%
  print()

# identificer unikke outlier CVR på tværs af alle fire retninger
extreme_outliers_byg <- outlier_comparison %>%
  filter(
    costs_diff > costs_high_cutoff |
    costs_diff < costs_low_cutoff  |
    yield_diff > yield_high_cutoff |
    yield_diff < yield_low_cutoff
  )

cat( "\nAntal unikke CVR fjernet (±", SD, "SD):",
     nrow( extreme_outliers_byg ), "\n" )
print( extreme_outliers_byg, n = Inf )

# undersøg overlap mellem ekstreme costs og ekstreme yield observationer
extreme_outliers_byg <- extreme_outliers_byg %>%
  mutate(
    flag_costs_high = costs_diff > costs_high_cutoff,
    flag_costs_low  = costs_diff < costs_low_cutoff,
    flag_yield_high = yield_diff > yield_high_cutoff,
    flag_yield_low  = yield_diff < yield_low_cutoff,
    n_flags         = flag_costs_high + flag_costs_low +
                      flag_yield_high + flag_yield_low,
    flag_type       = case_when(
      flag_costs_high & flag_yield_high ~ "High costs & High revenues",
      flag_costs_high & flag_yield_low  ~ "High costs & Low revenues",
      flag_costs_low  & flag_yield_high ~ "Low costs & High revenues",
      flag_costs_low  & flag_yield_low  ~ "Low costs & Low revenues",
      flag_costs_high                   ~ "High costs only",
      flag_costs_low                    ~ "Low costs only",
      flag_yield_high                   ~ "High revenues only",
      flag_yield_low                    ~ "Low revenues only"
    )
  )

# opsummér antal per flag type
cat( "\nAntal farms per outlier-type:\n" )
extreme_outliers_byg %>%
  count( flag_type ) %>%
  arrange( desc( n ) ) %>%
  print()

# vis farms med høje costs: har de også høje yields?
cat( "\nFarms med høje costs --- har de også høje yields?\n" )
extreme_outliers_byg %>%
  filter( flag_costs_high ) %>%
  select( cvr, catchment, n_fields,
          mean_yield_alloc, catchment_mean_yield, yield_diff,
          mean_costs_alloc, catchment_mean_costs, costs_diff,
          flag_type ) %>%
  arrange( desc( costs_diff ) ) %>%
  print( n = Inf )

# korrelation mellem costs_diff og yield_diff for alle farms
cat( "\nKorrelation mellem costs_diff og yield_diff (alle farms):",
     round( cor( outlier_comparison$costs_diff,
                 outlier_comparison$yield_diff,
                 use = "complete.obs" ), 3 ), "\n" )

# scatter plot: costs_diff vs yield_diff
ggplot( outlier_comparison,
        aes( x = costs_diff, y = yield_diff ) ) +
  geom_point( color = "steelblue", alpha = 0.6 ) +
  geom_point(
    data = extreme_outliers_byg,
    aes( x = costs_diff, y = yield_diff, color = flag_type ),
    size = 2.5
  ) +
  geom_vline( xintercept = c( costs_low_cutoff, costs_high_cutoff ),
              linetype = "dashed", color = "black",   linewidth = 0.6 ) +
  geom_hline( yintercept = c( yield_low_cutoff, yield_high_cutoff ),
              linetype = "dashed", color = "black", linewidth = 0.6 ) +
  labs(
    x     = "Costs deviation from catchment mean (DKK/ha/y)",
    y     = "Revenues deviation from catchment mean (DKK/ha/y)",
    color = "Outlier type"
  ) +
  theme_bw() +
  theme( legend.position = "bottom" )

# COMMAND ----------

#### FJERN OUTLIERS ####
cvrOutAll <-  c( extreme_outliers_hvede$cvr )
cat( "\nFjerner", length( cvrOutAll ), "unikke cvr i alt\n" )
datFields <- subset( datFields, !cvr %in% cvrOutAll )
datFarms  <- subset( datFarms,  !cvr %in% cvrOutAll )
cat( "Marker tilbage i datFields:", nrow( datFields ), "\n" )
cat( "Bedrifter tilbage i datFarms:", nrow( datFarms ), "\n" )

# COMMAND ----------

#### VINTERBYG ####
crop <- "VinterByg"
cfg  <- cropConfig[[ crop ]]
datCrop <- subset(get(cfg$datFields), alloc_yield > 0 & !is.na(alloc_yield))
log_omk   <- log(with(datCrop, alloc_OMK_1 + alloc_OMK_2 + alloc_OMK_3 + alloc_OMK_4 + alloc_OMK_5 + alloc_OMK_6 + alloc_OMK_7 + alloc_OMK_8 + alloc_OMK_9))
log_yield <- log(datCrop$alloc_yield)
ratio     <- log_yield - log_omk
obs <- !is.na(ratio) & !is.na(log_omk)
log_omk <- log_omk[obs]; log_yield <- log_yield[obs]; ratio <- ratio[obs]; datCrop <- datCrop[obs,]
lsfenceLower <- quantile(ratio, 0.25) - 1.5 * IQR(ratio)
lsfenceUpper <- quantile(ratio, 0.75) + 1.5 * IQR(ratio)
obsOut  <- which(ratio < lsfenceLower | ratio > lsfenceUpper)
cvrOut  <- sort(unique(datCrop$cvr[obsOut]))
cvrOutList[[crop]] <- cvrOut
cat("Outliers (marker):", length(obsOut), ". Outliers (bedrifter):", length(cvrOut), ". Unikke CVR: \n"); print(cvrOut)
omkNames       <- paste0("OMK_", 1:9, "_", cfg$pg)
log_omk_farm   <- log(rowSums(datFarms[, omkNames], na.rm = TRUE))
log_yield_farm <- log(datFarms[[cfg$uVar]])
obsOutFarms    <- datFarms$cvr %in% cvrOut
par(mfrow = c(1, 2))
compPlot(log_omk, log_yield, xlab = "log( costs )", ylab = "log( yields )", main = "Field-level - Vinterbyg")
if (length(obsOut) > 0) points(log_omk[obsOut], log_yield[obsOut], col = "red", pch = 1)
compPlot(log_omk_farm, log_yield_farm, xlab = "log( costs )", ylab = "log( yields )", main = "Farm-level - Vinterbyg")
if (any(obsOutFarms)) {
  points(log_omk_farm[obsOutFarms], log_yield_farm[obsOutFarms], col = "red", pch = 1)
  text(log_omk_farm[obsOutFarms], log_yield_farm[obsOutFarms], labels = datFarms$cvr[obsOutFarms], pos = 3, cex = 0.5, col = "red")
}

# COMMAND ----------

# MAGIC %md
# MAGIC ## Laver vinterhvede reference kort

# COMMAND ----------

#### VINTERHVEDE ####

# Trin 1: pivoterer vinterhvede-referencen til langt format
ref_vinterhvede <- datVinterhvede %>%
  select(Kystvandopland, DBII_ha_finsand, DBII_ha_grovsand, DBII_ha_lerjord) %>%
  pivot_longer(
    cols = c(DBII_ha_finsand, DBII_ha_grovsand, DBII_ha_lerjord),
    names_to = "jb3_kat",
    values_to = "DBII_ref_Vinterhvede"
  ) %>%
  mutate(jb3_kat = case_when(
    jb3_kat == "DBII_ha_finsand"  ~ "sand_fin",
    jb3_kat == "DBII_ha_grovsand" ~ "sand_grov",
    jb3_kat == "DBII_ha_lerjord"  ~ "ler"
  ))

# Trin 2: join på datFields via kystvandopland + jordtype
dat_ref_Vinterhvede <- datFields %>%
  left_join(ref_vinterhvede, by = c("KystvandNa" = "Kystvandopland", "jb3_kat"))

### gemmer reference kort 
obs <- "conv"
saveRDS( dat_ref_Vinterhvede, file.path( dirPath, paste0( "results/datResults_", obs, "_refMap_vinter.rds" ) ) )

# COMMAND ----------

display( dat_ref_Vinterhvede )

# COMMAND ----------

#### Vaarbyg ####

# Trin 1: pivoterer vinterhvede-referencen til langt format
ref_vaarbyg <- datVaarByg %>%
  select(Kystvandopland, DBII_ha_finsand, DBII_ha_grovsand, DBII_ha_lerjord) %>%
  pivot_longer(
    cols = c(DBII_ha_finsand, DBII_ha_grovsand, DBII_ha_lerjord),
    names_to = "jb3_kat",
    values_to = "DBII_ref_Vaarbyg"
  ) %>%
  mutate(jb3_kat = case_when(
    jb3_kat == "DBII_ha_finsand"  ~ "sand_fin",
    jb3_kat == "DBII_ha_grovsand" ~ "sand_grov",
    jb3_kat == "DBII_ha_lerjord"  ~ "ler"
  ))

# Trin 2: join på datFields via kystvandopland + jordtype
dat_ref_Vaarbyg <- datFields %>%
  left_join(ref_vaarbyg, by = c("KystvandNa" = "Kystvandopland", "jb3_kat"))

  ### gemmer reference kort 
obs <- "conv"
saveRDS( dat_ref_Vaarbyg, file.path( dirPath, paste0( "results/datResults_", obs, "_refMap_vaarbyg.rds" ) ) )

# COMMAND ----------

display( dat_ref_Vaarbyg )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Tester uvægtet DB på tværs af jordtyper og kystvandoplande

# COMMAND ----------

# funktion til at teste signifikans af DBII på tværs af jordtyper og kystvandoplande
testDBII <- function( dat, crop_name ) {
  
  # lav subset af data
  dat <- subset( dat, KystvandID %in% codeCatchment & !is.na( DBII_ha ) )

  cat( "\n========================================\n" )
  cat( "Signifikanstest for:", crop_name, "\n" )
  cat( "Antal marker:", nrow( dat ), "\n" )
  cat( "========================================\n" )

  #### Test på tværs af jordtyper ####
  cat( "\n--- Jordtyper ---\n" )

  # antal marker og gennemsnit per jordtype
  cat("Antal marker per jordtype:\n")
  print( table( dat$jb3_kat ) )
  cat("\nGennemsnitlig DBII_ha per jordtype (DKK/ha):\n")
  print( round( tapply( dat$DBII_ha, dat$jb3_kat, mean, na.rm = TRUE ), 0 ) )

  # ANOVA test
  aov_jb3 <- aov( DBII_ha ~ jb3_kat, data = dat )
  cat("\nANOVA test (jordtype):\n")
  print( summary( aov_jb3 ) )

  # pairwise t-test hvis signifikant
  if ( summary( aov_jb3 )[[1]][[ "Pr(>F)" ]][1] < 0.05 ) {
    cat("\nPairwise t-test (Bonferroni):\n")
    print( pairwise.t.test( dat$DBII_ha, dat$jb3_kat, p.adjust.method = "bonferroni" ) )
  }

  #### Test på tværs af kystvandoplande ####
  cat( "\n--- Kystvandoplande ---\n" )

  # antal marker og gennemsnit per kystvandopland
  cat( "Antal marker per kystvandopland:\n" )
  print( table( dat$KystvandNa ) )
  cat( "\nGennemsnitlig DBII_ha per kystvandopland (DKK/ha):\n" )
  print( round( tapply( dat$DBII_ha, dat$KystvandNa, mean, na.rm = TRUE ), 0 ) )

  # ANOVA test
  aov_kyst <- aov( DBII_ha ~ KystvandNa, data = dat )
  cat( "\nANOVA test (kystvandopland):\n" )
  print( summary( aov_kyst ) )

  # pairwise t-test hvis signifikant
  if ( summary( aov_kyst )[[1]][[ "Pr(>F)" ]][1] < 0.05 ) {
    cat( "\nPairwise t-test (Bonferroni):\n" )
    print( pairwise.t.test( dat$DBII_ha, dat$KystvandNa, p.adjust.method = "bonferroni" ) )
  }

}

# kør test for hver afgrøde
testDBII( datFieldsVaarbyg, "Byg" )
testDBII( datFieldsVinterhvede, "Hvede" )
testDBII( datFieldsKartofler, "Kartofler" )
testDBII( datFieldsBland, "Havre_blandet_korn" )
testDBII( datFieldsRaps, "Raps" )
testDBII( datFieldsFroe, "Frø" )
testDBII( datFieldsMajs, "Fodermajs" )
testDBII( datFieldsAerter, "Ærter_mv" )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Validering

# COMMAND ----------

display( datFields )

# COMMAND ----------

datFieldsVaarByg <- subset( datFields, afgkode == 1 )
datFieldsVinterhvede <- subset( datFields, crop_kat == "Hvede" )

for (dat_name in c("datFieldsVinterhvede", "datFieldsVaarByg")) {
  dat <- get(dat_name)

  # DST Costs I: Seed + Fertiliser + Plant protection + Misc +
  #              Energy (excl. fuel) + Fuel + Contractor services
  dat$alloc_costs_I_ha_dst  <- dat$alloc_OMK_1_ha + dat$alloc_OMK_2_ha +
                                dat$alloc_OMK_3_ha + dat$alloc_OMK_4_ha +
                                dat$alloc_OMK_6_ha + dat$alloc_OMK_7_ha +
                                dat$alloc_OMK_5_ha

  # DST Costs II: Labor + Maintenance (fields) + Depreciation (fields)
  dat$alloc_costs_II_ha_dst <- dat$alloc_OMK_8_ha +
                                dat$alloc_OMK_9_ha +
                                dat$alloc_OMK_11_ha

  assign(dat_name, dat)
}

# Define variables and labels (DST ordering)
vars <- c("alloc_yield_ha",
          "alloc_costs_I_ha_dst",
          "alloc_OMK_1_ha", "alloc_OMK_2_ha", "alloc_OMK_3_ha", "alloc_OMK_4_ha",
          "alloc_OMK_6_ha", "alloc_OMK_7_ha", "alloc_OMK_5_ha",
          "alloc_costs_II_ha_dst",
          "alloc_OMK_8_ha", "alloc_OMK_9_ha", "alloc_OMK_11_ha",
          "DBII_ha")

labels <- c("Gross yield",
            "Costs I",
            "  Seed", "  Fertiliser", "  Plant protection", "  Misc. field costs",
            "  Energy excl. fuel", "  Fuel", "  Contractor services",
            "Costs II",
            "  Labor", "  Maintenance, fields", "  Depreciation, fields",
            "DBII")

# Function to compute area-weighted means
wtd_means <- function(dat, vars) {
  sapply(vars, function(v) weighted.mean(dat[[v]], w = dat$imk_areal, na.rm = TRUE))
}

# Calculate weighted means
resultsVinterHvede <- wtd_means(datFieldsVinterhvede, vars)
resultsVaarByg     <- wtd_means(datFieldsVaarByg,     vars)

# Farm counts
n_farms_hvede <- length(unique(datFieldsVinterhvede$cvr))
n_farms_byg   <- length(unique(datFieldsVaarByg$cvr))

# Combine into table
results <- rbind(
  data.frame(Variable    = "N fields (farms)",
             VinterHvede = paste0(nrow(datFieldsVinterhvede), " (", n_farms_hvede, ")"),
             Vaarbyg     = paste0(nrow(datFieldsVaarByg),     " (", n_farms_byg,   ")")),
  data.frame(Variable    = labels,
             VinterHvede = as.character(round(resultsVinterHvede)),
             Vaarbyg     = as.character(round(resultsVaarByg)))
)
row.names(results) <- NULL
print(results)

# Catchment codes and names for labelling
catchment_names <- c(
  "111" = "Lister Dyb",
  "232" = "Nissum Bredning",
  "131" = "Nissum Fjord, Felsted Kog",
  "236" = "Thisted Bredning",
  "235" = "Nibe Bredning og Langerak",
  "136" = "Randers Fjord, indre",
  "128" = "Horsens Fjord, indre",
  "93"  = "Odense Fjord, Seden Strand",
  "165" = "Isefjord, indre",
  "35"  = "Karrebæk Fjord",
  "2"   = "Roskilde Fjord, indre"
)

codeCatchment <- c(111, 232, 131, 236, 235, 136, 128, 93, 165, 35, 2)

# Add DST-aligned aggregated cost variables
for (dat_name in c("datFieldsVinterhvede", "datFieldsVaarByg")) {
  dat <- get(dat_name)

  # DST Costs I: Seed + Fertiliser + Plant protection + Misc +
  #              Energy excl. fuel + Fuel + Contractor services
  dat$alloc_costs_I_ha_dst  <- dat$alloc_OMK_1_ha + dat$alloc_OMK_2_ha +
                                dat$alloc_OMK_3_ha + dat$alloc_OMK_4_ha +
                                dat$alloc_OMK_6_ha + dat$alloc_OMK_7_ha +
                                dat$alloc_OMK_5_ha

  # DST Costs II: Labor + Maintenance (fields) + Depreciation (fields)
  dat$alloc_costs_II_ha_dst <- dat$alloc_OMK_8_ha +
                                dat$alloc_OMK_9_ha +
                                dat$alloc_OMK_11_ha

  assign(dat_name, dat)
}

# Variables and labels (DST ordering)
vars <- c("alloc_yield_ha",
          "alloc_costs_I_ha_dst",
          "alloc_OMK_1_ha", "alloc_OMK_2_ha", "alloc_OMK_3_ha", "alloc_OMK_4_ha",
          "alloc_OMK_6_ha", "alloc_OMK_7_ha", "alloc_OMK_5_ha",
          "alloc_costs_II_ha_dst",
          "alloc_OMK_8_ha", "alloc_OMK_9_ha", "alloc_OMK_11_ha",
          "DBII_ha")

labels <- c("Gross yield",
            "Costs I",
            "  Seed", "  Fertiliser", "  Plant protection", "  Misc. field costs",
            "  Energy excl. fuel", "  Fuel", "  Contractor services",
            "Costs II",
            "  Labor", "  Maintenance, fields", "  Depreciation, fields",
            "DBII")

# Function to compute area-weighted means
wtd_means <- function(dat, vars) {
  sapply(vars, function(v) weighted.mean(dat[[v]], w = dat$imk_areal, na.rm = TRUE))
}

# Loop over catchments
resultsByCatchment <- list()

for (code in codeCatchment) {

  cat_hvede <- subset(datFieldsVinterhvede, KystvandID == code)
  cat_byg   <- subset(datFieldsVaarByg,    KystvandID == code)

  if (nrow(cat_hvede) < 5 & nrow(cat_byg) < 5) {
    cat("Skipping catchment", code, "- too few observations\n")
    next
  }

  calc <- function(dat) {
    if (nrow(dat) == 0) return(rep(NA, length(vars)))
    wtd_means(dat, vars)
  }

  res_hvede <- calc(cat_hvede)
  res_byg   <- calc(cat_byg)

  n_fields_hvede <- nrow(cat_hvede)
  n_fields_byg   <- nrow(cat_byg)
  n_farms_hvede  <- length(unique(cat_hvede$cvr))
  n_farms_byg    <- length(unique(cat_byg$cvr))

  results <- rbind(
    data.frame(
      Variable    = "N fields (farms)",
      VinterHvede = paste0(n_fields_hvede, " (", n_farms_hvede, ")"),
      Vaarbyg     = paste0(n_fields_byg,   " (", n_farms_byg,   ")")
    ),
    data.frame(
      Variable    = labels,
      VinterHvede = as.character(round(res_hvede)),
      Vaarbyg     = as.character(round(res_byg))
    )
  )
  row.names(results) <- NULL

  resultsByCatchment[[as.character(code)]] <- results

  cat("\nCatchment:", catchment_names[as.character(code)], "\n")
  print(results)
}