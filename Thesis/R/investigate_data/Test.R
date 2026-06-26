# Databricks notebook source
# installer R pakker
install.packages( "gtools" )
install.packages( "lmtest" )
install.packages( "miscTools" )
install.packages( "car" )
install.packages( "sandwich" )

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

# indlæs R pakker - spatial analysis
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

# indlæs estimeret output-specifikke input mængder
datFarmsInp <- readRDS( file.path( dirPath, paste0( "results/datResults_", obs, ".rds" ) ) )
datFarmsYield <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )

# indlæs klarggjort markniveau datasæt
datFields <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_fields.rds" ) )
datFields <- st_drop_geometry( datFields )

# behold kun observationer i datasættene der er brugt i estimeringen
datFields <- datFields %>% semi_join( datFarmsInp, by = "cvr" ) 
datFarmsYield <- datFarmsYield %>% semi_join( datFarmsInp, by = "cvr" ) 


#### Forbered kystvandoplande ####

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
  summarise(
    total_area = sum( imk_areal, na.rm = TRUE ),
    .groups = "drop"
  ) %>%
  arrange( cvr, desc( total_area ) ) %>%
  group_by( cvr ) %>%
  slice( 1 ) %>%
  ungroup() %>%
  rename( main_catchment = KystvandID )


#### Forbered datasæt til estimering af udbytter ####

# definér mapping fra afgrødekategori til U_ variable
uToCrop <- tibble(
  uVar = paste0( "U_", c(1:4, 6:9, 11)),
  crop_kat = c(
    "Byg", "Hvede", "Havre_blandet_korn", "Rug_Triticale",
    "Frø", "Kartofler", "Raps", "Ærter_mv", "Fodermajs"
  )
)

# beregn areal, jordtype-andele og udbytte per hektar i ét datasæt
datYield <- datFields %>%
  group_by( cvr, crop_kat ) %>%
  summarise(
    total_area      = sum( imk_areal, na.rm = TRUE ),
    share_sand_fin  = sum( imk_areal[ jb3_kat == "sand_fin" ],  na.rm = TRUE ) / total_area,
    share_sand_grov = sum( imk_areal[ jb3_kat == "sand_grov" ], na.rm = TRUE ) / total_area,
    .groups = "drop"
  ) %>%
  left_join( datFarmsYield %>%
      select( cvr, all_of( uToCrop$uVar ) ) %>%
      pivot_longer( all_of( uToCrop$uVar ), 
      names_to = "uVar", 
      values_to = "uValue" ) %>%
      left_join( uToCrop, by = "uVar" ) %>%
      filter( uValue > 0 ),
    by = c( "cvr", "crop_kat" )
  ) %>%
  mutate( yield_ha = uValue / total_area ) %>%
  select( cvr, crop_kat, share_sand_fin, share_sand_grov, yield_ha )

datYield <- datYield %>% 
  left_join( datCatchment, by = "cvr") %>% 
  select( cvr, crop_kat, share_sand_fin, share_sand_grov, yield_ha, main_catchment )

# beregn dummy variable for udvalgte afgrøde kategorier
datDummy <- datFields %>%
  group_by( cvr, crop_kat ) %>%
  summarise(
    ha_tot              = sum( imk_areal, na.rm = TRUE ),
    share_hesteboenner  = sum( imk_areal[ afgkode == 31 ], na.rm = TRUE) / ha_tot,
    share_vinterbyg     = sum( imk_areal[ afgkode == 1 ], na.rm = TRUE) / ha_tot,
    share_laegge_tidlige = sum( imk_areal[ afgkode %in% c( 149, 150, 157 ) ], na.rm = TRUE) / ha_tot,
    share_spise         = sum( imk_areal[ afgkode == 152 ], na.rm = TRUE) / ha_tot,
    share_havefroe      = sum( imk_areal[ afgkode %in% c( 113, 124, 666, 668 ) ], na.rm = TRUE ) / ha_tot,
    share_vaarhvede     = sum( imk_areal[ afgkode == 2 ], na.rm = TRUE ) / ha_tot,
    .groups = "drop"
  ) %>%
  select( -ha_tot )

# tilføj dummy variable til udbytte datasættet
datYield <- datYield %>%
  left_join( datDummy, by = c( "cvr", "crop_kat" ) ) %>%
  mutate( across( starts_with( "share_" ), ~ replace_na( .x, 0 ) ) )

# klarggør datasæt til estimering
datYield <- subset( datYield, !is.na( yield_ha ) & yield_ha > 0 & !is.na( main_catchment ) )

#### OLS estimering for hver afgrøde kategori ####
mYield <- lm( log( yield_ha ) ~ (share_sand_fin + share_sand_grov) * crop_kat + share_hesteboenner
+ share_vinterbyg + share_laegge_tidlige + share_spise + share_havefroe + share_vaarhvede + factor( main_catchment ), data = datYield )
summary( mYield )

all_results_yield <- list()
crop_kats_base <- "Ærter_mv"  # base kategori
crop_kats_rest <- setdiff( unique( datYield$crop_kat ), crop_kats_base )

results <- lapply( crop_kats_rest, function(crop) {
  me_fin  <- coef(mYield)["share_sand_fin"]  + coef(mYield)[paste0("share_sand_fin:crop_kat",  crop)]
  me_grov <- coef(mYield)["share_sand_grov"] + coef(mYield)[paste0("share_sand_grov:crop_kat", crop)]
  me_ler  <- (-1) * (me_fin + me_grov)
  
  hypotheses <- c(
    paste0("share_sand_fin + share_sand_fin:crop_kat",   crop, " = 0"),
    paste0("share_sand_grov + share_sand_grov:crop_kat", crop, " = 0")
  )
  
  test <- tryCatch(
    linearHypothesis(mYield, hypotheses, vcov = vcovCL(mYield, cluster = ~cvr, data = datYield)),
    error = function(e) NULL
  )
  
  data.frame(
    crop_kat = crop,
    me_fin   = round(me_fin,  4),
    me_grov  = round(me_grov, 4),
    me_ler   = round(me_ler,  4),
    F_stat   = ifelse(is.null(test), NA, round(test$F[2], 4)),
    p_value  = ifelse(is.null(test), NA, round(test$`Pr(>F)`[2], 5))
  )
}
)

# tilføj base kategori (Ærter_mv)
results_base <- data.frame(
  crop_kat = crop_kats_base,
  me_fin   = round(coef(mYield)["share_sand_fin"],  4),
  me_grov  = round(coef(mYield)["share_sand_grov"], 4),
  me_ler   = round(-(coef(mYield)["share_sand_fin"] + coef(mYield)["share_sand_grov"]), 4),
  F_stat   = NA,
  p_value  = NA
)

result_table_yield <- rbind(results_base, do.call(rbind, results))
rownames(result_table_yield) <- NULL
print(result_table_yield)



#### Forbered datasæt til estimering af omkostninger/input ####

# subset inputs (omkostninger og kvælstof) for hver produktionsgren
omkNames <- grep( "^coef_OMK_", names( datFarmsInp ), value = TRUE )
inpNames <- grep( "^coef_Q_", names( datFarmsInp ), value = TRUE )
datFarmsInp <- datFarmsInp[ , c( "cvr", omkNames, inpNames ) ]

# lav datasæt til 'long' format
datFarmsInpLong <- datFarmsInp %>%
  pivot_longer(
    cols          = c( starts_with( "coef_OMK_" ), starts_with( "coef_Q_" ) ),
    names_to      = c( "inp_kat", "pg_kat" ),
    names_pattern = "coef_(OMK_\\d+|Q_\\d+)_(PG_.*)",
    values_to     = "inp_ha"
  ) %>% 
  select( cvr, pg_kat, inp_kat, inp_ha )

# beregn jordandele (med ler som base) for hver produktionsgren inden for hvert cvr
datInp <- datFields %>% 
group_by( cvr, pg_kat ) %>% summarise( 
    total_area = sum( imk_areal ),
    share_sand_fin = sum( imk_areal[ jb3_kat == "sand_fin" ] ) / total_area,
    share_sand_grov = sum( imk_areal[ jb3_kat == "sand_grov" ] ) / total_area,
    .groups = "drop"
  ) %>% select( 
    cvr, pg_kat, share_sand_fin, share_sand_grov 
)

# tilføje kolonne med inputs til datasæt med afgrøde x jordandele for hvert cvr
datInp <- datInp %>% left_join( datFarmsInpLong, by = c( "cvr", "pg_kat" ) )

# tilføj kystvandoplande koder som kolone til datasættene
datInp <- datInp %>% 
left_join( datCatchment, by = "cvr" ) %>% 
select( cvr, pg_kat, share_sand_fin, share_sand_grov, inp_kat, inp_ha, main_catchment )

# klarggør datasæt til estimering
datInp <- subset( datInp, !is.na( inp_ha ) & inp_ha > 0 & !is.na( main_catchment ) )


#### OLS estimering for hver omkostning/input ####
all_results <- list()
for( inp in unique( datInp$inp_kat ) ) {
  datEst <- subset( datInp, inp_kat == inp )
  mInt <- lm( log( inp_ha ) ~ ( share_sand_fin + share_sand_grov ) * pg_kat + factor( main_catchment ), 
              data = datEst ) # brug her cvr som factor hvis muligt
  
  # beregn marginal effekter for alle PG-jordtype kombinationer bortset fra base (PG_1)
  pg_kats <- paste0( "PG_", 2:19 )
  results <- lapply( pg_kats, function( pg ) {
    me_fin  <- coef( mInt )[ "share_sand_fin" ] + coef( mInt )[ paste0( "share_sand_fin:pg_kat", pg ) ]
    me_grov <- coef( mInt )[ "share_sand_grov" ] + coef( mInt )[ paste0( "share_sand_grov:pg_kat", pg ) ]
    me_ler <- (-1) * ( me_fin + me_grov )
    hypotheses <- c( paste0( "share_sand_fin + share_sand_fin:pg_kat", pg, " = 0" ),
                     paste0( "share_sand_grov + share_sand_grov:pg_kat", pg, " = 0" ) )
    
    # Some PG categories may not exist in all subsets - wrap in tryCatch
    test <- tryCatch( linearHypothesis( mInt, hypotheses, vcov = vcovCL(mInt, cluster = ~cvr, 
                      data = datEst ) ), error = function(e) NULL )
    
    # datasæt med marginal effekter, F-test og P-værdier
    data.frame( pg_kat = pg, me_fin  = round( me_fin,  4 ), me_grov = round( me_grov, 4 ), 
                me_ler = round( me_ler, 4 ), F_stat = ifelse( is.null( test ), NA, round( test$F[2], 4 ) ), p_value = ifelse( is.null(test), NA, round( test$`Pr(>F)`[2], 5 ) ) )
    }
  )
  # tilføjer base kategori (PG_1)
  results_base <- data.frame( pg_kat  = "PG_1", me_fin  = round( coef( mInt )[ "share_sand_fin" ], 4 ),
                              me_grov = round( coef( mInt )[ "share_sand_grov" ], 4 ),
                              me_ler = round( -( coef( mInt )[ "share_sand_fin" ] + coef( mInt )[ "share_sand_grov" ] ), 4 ),
                              F_stat  = NA,
                              p_value = NA )
  # gemmer estimeret marginale effekter 
  result_table <- rbind( results_base, do.call( rbind, results ) )
  rownames( result_table ) <- NULL
  all_results[[ inp ]] <- result_table
}

# tjek alle estimeret marginale effekter
for( inp in names( all_results ) ) {
  cat( "\nInput:", inp, "\n" )
  print( all_results[[ inp ]] )
}

# COMMAND ----------

summary(mYield)

# COMMAND ----------

# tilføj omkostninger / inputs til field-level datasæt
datFarmsInpWide <- datFarmsInpLong %>%
  pivot_wider(
    names_from  = inp_kat,
    values_from = inp_ha
  )
datFields <- datFields %>% left_join( datFarmsInpWide, by = c( "cvr", "pg_kat" ) )

# lav tabel med beregnet jordtype-specfikke theta for hvert input/omostning og produktionsgren
theta_table <- bind_rows(
  lapply( names( all_results ), function( inp ) {
    all_results[[ inp ]] %>% mutate(
      inp_kat = inp,
      theta_sand_fin = exp( me_fin ),
      theta_sand_grov = exp( me_grov ),
      theta_ler = exp( me_ler )
    ) %>%
    select( inp_kat, pg_kat, theta_sand_fin, theta_sand_grov, theta_ler )
  }
 )
)

# pivoter theta_table til 'long' format
theta_long <- theta_table %>%
  pivot_longer(
    cols = c( theta_sand_fin, theta_sand_grov, theta_ler ),
    names_to  = "jb3_kat_theta",
    values_to = "theta"
  ) %>%
  mutate( jb3_kat = case_when(
    jb3_kat_theta == "theta_sand_fin"  ~ "sand_fin",
    jb3_kat_theta == "theta_sand_grov" ~ "sand_grov",
    jb3_kat_theta == "theta_ler"       ~ "ler"
  ) ) %>%
  select( inp_kat, pg_kat, jb3_kat, theta )

# tilføj række ID for pivotere tilbage senere 
datFields <- datFields %>%
  mutate( .row_id = row_number() )

# pivoter omkostninger/inputs til 'long' format, tilføj thetaer, og pivoter tilbage 'wide' format
theta_wide <- datFields %>%
  select(.row_id, pg_kat, jb3_kat, starts_with( "OMK_"), Q_1 ) %>%
  pivot_longer(
    cols      = c( starts_with( "OMK_" ), Q_1 ),
    names_to  = "inp_kat",
    values_to = "omk_value"
  ) %>%
  left_join( theta_long, by = c( "inp_kat", "pg_kat", "jb3_kat" ) ) %>%
  mutate( theta_col = paste0( "theta_", inp_kat ) ) %>%
  select( .row_id, theta_col, theta ) %>%
  pivot_wider(
    names_from  = theta_col,
    values_from = theta
  )

# tilføj de 10 nye theta kolonner tilbage i mark niveau datasættet
datFields <- datFields %>%
  left_join( theta_wide, by = ".row_id" ) %>%
  select( -.row_id )

# beregn nævneren inden for cvr × produktionsgren
denom <- datFields %>%
  group_by( cvr, pg_kat ) %>%
  summarise(
    imk_areal_tot = sum( imk_areal ),
    denom_OMK_1 = sum( imk_areal * theta_OMK_1 ),
    denom_OMK_2 = sum( imk_areal * theta_OMK_2 ),
    denom_OMK_3 = sum( imk_areal * theta_OMK_3 ),
    denom_OMK_4 = sum( imk_areal * theta_OMK_4 ),
    denom_OMK_5 = sum( imk_areal * theta_OMK_5 ),
    denom_OMK_6 = sum( imk_areal * theta_OMK_6 ),
    denom_OMK_7 = sum( imk_areal * theta_OMK_7 ),
    denom_OMK_8 = sum( imk_areal * theta_OMK_8 ),
    denom_OMK_9 = sum( imk_areal * theta_OMK_9 ),
    denom_Q_1   = sum( imk_areal * theta_Q_1 ),
    .groups = "drop"
  )

# join and alloker omkostninger/inputs
datFields <- datFields %>%
  left_join( denom, by = c("cvr", "pg_kat" ) ) %>%
  mutate(
    alloc_OMK_1 = OMK_1 * ( imk_areal * theta_OMK_1 ) / denom_OMK_1 * imk_areal_tot,
    alloc_OMK_2 = OMK_2 * ( imk_areal * theta_OMK_2 ) / denom_OMK_2 * imk_areal_tot,
    alloc_OMK_3 = OMK_3 * ( imk_areal * theta_OMK_3 ) / denom_OMK_3 * imk_areal_tot,
    alloc_OMK_4 = OMK_4 * ( imk_areal * theta_OMK_4 ) / denom_OMK_4 * imk_areal_tot,
    alloc_OMK_5 = OMK_5 * ( imk_areal * theta_OMK_5 ) / denom_OMK_5 * imk_areal_tot,
    alloc_OMK_6 = OMK_6 * ( imk_areal * theta_OMK_6 ) / denom_OMK_6 * imk_areal_tot,
    alloc_OMK_7 = OMK_7 * ( imk_areal * theta_OMK_7 ) / denom_OMK_7 * imk_areal_tot,
    alloc_OMK_8 = OMK_8 * ( imk_areal * theta_OMK_8 ) / denom_OMK_8 * imk_areal_tot,
    alloc_OMK_9 = OMK_9 * ( imk_areal * theta_OMK_9 ) / denom_OMK_9 * imk_areal_tot,
    alloc_Q_1 = Q_1 * ( imk_areal * theta_Q_1 ) / denom_Q_1   * imk_areal_tot
  ) %>%
  select( -starts_with( "denom_" ) )

# tjek om de mark niveau estimater summere til de observeret omkostinger/inputs in regnskaberne
for ( j in c( paste0( "OMK_", 1:9 ), "Q_1" ) ) {
  result <- datFields %>%
    group_by( cvr, pg_kat ) %>%
    summarise(
      check      = sum( .data[[ paste0("alloc_", j ) ] ], na.rm = TRUE ),
      farm_total = first( .data[[ j ]], na_rm = TRUE ) * first( imk_areal_tot, na_rm = TRUE ),
      diff       = abs( check - farm_total ),
      .groups    = "drop"
    ) %>%
    summarise(
      max_diff  = max( diff,  na.rm = TRUE ),
      mean_diff = mean( diff, na.rm = TRUE ),
      n_nonzero = sum( diff > 0.01, na.rm = TRUE )
    )
  
  cat(j, "-> max_diff:", result$max_diff, " mean_diff:", result$mean_diff, " n_nonzero:", result$n_nonzero, "\n")
}

# COMMAND ----------

display(datFields)

# COMMAND ----------

datFieldsSub <- subset( datFields, !is.na( alloc_yield ) )
datFieldsSub$DBII <- datFieldsSub$alloc_yield - rowSums( datFieldsSub[ grep( "alloc_OMK_", names( datFieldsSub ), value = TRUE ) ], na.rm = TRUE )

# COMMAND ----------

datFieldsSub$DBI <- with( datFieldsSub, alloc_yield - ( alloc_OMK_1 + alloc_OMK_2 + alloc_OMK_3 + alloc_OMK_4 ) )
datFieldsSub$DBII <- with( datFieldsSub, alloc_yield - ( alloc_OMK_1 + alloc_OMK_2 + alloc_OMK_3 + alloc_OMK_4 + alloc_OMK_5 + alloc_OMK_6 + alloc_OMK_7 + alloc_OMK_8 + alloc_OMK_9 ) )