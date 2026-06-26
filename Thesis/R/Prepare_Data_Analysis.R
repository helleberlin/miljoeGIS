# Databricks notebook source
# indlæs R pakker - basale
install.packages( "readxl" )
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "stringr" )
library( "ggplot2" )
library( "readxl" )

# indlæs R pakker - spatial
system( "sudo apt-get update -qq && sudo apt-get install -y -qq libudunits2-dev libgdal-dev libgeos-dev # libproj-dev && sudo ldconfig", intern = TRUE )
install.packages( c( "Rcpp", "units" ), repos = "https://cloud.r-project.org" )
install.packages( "sf" )
library( "sf" )
install.packages( "terra" )
library( "terra" )

# definér stivej 
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro"

# indlæs retentionskort og udvaskningskort
datRet <- rast( file.path( dirPath, "input/retention/TotalRetention_regioner.tif" ) )
datHvede <- rast( file.path( dirPath, "input/IfroVisa180_j.tif" ) )
datByg <- rast( file.path( dirPath, "input/VASA+_ID44/VASA+_ID44.tif" ) )

# indlæs kystvandoplande
datKyst <- st_read( file.path( dirPath, "input/Kystvandoplande_VP3_2024/Kystvandoplande_VP3_2024.shp" ) )

# indlæs liste med oplande-specifikke 'damage costs'
datCosts <- read_excel( file.path( dirPath, "output/uploaded/Damage cost estimates.xlsx" ) )
costNames <- c( "D_Cost_2024", "ENA_lower_2024", "ENA_upper_2024", "ENA_mean_2024" )
datCosts <- datCosts[ 1:sum( datKyst$KystvandID %in% datCosts$Kystvand_ID ), c( "Kystvand_ID", costNames ) ]

# indlæs klargjorte SEGES og GIS datasæt
datSeges <- readRDS( file.path( dirPath, "output/dat_prepared/dat_prepared_SEGES.rds" ) )
datGis <- arrow::read_parquet( file.path( dirPath, "output/dat_prepared/dat_MGIS_JB.parquet" ) )

# indlæs datasæt med gødningsvariable
sc <- spark_connect( method = "databricks" )
datN <- tbl( sc, "ledelseoekonomi_integration_prod.ifro.ghi" )

## lav kategorier med jordtyper og produktionsgrene
datGis <- datGis %>% mutate(
  # jordboniteter
  jb_kat = case_when(
    jb_kode %in% c(1, 3)  ~ "JB_1_og_3",
    jb_kode %in% c(2, 4)  ~ "JB_2_og_4",
    jb_kode %in% c(5, 6)  ~ "JB_5_og_6",
    jb_kode %in% c(7, 8)  ~ "JB_7_og_8",
    jb_kode == 11         ~ "JB_11",
  ),
  jb3_kat = case_when(
    jb_kat == "JB_1_og_3" ~ "sand_grov", # rettet d. 11.5 (byt fin/grov)
    jb_kat == "JB_2_og_4" ~ "sand_fin", # rettet d 11.5 (byt fin/grov)
    jb_kat %in% c("JB_5_og_6", "JB_7_og_8", "JB_11") ~ "ler",
    TRUE ~ NA_character_ ),
  # produktionsgrene til omkostninger
  pg_kat = case_when(
    afgroede %in% c( "Vårbyg" ) ~ "PG_1",
    afgroede %in% c( "Vinterbyg" ) ~ "PG_2",
    afgroede %in% c( "Vinterhvede", "Vinterhvede, brødhvede" ) ~ "PG_3",
    afgroede %in% c( "Vårhavre", "Vårhvede", "Vårhvede, brødhvede", "Blanding af vårsåede arter", "Vårtriticale", "Vårrug" ) ~ "PG_4",
    afgroede %in% c( "Vinterhybridrug", "Vinterrug", "Vintertriticale" ) ~ "PG_5",
    afgroede %in% c( "Sukkerroer til fabrik", "Fodersukkerroer" ) ~ "PG_6",
    afgroede %in% c( "Rajgræsfrø, alm.", "Rajgræsfrø, alm. 1. år, efterårsudlagt", "Spinatfrø", "Engrapsgræsfrø (plænetype)", "Kløverfrø", "Rødsvingelfrø", "Svingelfrø, strand-", "Timothefrø", "Hundegræsfrø", "Engrapgræsfrø (marktype)", "Engsvingelfrø", "Rajgræs, efterårsudl. hybrid", "Olieræddike til frø, radisefrø", "Rajsvingelfrø", "Dildfrø", "Purløgsfrø", "Blomsterfrø" ) ~ "PG_7",
    afgroede %in% c( "Kartofler, stivelses-", "Kartofler, lægge- (certificerede)", "Kartofler, spise- (pakkeri, vejsalg)", "Kartofler, lægge- (egen opformering)", "Kartofler, pulver/granules-", "Kartofler, friteret/chips/pommes frites", "Kartofler, spise- tidligt høstede med efterafgrøder" ) ~ "PG_8",
    afgroede %in% c( "Vinterraps", "Vårraps" ) ~ "PG_9",
    afgroede %in% c( "Hestebønner", "Ærter", "Lupin", "Korn og bælgsæd (over 50 % bælgsæd)" ) ~ "PG_10",
    afgroede %in% c( "Græs med kløver/lucerne, under 50 % bælgpl. (omdrift)", "Græs uden kløvergræs (omdrift)", "Græs under 50% kløver/lucerne, lavt udbytte (omdrift)", "Miljøgræs MVJ-tilsagn (0 N), omdrift", "Lucernegræs, over 25% græs til slæt inkl. eget foder", "Kløvergræs til fabrik", "Græs  under 50% kløver/lucerne, meget lavt udbytte (omdrift)", "Græs til udegrise, omdrift", "Markbræmme, på omdrift, slåning", "Kløvergræs, over 50% kløver (omdrift)", "Lucerne, slæt", "Lucernegræs, over 50% lucerne (omdrift)", "Græs under 50% kløver/lucerne, ekstremt lavt udbytte (omdrift)", "Græs i omdrift, uden udbetaling af økologi-tilskud" ) ~ "PG_11",
    afgroede %in% c( "Vårbyg, helsæd", "Korn og bælgsæd, helsæd, under 50% bælgsæd", "Grønkorn af vårbyg", "Ærtehelsæd", "Grønkorn af vårrug", "Grønkorn af vårhavre", "Grønkorn af vinterhvede", "Grønkorn af vinterrug", "Blandkorn, vårsået, helsæd", "Grønkorn af vårhvede", "Korn og bælgsæd, grønkorn, under 50% bælgsæd", "Vinterhvede, helsæd", "Vårhavre, helsæd", "Blanding af vårkorn, grønkorn", "Vinterrug, helsæd" ) ~ "PG_12",
    afgroede %in% c( "Permanent græs, normalt udbytte", "Permanent græs og kløvergræs uden norm, under 50 % kløver", "Miljøgræs MVJ-tilsagn (0 N), permanent", "Permanent græs, uden kløver", "Permanent græs, lavt udbytte", "Permanent græs, under 50% kløver/lucerne", "Permanent græs, meget lavt udbytte", "Permanent kløvergræs, over 50% kløver/lucerne", "Permanent græs og kløvergræs uden norm, over 50 % kløver", "Permanent græs, uden udbetaling af økologi-tilskud", "Græs med kløver/lucerne, under 50 % bælgpl. (omdrift) efterårsudlagt i vinterkorn til grønkorn", "Græs og kløvergræs uden norm, under 50 % kløver (omdrift)", "Græs og kløvergræs uden norm, over 50 % kløver (omdrift)" )  ~ "PG_13",
    afgroede %in% c( "Silomajs med græsudlæg", "Silomajs" ) ~ "PG_14",
    afgroede %in% c( "Pil", "Poppel", "Lavskov", "Elefantgræs" ) ~ "PG_15",
    afgroede %in% c("Jordskokker, konsum", "Grøntsager, blandinger", "Asparges", "Jordbær", "Sødkirsebær uden undervækst af græs", "Æbler", "Centnergræskar", "Spinat", "Blandet frugt", "Vindrue") ~ "PG_16",
    afgroede %in% c( "Juletræer og pyntegrønt" ) ~ "PG_17",
    afgroede %in% c( "Brak, slåning", "Bestøverbrak", "Blomsterbrak", "Brak langs vandløb og søer, slåning (alternativ til efterafgrøder)" ) ~ "PG_18",
    afgroede %in% c( "Vinterhavre", "Korn + bælgsæd under 50% bælgsæd", "Boghvede", "Majs til modenhed", "Majs til modenhed med græsudlæg", "Gul sennep", "Solsikke" ) ~ "PG_19",
    afgroede %in% c( "Naturarealer, økologisk jordbrug", "Minivådområder, projekttilsagn", "20-årig Udtagning med fastholdelse, ej landbrugsareal", "Ikke støtteberettiget landbrugsareal", "MVJ ej udtagning, ej landbrugsareal", "Miljøtiltag, ej landbrugsarealer", "Rekreative formål", "Intern kode: Bar jord", "Anden skovdrift", "Skovdrift med fjernelse af ved", "Skovlandbrug, ikke støtteberettiget", "Lysåbne arealer i skov", "Skovrejsning (privat) - forbedring af vandmiljø og grundvandsbeskyttelse", "Skovrejsning, direktivimplementerende uden tilsagn ved Landbrugsstyrelsen", "Skovrejsning (privat) ? kulstofbinding og grundvandsbeskyttelse", "Klimaskovrejsning, national ordning ej Landbrugsstyrelsen" ) ~ "Uden for kategori" 
  ),
  # afgrøde kategorier til udbytter
  crop_kat = case_when(
      afgkode %in% c(1, 10) ~ "Byg",
      afgkode %in% c(2, 11, 13) ~ "Hvede",
      afgkode %in% c(22) ~ "Raps", 
      afgkode %in% c(14, 15, 16) ~ "Rug_Triticale",
      afgkode %in% c(149, 150, 151, 152, 155, 156, 157) ~ "Kartofler", 
      afgkode %in% c(3, 4) ~ "Havre_blandet_korn",
      afgkode %in% c(101, 102, 105, 107, 108, 111, 112, 113, 117, 120, 124, 655, 666, 668) ~ "Frø",
      afgkode %in% c(250, 251, 252, 254, 255, 256, 257, 276, 286, 172, 174, 247, 260, 261, 263, 264, 266, 267, 268, 270, 285, 327, 210, 212, 213, 214, 701, 703, 709) ~ "Grovfoder", 
      afgkode %in% c(30, 31, 32) ~ "Ærter_mv", 
      afgkode %in% c(160, 280) ~ "Sukkerroer",
      afgkode %in% c(216, 218) ~ "Fodermajs",
      afgkode %in% c(592, 593, 591, 596) ~ "Energiafgrøder", 
      afgkode %in% c(429, 450, 401, 513, 524, 528) ~ "Gartneri", 
      afgkode %in% c(180, 24, 5, 19) ~ "Andre_industriafgrøder", 
      afgkode %in% c(180, 24, 5, 19) ~ "Andre_landbrugsindtægter", 
      TRUE ~ NA_character_ )
) %>% filter(
  !is.na( pg_kat ), !is.na( jb_kat ), !is.na( jb3_kat )
) 


#### Forbered Bedrifts Data ####

# lav midlertidigt datasæt med totale hektar per produktionsgren per landbrug (cvr)
datGisPg <- datGis %>%
  group_by( cvr, pg_kat ) %>%
  summarise( hektar = sum( imk_areal, na.rm = TRUE ), .groups = "drop" ) %>%
  pivot_wider(
    names_from  = pg_kat,
    values_from = hektar,
    values_fill = 0
)

# lav midlertidigt datasæt med totale hektar per jordbonitet per landbrug (cvr)
datGisJb <- datGis %>%
  group_by( cvr, jb_kat ) %>%
  summarise( hektar = sum( imk_areal, na.rm = TRUE ), .groups = "drop" ) %>%
  pivot_wider(
    names_from  = jb_kat,
    values_from = hektar,
    values_fill = 0
)

# kombinerer information of produktionsgren og jordbonitet 
datGisPgJb <- inner_join( datGisPg, datGisJb, by = "cvr" )

# vektorere navne på specifikke PG'er og jordboniteter vi har defineret
pgName <- grep( "^PG_", names( datGisPgJb ), value = TRUE )
restName <- "Uden for kategori"
jbName <- grep( "^JB_", names( datGisPgJb ), value = TRUE )

# justerer rækkefølge på produktionsgrene og jordboniteter
datGisPgJb <- datGisPgJb[, c( "cvr", pgName, restName, jbName ) ]

# kombinerer SEGES and GIS datasæt via identiske landbrug (cvr) 
datFarms <- inner_join( datSeges, datGisPgJb, by = "cvr" )

# tjekker om arealer er beregnet korrekt
all.equal( rowSums( datFarms[, c( pgName, restName ) ] ), rowSums( datFarms[, jbName ] ), tolerance = 1e-10 )

# beregn kultiveret landbrugsareal brugt i produktionsgrene
datFarms$landbrugsareal_ha <- rowSums( datFarms[, pgName ] )

# beregn kg N husdyrgødning per hektar landbrugsareal
datFarms$husdyr_N_per_ha <- datFarms$husdyrgoedning_N / datFarms$landbrugsareal_ha

# beregn totale landbrugsareal
datFarms$total_areal_ha <- rowSums( datFarms[, jbName ] )

# beregn procentandel jordbonitet
for( i in jbName ){
  datFarms[[ paste0( i, "_andel" ) ]] <- datFarms[[ i ]] / datFarms[[ "total_areal_ha" ]] * 100
}

# beregn logaritme af dyreenheder, landbrugsareal, vandingsareal og udbytter
for( s in grep("^(DE|landbrugsareal|vandingsareal|U)_", names( datFarms ), value = TRUE ) ) {
  datFarms[[ paste0( "log_", s ) ]] <- ifelse( datFarms[[ s ]] == 0, 0, log( datFarms[[ s ]] ) )
}

# sæt negative værdier til nul for konstrueret PG'er
for( s in grep( "^PG_", names( datFarms ), value = TRUE ) ){
  n <- sum( datFarms[[ s ]] < 0, na.rm = TRUE ) 
  if( n > 0 ){
    warning( n, " negative vaerdier i ", s, " blev sat til nul" )
    datFarms[[ s ]][ !is.na( datFarms[[ s ]] ) & datFarms[[ s ]] < 0 ] <- 0
  }
}


#### Forbered Mark Data ####

# lav GIS datasæt til 'sf' dataframe for at merge med kystvandoplande
datGis <- st_as_sf( datGis, wkt = "geometry", crs = st_crs( datKyst ) )
# brug kun cvr numre der findes i datasættet
datGis <- datGis %>% filter( cvr %in% datFarms$cvr )

# merge GIS datasæt og kystvandoplande via geometri
datFields <- st_intersection( datGis, datKyst[, c( "KystvandID", "KystvandNa" ) ] )

# trimmer retentionskortet så det kun dækker cvr-specifikke marker
datRet <- crop( datRet, vect( datFields ) ) 
datHvede <- crop( datHvede, vect( datFields ) )
datByg <- crop( datByg, vect( datFields ) )

# ekstrahere og tilføj retentionsdataen for hver mark til GIS datasæt
datFields$retention <- extract( datRet, vect( datFields ), fun = mean, na.rm = TRUE )[,2]
datFields$udv_hvede <- extract( datHvede, vect( datFields ), fun = mean, na.rm = TRUE )[,2]
datFields$udv_byg <- extract( datByg, vect( datFields ), fun = mean, na.rm = TRUE )[,2]

# tilføj 'damage costs' til GIS datasæt
datFieldsNoGeo <- st_drop_geometry( datFields ) %>% 
left_join( datCosts, by = c( "KystvandID" = "Kystvand_ID" ) )
for( i in costNames ){
  datFields[[ i ]] <- datFieldsNoGeo[[ i ]]
  datFields$ENA_mean_2024 <- as.numeric( unique( datFieldsNoGeo$ENA_mean_2024[ !is.na( datFieldsNoGeo$ENA_mean_2024 ) ] ) )
}
rm( datFieldsNoGeo )

# hent relevante variable fra gødningsregnskab
datNlocal <- datN %>%
  select( CVR, F_706_1, F_716, F_512 ) %>%
  collect() %>%
  mutate( CVR = as.character( CVR ) )

# sammenkør GIS datasæt og gødningsvariable på cvr numre
datFields <- datFields %>%
  select( -any_of( c( "F_706_1", "F_716", "F_512" ) ) ) %>%
  left_join( datNlocal, by = c( "cvr" = "CVR" ) )

# definér funktion der tilpasser variable (fx, fra 1.234,56 til 1234.56)
clean_number <- function(x) {
  as.numeric( gsub( ",", ".", gsub( "\\.", "", x ) ) )
}

# rens og beregn gødningsvariable
datFields <- datFields %>%
  mutate(
    across( c(F_706_1, F_716, F_512), clean_number ),
    N_udbragt_total = rowSums( across( c( F_706_1, F_716 ) ), na.rm = TRUE ),
    overforbrug_N_kvote = N_udbragt_total - F_512
  ) %>%
  select( -any_of( c( "F_706_1", "F_716", "F_512" ) ) )

# plot distribution af jordtype andele
datSoil <- datFarms %>%
  select( all_of( grep( "andel", names( datFarms ), value = TRUE ) ) ) %>%
  pivot_longer( everything(), names_to = "soil_type", values_to = "share" ) %>% 
  filter( share > 0 )
histShareSoil <- ggplot( datSoil, aes( x = share )) +
  geom_histogram( bins = 30, color = "black", fill = "grey70" ) +
  facet_wrap( ~ soil_type, scales = "free_y" ) +
  labs( x = "Soil Type Share (%)", y = "Frequency" )
ggsave( filename = "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_share_soil.png", plot = histShareSoil, device = "png" )


# gemmer markniveau og bedriftsniveau datasæt
saveRDS( datFarms, file.path( dirPath, "output/dat_prepared/dat_prepared_farms.rds" ) )
saveRDS( datFields, file.path( dirPath, "output/dat_prepared/dat_prepared_fields.rds" ) )

# COMMAND ----------

histShareSoil