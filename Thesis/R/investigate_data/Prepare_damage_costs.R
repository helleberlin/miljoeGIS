# Databricks notebook source
# indlæs R pakker - basale
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
install.packages( "readxl" )
library( "readxl" )
library( "ggplot2" )

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
mod <- "ConvCrops" # "EcoCrops" 

# indlæs bedriftsniveau datasæt bestående af observationer fra estimeringen: henter datasæt med resultater for CVR’er hvor TYPEKODE = 4 (konventionelle planteproducenter)
datFarms <- readRDS( file.path( dirPath, paste0( "output/results/datResults", mod, ".rds" ) ) ) 

# indlæs klarggjort markniveau datasæt
datFields <- readRDS( file.path( dirPath, "output/dat_prepared/dat_prepared_fields.rds" ) )

# behold kun observationer i markniveau datasættet der er brugt i estimeringen
datFields <- datFields %>% semi_join( datFarms, by = "cvr" ) 

# laver subset med kun observationer fra de 6 kystvandsoplande
datFieldsSub <- datFields %>% filter( KystvandID %in% c( 2, 93, 128, 131, 136, 165 ) )

# laver datasæt med kun observationer med positive værdier for N_udbragt_Ialt
datFieldsSub <- datFieldsSub %>% filter( !is.na( N_udbragt_total ) & N_udbragt_total > 0 )

# Fjerner alle observationer af PG_ med forventeligt meget lavt eller intet gødningsforbrug så disse ikke tildeles proportionalt N:

datFieldsSub <- datFieldsSub %>%
  filter(!pg_kat %in% c(
    "PG_11",
    "PG_13",
    "PG_15",
    "PG_17",
    "PG_18"
  ))

# dropper geometrien
datFieldsSub <- datFieldsSub %>% st_drop_geometry()

# samlet markareal per cvr tilføjes til datasæt:
datFieldsSub <- datFieldsSub %>%
  group_by( cvr ) %>%
  mutate( samlet_areal_cvr = sum( imk_areal, na.rm = TRUE ) ) %>%
  ungroup()

## beregner damage costs på markniveau for vinterhvedeudvaskningskort
datFieldsSub <- datFieldsSub %>% mutate( 
  # markens størrelse i procent (%)
  markandel_procent = 100 * imk_areal / samlet_areal_cvr,
  
  # samlet udbragt kg N efter markens størrelse i procent (%)
  markandel_N_kg = N_udbragt_total / 100 * markandel_procent,
  
  # andelen af udvasket kg N på hvede marker
  udv_hvede_N_kg = markandel_N_kg / 100 * udv_hvede,
  
  # andelen af udvasket kg N fra hvede marker til kystvande
  kystvand_N_kg = udv_hvede_N_kg / 100 * ( 100 - retention ),
  
  # laver 'damage costs' til numeriske kolonner
  D_Cost_2024 = as.numeric( D_Cost_2024 ),
  ENA_mean_2024 = as.numeric( ENA_mean_2024 ),
  
  # beregner mark-specifikke 'damage costs' ved brug af variable- og uniforme sociale priser
  Specific_D_Cost_mark_hvede = kystvand_N_kg * D_Cost_2024,
  Uniform_D_Cost_mark_hvede = kystvand_N_kg * ENA_mean_2024,
 
  ## beregninger for Vårbyg - beregner damage costs på markniveau for vårbygudvaskningskort
  # andelen af udvasket kg N på vårbygmarker
  
  udv_byg_N_kg = markandel_N_kg / 100 * udv_byg,
  
  # andelen af udvasket kg N fra vårbyg-marker til kystvande
  kystvand_N_kg_byg = udv_byg_N_kg / 100 * ( 100 - retention ),
  
  # beregner mark-specifikke 'damage costs' ved brug af variable- og uniforme sociale priser
  Specific_D_Cost_mark_byg = kystvand_N_kg_byg * D_Cost_2024,
  Uniform_D_Cost_mark_byg = kystvand_N_kg_byg * ENA_mean_2024

)

# COMMAND ----------


lm(N_udbragt_total ~ F_512, data = datFields11) %>% summary()

# COMMAND ----------

cvr_kvote <- datFields %>%
  group_by(cvr) %>%
  summarise(
    has_kvote = any(!is.na(F_512)),
    total_area = sum(imk_areal, na.rm = TRUE),
    .groups = "drop"
  )
  cvr_kvote %>%
  summarise(
    total_cvr = n(),
    missing_kvote = sum(!has_kvote),
    pct_missing = mean(!has_kvote)
  )

# COMMAND ----------

datFields <- datFields %>%
  mutate(
    N_udbragt_total = case_when(
      is.na(N_udbragt_total) | N_udbragt_total == 0 ~ F_512 * 1.138,
      TRUE ~ N_udbragt_total
    )
  )

# COMMAND ----------

library(ggplot2)
# histogram:
datFieldsSub %>%
  select( Specific_D_Cost_mark_hvede ) %>%
  collect() %>%
  ggplot( aes( x = Specific_D_Cost_mark_hvede ) ) +
  geom_histogram( binwidth = 1000, fill = "steelblue", color = "white" ) +
  labs(
    x = "Specific Damage Costs (kr)",
    y = "Number of Fields",
    title = "Histogram (1000 kr intervaller)"
  )

# COMMAND ----------

datFieldsSub %>%
  select( Uniform_D_Cost_mark_hvede) %>%
  collect() %>%
  ggplot( aes( x = Uniform_D_Cost_mark_hvede ) ) +
  geom_histogram( binwidth = 1000, fill = "steelblue", color = "white" ) +
  labs(
    x = "Uniform Damage Costs_hvede (kr)",
    y = "Number of Fields",
    title = "Histogram (1000 kr intervaller)"
  )

# COMMAND ----------

names( datFields )
names( datFieldsSub )

# COMMAND ----------

datFields %>%
  mutate(
    N_group = case_when(
      is.na(N_udbragt_total) ~ "missing",
      N_udbragt_total == 0 ~ "zero",
      N_udbragt_total > 0 ~ "positive"
    )
  ) %>%
  group_by(N_group) %>%
  summarise(antal_cvr = n_distinct(cvr))

# COMMAND ----------

lm(N_udbragt_total ~ F_512,
   data = datFields %>% filter(N_udbragt_total > 0)) %>%
  summary()

# COMMAND ----------

datFields %>%
  mutate(is_zero = N_udbragt_total == 0) %>%
  group_by(is_zero) %>%
  summarise(
    mean_kvote = mean(F_512, na.rm = TRUE),
    mean_area  = mean(imk_areal, na.rm = TRUE),
    n = n_distinct(cvr)
  )

# COMMAND ----------

sum( datFields$N_udbragt_total > 0, na.rm = TRUE )

# COMMAND ----------

# observationer - Kun for datasæt med TYPEKODE = 4 (konventionelle planteproducenter) 

# rækker med positive observationer for N_udbragt_total: 
sum( datFields$N_udbragt_total > 0, na.rm = TRUE ) # 3672

# rækker uden positive observationer (0 eller na)
nrow(datFields) # 9059

# unikke cvr-numre
length(unique(datFields$cvr)) # 248

#cvr-numre med positive N_udbragt_total 
length(unique(datFields$cvr[datFields$N_udbragt_total > 0])) # 94

# cvr-numre uden positive observationer (0 eller na)
length(unique(datFields$cvr[datFields$N_udbragt_total <= 0 | is.na(datFields$N_udbragt_total)])) # 154

# COMMAND ----------

# Undersøger for 6 kystvandsoplande med lokations specifikke damage costs (Kun konventionelle planteproducenter)

length(unique(datFieldsSub$cvr)) # 93

length(unique(datFieldsSub$cvr[datFieldsSub$N_udbragt_total > 0])) # 93

length(unique(datFieldsSub$cvr[datFieldsSub$N_udbragt_total <= 0 | is.na(datFieldsSub$N_udbragt_total)])) # 0

# COMMAND ----------

datFieldsSub %>%
  summarise(
    na_udv_hvede_pct = mean(is.na(udv_hvede)),
    na_udv_byg_pct = mean(is.na(udv_byg)),
    na_retention_pct = mean(is.na(retention)),
    na_imk_areal = mean(is.na(imk_areal))
  )

# COMMAND ----------

# Plotter Nkvote over N_udbragt_total for datFieldsSub
  
plot(datFieldsSub$N_udbragt_total,
     datFieldsSub$F_512,
     xlab = "N_udbragt_total",
     ylab = "F_512")

ggplot(collect(select(datFieldsSub, "F_512", "N_udbragt_total")),
       aes(x = N_udbragt_total, y = F_512)) +
  geom_point()


# COMMAND ----------

# Plotter damage costs over N_udbragt_total for datFieldssub - hvedeudvaskningskort

datFieldsSub$Uniform_D_Cost_mark_1000 <- datFieldsSub$Uniform_D_Cost_mark_hvede / 1000
  
plot(datFieldsSub$N_udbragt_total,
     datFieldsSub$Uniform_D_Cost_mark_1000,
     xlab = "N_udbragt_total",
     ylab = "Uniform_D_Cost_mark_1000")

ggplot(collect(select(datFieldsSub, "Uniform_D_Cost_mark_1000", "N_udbragt_total")),
       aes(x = N_udbragt_total, y = Uniform_D_Cost_mark_1000)) +
  geom_point()

# COMMAND ----------

# Plotter damage costs over N_udbragt_total for datFieldssub vårbygudvaskningskort

datFieldsSub$Uniform_D_Cost_mark_1000 <- datFieldsSub$Uniform_D_Cost_mark_byg / 1000
  
plot(datFieldsSub$N_udbragt_total,
     datFieldsSub$Uniform_D_Cost_mark_1000,
     xlab = "N_udbragt_total",
     ylab = "Uniform_D_Cost_mark_1000")

ggplot(collect(select(datFieldsSub, "Uniform_D_Cost_mark_1000", "N_udbragt_total")),
       aes(x = N_udbragt_total, y = Uniform_D_Cost_mark_1000)) +
  geom_point()

# COMMAND ----------

# MAGIC %md
# MAGIC to do
# MAGIC plot samlet udbragt N mod N-kvoten
# MAGIC lav også damage cost beregninger for byg
# MAGIC undersøg hvordan vi forbedrer uddeling af N er der fx produktionsgrene vi bør tilskrive ingen eller meget lille gødninhgsmængde, hvordan kan vi gøre? 

# COMMAND ----------

# estimering for kovnentionelle planteproducenter med uniforme damagecost - alle 11 kystvandsoplande

# laver datasæt med kun observationer med positive værdier for N_udbragt_Ialt
datFields11 <- datFields %>% filter( !is.na( N_udbragt_total ) & N_udbragt_total > 0 )

# Fjerner alle observationer af PG_ med forventeligt meget lavt eller intet gødningsforbrug så disse ikke tildeles proportionalt N:


datFields11 <- datFields11 %>%
  filter(!pg_kat %in% c(
    "PG_11",
    "PG_13",
    "PG_15",
    "PG_17",
    "PG_18"
  ))

# dropper geometrien
datFields11 <- datFields11 %>% st_drop_geometry()

# samlet markareal per cvr tilføjes til datasæt:
datFields11 <- datFields11 %>%
  group_by( cvr ) %>%
  mutate( samlet_areal_cvr = sum( imk_areal, na.rm = TRUE ) ) %>%
  ungroup()

# tilføjer damagecost variabel ENA_mean_2024 med uniform damage cost

datFields11$ENA_mean_2024 <- as.numeric("111.07337297916223")

## beregner damage costs på markniveau for vinterhvedeudvaskningskort
datFields11 <- datFields11 %>% mutate( 
  # markens størrelse i procent (%)
  markandel_procent = 100 * imk_areal / samlet_areal_cvr,
  
  # samlet udbragt kg N efter markens størrelse i procent (%)
  markandel_N_kg = N_udbragt_total / 100 * markandel_procent,
  
  # andelen af udvasket kg N på hvede marker
  udv_hvede_N_kg = markandel_N_kg / 100 * udv_hvede,
  
  # andelen af udvasket kg N fra hvede marker til kystvande
  kystvand_N_kg = udv_hvede_N_kg / 100 * ( 100 - retention ),
  
  # beregner mark-specifikke 'damage costs' ved brug af uniforme damage costs
  
  Uniform_D_Cost_mark_hvede = kystvand_N_kg * ENA_mean_2024,
 
  ## beregninger for Vårbyg - beregner damage costs på markniveau for vårbygudvaskningskort
  # andelen af udvasket kg N på vårbygmarker
  
  udv_byg_N_kg = markandel_N_kg / 100 * udv_byg,
  
  # andelen af udvasket kg N fra vårbyg-marker til kystvande
  kystvand_N_kg_byg = udv_byg_N_kg / 100 * ( 100 - retention ),
  
  # beregner mark-specifikke 'damage costs' ved brug af uniforme damage costs
  
  Uniform_D_Cost_mark_byg = kystvand_N_kg_byg * ENA_mean_2024

)
length(unique(datFields11$cvr)) 

# COMMAND ----------

display(datFields11)

# COMMAND ----------

# histogram for datFields11 - Uniform_D_Cost_mark_hvede 
datFields11 %>%
  select( Uniform_D_Cost_mark_hvede ) %>%
  collect() %>%
  ggplot( aes( x = Uniform_D_Cost_mark_hvede ) ) +
  geom_histogram( binwidth = 1000, fill = "steelblue", color = "white" ) +
  labs(
    x = "Uniform Damage Costs Hvede (kr)",
    y = "Number of Fields",
    title = "Histogram (1000 kr intervaller)"
  )

# COMMAND ----------

# Plotter Nkvote over N_udbragt_total for datFields11
  
plot(datFields11$N_udbragt_total,
     datFields11$F_512,
     xlab = "N_udbragt_total",
     ylab = "F_512")

ggplot(collect(select(datFields11, "F_512", "N_udbragt_total")),
       aes(x = N_udbragt_total, y = F_512)) +
  geom_point()

# COMMAND ----------

# Plotter damage costs over N_udbragt_total for datFields11
datFields11$Uniform_D_Cost_mark_hvede_1000 <- datFields11$Uniform_D_Cost_mark_hvede / 1000
plot(datFields11$N_udbragt_total,
     datFields11$Uniform_D_Cost_mark_hvede_1000,
     xlab = "N_udbragt_total",
     ylab = "Uniform_D_Cost_mark_hvede_1000")

ggplot(collect(select(datFields11, "Uniform_D_Cost_mark_hvede_1000", "N_udbragt_total")),
       aes(x = N_udbragt_total, y = Uniform_D_Cost_mark_hvede_1000)) +
  geom_point()

# COMMAND ----------

# kun for vinterhvede:

dat_hvede <- datFields11[datFields11$pg_kat == "PG_3", ]

dat_hvede$Uniform_D_Cost_mark_hvede_1000 <- 
  dat_hvede$Uniform_D_Cost_mark_hvede / 1000

plot(dat_hvede$N_udbragt_total,
     dat_hvede$Uniform_D_Cost_mark_hvede_1000,
     xlab = "N_udbragt_total",
     ylab = "Uniform_D_Cost_mark_hvede (1000 kr)")

# COMMAND ----------

#kun for vårbyg:


dat_byg <- datFields11[datFields11$pg_kat == "PG_1", ]

dat_byg$Uniform_D_Cost_mark_byg_1000 <- 
  dat_byg$Uniform_D_Cost_mark_byg / 1000

plot(dat_byg$N_udbragt_total,
     dat_byg$Uniform_D_Cost_mark_byg_1000,
     xlab = "N_udbragt_total",
     ylab = "Uniform_D_Cost_mark_byg (1000 kr)")