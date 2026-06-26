# Databricks notebook source
# MAGIC %md
# MAGIC Indlæser GIS data; fjerner observationer med manglende værdier; og laver en tabel over antallet af NAs for hver kolonne/variabel

# COMMAND ----------

# indlæs R pakker - basale
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "tidyr" )
library( "ggplot2" )

# indlæs R pakker - spatial
# system( "sudo apt-get update -qq && sudo apt-get install -y -qq libudunits2-dev libgdal-dev libgeos-dev # libproj-dev && sudo ldconfig", intern = TRUE )
# install.packages( c( "Rcpp", "units" ), repos = "https://cloud.r-project.org" )
# install.packages( "sf" )
# library( "sf" )

## indlæser klargjort SEGES datasæt
edat <- readRDS( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/dat_prepared_SEGES.rds" )

# indlæser miljoegis datasæt
ab <- spark_connect( method = "databricks" ) 
gdat <- tbl( ab, "ledelseoekonomi_integration_prod.ifro.miljoegis_mark" )

# tjekker datasæt
gdat <- gdat %>% select( marknr, imk_areal, cvr, afgkode, afgroede, markblok, geometry)
colnames( gdat %>% head(1) %>% collect() )

# erstatter tomme strenge "" med NA i character kolonner
gdat <- gdat %>% mutate(
  across( where( is.character ), ~ ifelse(. == "", NA, .) )
)

# frasorterer observationer med tomme cvr-numre
gdat <- gdat %>% filter( 
  cvr != "" & !is.na( cvr ) & afgroede != "" & !is.na( afgroede ) 
)

# frasorterer afgrøder med mindre end 10 ha
gdat_clean <- gdat %>%
  group_by( cvr ) %>%
  filter( sum( imk_areal, na.rm = TRUE ) >= 10 ) %>% 
  ungroup() 

# Tæller NA pr kolonne 
na_counts <- gdat_clean %>% summarise( 
  across( everything(), ~ sum( ifelse( is.na(.), 1, 0 ) ) ) 
) %>% collect()

# laver en overskuelig tabel: kolonne + antal NA
na_counts_tidy <- na_counts %>% pivot_longer(
  everything(), names_to = "kolonne", values_to = "antal_na"
)

## brug kun cvr numre som findes i SEGES data
gdat <- gdat %>% filter( cvr %in% edat$cvr ) 

# indlæser datasættet som lokal R dataframe (obs: kan tage omkring 2-3 min)
gdat_local <- gdat %>% collect()

# Print resultat for tjekke antallet af NAs i kolonner
display( na_counts_tidy )

# gem datasæt som RDS fil
saveRDS( gdat_local, "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/dat_prepared_MGIS.rds" )

# COMMAND ----------

# MAGIC %md
# MAGIC Fjerner observationer som ikke findes i SEGES data; indlæser GIS datasættet som lokal R 'dataframe' (obs: kan tage omkring 2-3 min); tilføjer PG kolonne til GIS datasættet; og gemmer det klargjorte GIS datasæt på databricks.

# COMMAND ----------

# MAGIC %md
# MAGIC Bemærkning om marknr, markblokke og ans-variable - så vidt jeg har forstået
# MAGIC - Marknr er et unik variabelnavn for marken - den identificerer marken. 
# MAGIC - Markblokke (markblok) er et større areal og kan indeholde flere marker, vi skal derfor ikke bruge denne.  
# MAGIC - IMK arealet måler antal ha på marken (dvs. marknr)
# MAGIC - 'afgroede'variablen identificerer den hovedafgrøde landmanden har angivet - der er en afgrødeobservation for hver unik mark i datasættet. De forskellige ans-variable i datasættet er faktisk dem, der angiver efterafgrøder mm. 
# MAGIC - så hvis vi vil tage høje for (særligt) omkostniner forbundet hermed, skal måske have nogle af disse med - men i første omgang tænker jeg, at det er så fint at udelade dem. 

# COMMAND ----------

# Overblik over marknr 

# Antal unikke marknr:

unik_marknr <- gdat %>%
  summarise( n = n_distinct( marknr ) ) %>%
  collect() 

print( unik_marknr ) #resultat: 3661

# Antal observationer pr. marknummer inden for hver CVR
obs_per_mark <- gdat %>%
  group_by( cvr, marknr ) %>%
  summarise( n_obs = n_distinct( afgroede ) ) %>%  # tæller unikke afgrøder pr. fysisk mark
  ungroup()

# Fordeling af 1,2,3... observationer pr. mark (aggregeret over alle CVR)
distribution_mark <- obs_per_mark %>%
  group_by( n_obs ) %>%
  summarise( n_mark = n_distinct( paste0( cvr, "_", marknr ) ) ) %>%
  collect()

print( distribution_mark )

# Histogram for maks 20 observationer
ggplot( distribution_mark, aes( x = n_obs, y = n_mark ) ) +
  geom_col() +
  coord_cartesian( xlim = c(0, 20) ) +
  scale_x_continuous( breaks = 0:20 ) +
  labs(
    x = "Antal observationer pr. mark (unikke afgrøder)",
    y = "Antal marker (marknr)",
    title = "Fordeling af unikke marker med 1,2,3... afgrøder i 2024"
  ) +
  theme_minimal()


# COMMAND ----------

# Tjekker antal unikke marknr pr hovedafgrøde  
afgroede_count <- gdat %>%
  select(marknr, afgroede) %>%       # kun marknr og afgrøde (dvs hovedafgrøde)
  distinct() %>%                     # fjern dubletter, én række per marknr 
  group_by(afgroede) %>%
  summarise(
    n_marknr = n(),                  # antal unikke marknr
  ) %>%
  arrange(desc(n_marknr)) %>%
  collect()                          

print(afgroede_count)

# Visualisering
ggplot(afgroede_count, aes(x = reorder(afgroede, n_marknr), y = n_marknr)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Afgrødetype",
    y = "Antal unikke marknr",
    title = "Fordeling af afgrøder på marker i 2024 (én række pr marknr)"
  ) +
  theme_minimal()

# COMMAND ----------



cvr_nummer <- 26777372  # sæt CVR-nummer her

markblokke_for_cvr <- gdat %>%
  filter(cvr == cvr_nummer) %>%    # kun rækker med dette CVR
  select(marknr, afgroede) %>%  # vælg kolonner du vil se
  distinct() %>%                   # én række per marknr/afgrøde/år
  arrange(marknr) %>%              # sortér efter marknr
  collect()                        # hent til R, hvis det er Spark

# Vis resultatet
display(markblokke_for_cvr)