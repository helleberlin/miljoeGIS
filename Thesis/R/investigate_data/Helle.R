# Databricks notebook source
# indlæs R pakker
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "tidyr" )
install.packages( "miscTools" )
library( "miscTools" )
library( "ggplot2" )
install.packages( "patchwork" )
library( "patchwork" )

# indlæs økonomi- og gødnings data
sc <- spark_connect( method = "databricks" )
datE <- tbl( sc, "ledelseoekonomi_integration_prod.ifro.oedb" )
datG <- tbl( sc, "ledelseoekonomi_integration_prod.ifro.ghi_blandrk" )

# omdøb og fiks variable
datE <- datE %>% rename( year = "regnskabsaar" )
datG <- datG %>% rename( cvr = "CVR" )
datG <- datG %>% mutate( cvr = as.character( cvr ) )

# merge datasæt efter CVR numre
dat <- inner_join( datE, datG, by = "cvr" )

# indlæs datasæt som lokal R dataframe
dat <- dat %>% collect()

## definér produktionsgrene
dat <- dat %>% mutate( 
## planteproduktion
  # vårbyg, ha
  PG_1 = `003120`, 
  # vinterbyg, ha
  PG_2 = `003130`, 
  # hvede, ha
  PG_3 = `003136` + `003140`, 
  # havre og blandsæd, ha
  PG_4 = `003160` + `003190`,
  # rug & triticale, ha
  PG_5 = `003145` + `003150` + `003170`, 
  # sukkerroer og andre industriafgrøder, ha
  PG_6 = `003410` + `003590`,
  # frø, ha
  PG_7 = `003210` + `003220` + `003230` + `003240` + `003250` + `003255` + 
  `003260` + `003265` + `003270` + `003280` + `003285` + `003290`,
  # kartofler, ha
  PG_8 = `003450` + `003452` + `003454` + `003456` + `003460`,
  # raps og olie produkter, ha
  PG_9 = `003500` + `003520`,
  # bælgsæd (lupiner + hestebønner + ærter til konsum + ærter og andre bælgsæd), ha
  PG_10 = `003524` + `003525` + `003530` + `003540`,
  # sædskiftegræs, ha
  PG_11 = `003820`,
  # helsæd (uden majs) og grønkorn, ha
  PG_12 = `003830` + `003835`,
  # vedvarende græs, ha
  PG_13 = `003860`,
  # majs (helsæd og kolbemajs), ha
  PG_14 = `003870` + `003872`,
  # energiafgrøder (majs til energi + pil til energi + andre energiafgrøder), ha
  PG_15 = `003740` + `003730` + `003700`, 
  # gartneri (grøntsager + frugt og bær + andre gartneriafgrøder), ha
  PG_16 =  `003640` + `003650` + `003690`,
  # Foderroer, ha
  PG_17 = `003810`,
  # brak og brammer, ha
  PG_18 = `003882`,
## husdyrproduktion
  # malkekøer, stk
  PG_19m = `005110`,
  # ammekøer + slagterkvier + slagterkalve, stk
  PG_19k = `005112` + `005129` + `005150`,
  # alle søer inkl. gylte
  PG_19s = `005210`,
  # smågrise (15-35 kg) og slagtersvin, 100 stk
  PG_19g = (`005220` + `005250` ) / 100,
  # fjerkræ (høns + slagterkyllinger), 1000 stk.
  PG_19f = (`005310` + `005320`) / 1000,
  # andre husdyr, stk
  får = `005450`,
  andredyr = `005454`,
## Andre landbrugsindtægter
  # Udlejning (Maskiner, erhvervsejendom, osv.), 10.000 kr
  PG_20u = (`000205` + `000210`) / 10000
)

## definér arealer (ha) der ikke indgår i analysen
dat <- dat %>% mutate(
  # græs/lucerne til tørreri 
  graes_lucerne = `003570`, # Er nul for alle observationer i SEGES regnskab for 2024
  # juletræer (mark), ha
  juletraeer_mark = `003885`,
  # juletræer (skov)
  juletraeer_skov = `003886`,
  # andet grovfoder
  andet_grovfoder =  `003875`,
  # 20-årig udtagning (skov mv)
  tyveaarig_udtagning = `003880`,
  # permanent miljøgræs
  permanent_miljoegraes = `003887`,
  # efterafgrøder 
  efterafgroeder = `003881` + `003890`,
  # grøngødning, økologisk
  groengoeding_oeko = `003877`,
  # skov, krat, hede, mose mv
  skov_krat_hede_mv = `003908`,
  # have, gårdsplads, vej mv
  have_gaardsplads_mv = `003909`,
  # bortforpagtet arealer
  bortforpagtet_ha = `003905`,
  # landbrugsarealer dyrkbart, eget
  landbrugsareal_eget_ha = `003901`,
   # landbrugsarealer dyrkbart, forpagtet
  landbrugsareal_forpagtet_ha = `003911`
)

## definér input-output variable
dat <- dat %>% mutate(
### inputs
 ## DB1 omkostninger
  # udsæd
  OMK_1 = `000230` * (-1),
  # handelsgødning
  OMK_2 = `000235` * (-1),
  # planteværn
  OMK_3 = `000240` * (-1),
  # diverse vedrørende markbrug 
  OMK_4 = `000245` * (-1),
## DB2 omkostninger
  # Maskinstationomk. (ekstern kapacitet) (høst af korn og frøafgrøder + optagning + høst af grovfoder + udbringning af husdyrgødning + maskinstation diverse)
  OMK_5 = (`000310` + `000315` + `000320` + `000325` + `000330`) * (-1),
  # ernegi inkl. brændstof 
  OMK_6 = (`000300` + `000305`) * (-1),
  # heltid/deltid variabel
  HDKODE = ifelse( norm_t_ejendom >= 1665, 1, 2 ),
  # arbejdsomkostninger (ejerlønning + personalelønninger)
  OMK_7 = ifelse( HDKODE == 1, (600000 - `007960`) + `000360` * (-1), (300000 - `007960`) + `000360` * (-1) ), 
## øvrige kapacitetsomkostninger (kr)
  # ejendomskat
  OMK_8 = `000370` * (-1),
  # forsikringer
  OMK_9 = `000375` * (-1),
  # investeringer over driften
  OMK_10 = `000357` * (-1),
  # diverse
  OMK_11 = `000380` * (-1),
## husdyromkostninger (kr)
  # dyrlæge og medicin
  OMK_12 = `000270` * (-1),
  # diverse vedrørende husdyrbrug
  OMK_13 = `000275` * (-1),
  # korn
  OMK_14 = `000250` * (-1),
  # færdigblanding mv
  OMK_15 = `000255` * (-1),
  # andet indkøbt foder
  OMK_16 = `000260` * (-1),
  # eget grovfoder
  OMK_17 = `000265` * (-1),
  # andet eget foder
  OMK_18 = `000267` * (-1),
## vedligehold (kr)
  # fast ejendom
  OMK_19 = `000335` * (-1),
  # grundforbedringer
  OMK_20 = `000340` * (-1),
  # markredskaber
  OMK_21 = `000345` * (-1),
  # staldinventar
  # OMK_22 = `000350` * (-1),
## afskrivninger (kr)
  # driftsbygninger
  OMK_23 = `000410` * (-1),
  # markredskaber
  OMK_24 = `000430` * (-1),
  # blandede driftsmidler
  OMK_25 = `000460` * (-1),
## nedskrivninger (kr)
  # driftsbygninger 
  OMK_26 = `000510` * (-1),
  # markredskaber
  OMK_27 = `000530` * (-1),
  # andet inventar
  OMK_28 = `000550` * (-1),
  # blandede driftsmidler
  OMK_29 = `000560` * (-1),
## totale mark omkostninger (kr)
  # planteomkostninger
  OMK_P = OMK_1 + OMK_2 + OMK_3 + OMK_4,
  # kapacitetsomkostninger
  OMK_K = OMK_5 + OMK_6,
  # totale mark omkostninger
  OMK_T = OMK_P + OMK_K,


### output
## planteindtægter (kr)
  # byg 
  U_1 = `007055` + `007016` + `007025` - `007051`,
  # hvede 
  U_2 = `007063` + `007020` + `007029` - `007059`,
  # havre og øvrige korn (ingen kasseomsæting for øvrige korn)
  U_3 = `007077` + `007008` + `007036` - `007075` + ( `007071` + `007033` - `007069` ),
  # rug og triticale 
  U_4 = (`007082` + `007090` + `007045` - `007080`) + (`007097` + `007010` + `007038` - `007095`),
  # sukkerroer og andre industriafgrøder 
  U_5 = `000110` + `000130`,
  # frøafgrøder
  U_6 = `000105`,
  # kartofler 
  U_7 = `000115`,
  # Raps 
  U_8 = `000120`,
  # ærter mv 
  U_9 = `000125`,
  # sædskiftegræs 
  U_10 = `007216`,
  # helsæd uden majs
  U_11 = `007218`,
  # vedvarende græs
  U_12 = `007219`,
  # majs (til helsæd + kolbemajs)
  U_13 = `007220` + `007221`, 
  # energiafgrøder
  U_14 = `000145`,
  # gartneriafgrøder
  U_15 = `000135`,
  # Foderroer
  U_16 = `007214`,
  # Andre landbrugsindtægter - del 2 (flerårige afgrøder: juletræer, frugttræer osv.)
  U_17 =  `000212`,
## husdyrindtægter (kr)
  # kvæg
  U_18 = `000170` + `000171` + `000172` + `000175`,
  # svin
  U_19 = `000180`,
  # fjerkræ
  U_20 = `000185`,
  # husdyr i øvrigt
  U_21 = `000200`,
## andre landbrugsindtægter (kr)
  # maskinstationindtægter og andre landbrugsindtægter - del 1 (Udlejning af maskiner, erhvervsejendom osv.)
  U_22 = `000205` + `000210`,
## totale udbytter (kr)
  # planteindtægter
  U_P = U_1 + U_2 + U_3 + U_4 + U_5 + U_6 + U_7 + U_8 + U_9 + U_10 + U_11 + U_12 + U_13 + U_14 + U_15 + U_16 + U_17,
  # husdyrindtægter
  U_H = U_18 + U_19 + U_20 + U_21,
  # andre landbrugsindtægter
  U_A = U_22,
  # total udbytte
  U_T = U_P + U_H + U_A
)

## definér bedriftskarakteristika
dat <- dat %>% mutate(
  # jordtype arealer, ha
  grovsand_JB_1_og_3 = `003938`,
  lerjord_JB_7_til_9 = `003932`,
  lerjord_JB_5_til_6 = `003934`,
  finsand_JB_2_og_4 = `003936`,
  humus_JB_11 = `003939`,
  # arealer, der kan vandes, ha
  vandingsareal_ha = `003946`,
  # blandet husdyrgødning, udbragt, Kg N
  husdyrgoedning_N = as.numeric( gsub( ",", ".", gsub( "\\.", "", `C_603_2` ) ) ),
  # dyreenheder, DE
  DE_kvaeg = `005114`,
  DE_svin = `005212`,
  DE_fjerkræ = `005304`,
  # dummy variables
  EKOKODE = ifelse( `006410` == 2, 2, 1 ),
  PRODKODE = ifelse( PG_19m > 0 | PG_19k > 0 | PG_19s > 0 | PG_19g | PG_19f, "husdyr_plante", "plante" ),
  TYPEKODE = case_when( 
    # øko-husdyr
    EKOKODE == 2 & PRODKODE == "husdyr_plante" ~ 1,
    # konv-husdyr
    EKOKODE == 1 & PRODKODE == "husdyr_plante" ~ 2,
    # øko-plante
    EKOKODE == 2 & PRODKODE == "plante" ~ 3,
    # konv-plante
    EKOKODE == 1 & PRODKODE == "plante" ~ 4 ),
  # CVR nummer
  cvr = as.character( cvr ),
  # egnet til analyse
  pg_egnet = ifelse( `006406` == 3, 2, 1 )
)

# fjern bedrifter med ingen planteproduktion
dat <- subset( dat, U_P / U_T != 0 ) 

# fjern bedrifter med kun gartneriproduktion
dat <- subset( dat, U_15 / U_T == 0 )

# fjern bedrifter der er uegnet til analyse
dat <- subset( dat, pg_egnet == 1 )


# beregn totale landbrugsareal
typeJord <- c( "grovsand_JB_1_og_3", "lerjord_JB_7_til_9", "lerjord_JB_5_til_6", "finsand_JB_2_og_4", "humus_JB_11" )
dat$total_areal_ha <- rowSums( dat[, typeJord ] )

# beregn procentandel jordbonitet/jordkvalitet
for( i in typeJord ){
  dat[[ paste0( i, "_andel" ) ]] <- dat[[ i ]] / dat[[ "total_areal_ha" ]] * 100
}

# tjek om totale landbrugsareal er lig med eget + forpagtet dyrkbart landbrugsareal
stopifnot( all.equal( dat$total_areal_ha, dat$landbrugsareal_eget_ha + dat$landbrugsareal_forpagtet_ha, tol = 1e-8 ) )

# COMMAND ----------

# Ejerløn:

# Kun konventionelle planteproducenter
dat_kp <- dat %>%
  filter(EKOKODE == 1, PRODKODE == "plante") %>%
  mutate(type = 4)  # tildel typekode 4 for konventionel planteproducent


sum(dat_kp$`007960`, na.rm = TRUE) 

dat_cvr <- dat_kp %>%
  group_by(cvr) %>%
  summarise(ejerlon = sum(`007960`, na.rm = TRUE)) %>%
  mutate(
    lon_kategori = cut(
      ejerlon,
      breaks = c(0, 100000, 300000, 600000, 1000000, 2000000, Inf),
      labels = c("0-100k", "100-300k", "300-600k", "600k-1m", "1-2m", "2m+"),
      right = FALSE
    )
  )

ggplot(dat_cvr, aes(x = lon_kategori)) +
  geom_bar() +
  labs(
    title = "Owner's salary (per CVR)",
    x = "Salary interval (DKK)",
    y = "Number of farms"
  )


# COMMAND ----------

# Omkostninger Vedligeholdelse (for kun konventionelle planteproducenter)
# Vedligeholdelse andre markredskaber: -59.728.068
# vedligholdelse andet inventar: # -8.765.856
# vedlighold grundforbedringer: # -11.995.578
# vedligehold fast ejendom: #  -18.295.888
# vedligehold staldinventar: # -3.093.668
# vedligehold stamplanter: # 0

# Kun konventionelle planteproducenter
#dat_kp <- dat %>%
 # filter(EKOKODE == 1, PRODKODE == "plante") %>%
 # mutate(type = 4)  # tildel typekode 4 for konventionel planteproducent



  sum(dat_kp$`000350`, na.rm = TRUE) 

# COMMAND ----------

# Omkostninger Vedligeholdelse hele datasættet
# Vedligeholdelse andre markredskaber: -243.883.496
# vedligholdelse andet inventar: # -24.573.153
# vedlighold grundforbedringer: # -50.679.858
# vedligehold fast ejendom: #  -101.617.422
# vedligehold staldinventar: # -214.462.830
# vedligehold stamplanter: # 0

sum(dat$`000342`, na.rm = TRUE) 

# COMMAND ----------

# Husdyrgødning: -483.703
sum(dat$`007792`, na.rm = TRUE) 

# COMMAND ----------

# Afskrivninger kun konventionelle planteproducenter:

# Kun konventionelle planteproducenter
dat_kp <- dat %>%
  filter(EKOKODE == 1, PRODKODE == "plante") %>%
  mutate(type = 4)  # tildel typekode 4 for konventionel planteproducent

# Afskrivninger markredskaber: -88.764.230
# afskrivning andet inventar: (har vi ikke denne i data?)
# afskrivning grundforbedring: (har vi ikke denne i data?) heller ikke afskrivning staldinventar (440?)
# afskrivning driftsbygninger: -44.021.178
# Afskrivning bl. driftsmidler mv: -4390422

  sum(dat_kp$`000440`, na.rm = TRUE) 



# COMMAND ----------

# Energi inkl. brændstof
# Energi: -162.763.335
sum(dat$`000300`, na.rm = TRUE)
# Brændstof: -217.572.827
sum(dat$`000305`, na.rm = TRUE)

# COMMAND ----------

# Maskinstation
# Sum 000330 'maskinstation diverse' #-195.518.537
# sum(dat$`000330`, na.rm = TRUE)
# Sum 000325 'udbringning af husdyrgødning' #-145.387.789
sum(dat$`000325`, na.rm = TRUE)

# Sum all values across the specified columns, ignoring NA # -339.764.364

#sum(dat$`000325`, dat$`000310`, dat$`000315`, dat$`000320`, na.rm = TRUE)

# COMMAND ----------

cvr_nummer <- 21036277  # sæt CVR-nummer her

prøve <- dat %>%
  filter(cvr == cvr_nummer) %>%    # kun rækker med dette CVR
  collect()                        # hent til R, hvis det er Spark

# Vis resultatet
display(prøve)

# COMMAND ----------

cvr_nummer <- 26777372 # sæt CVR-nummer her

# Vælg kun rækker for dette CVR
prøve <- dat %>%
  filter(cvr == cvr_nummer) %>%
  collect()

# Fjern kolonner hvor alle værdier er 0 eller NA
prøve_nonzero <- prøve[, sapply(prøve, function(x) any(x != 0, na.rm = TRUE))]

# Vis resultatet
display(prøve_nonzero)

# COMMAND ----------

#Gødning - Handelsgødning fra økonomidata (6873 - kv): 8.043.223 kg N

# Kun konventionelle planteproducenter
dat_kp <- dat %>%
 filter(EKOKODE == 1, PRODKODE == "plante") %>%
 mutate(type = 4)  # tildel typekode 4 for konventionel planteproducent

# Husdyrgødning: # 1.560.263

sum(dat_kp$husdyrgoedning_N, na.rm = TRUE) 
