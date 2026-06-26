# Databricks notebook source
# indlæs R pakker
install.packages("ggplot2")
install.packages( "miscTools" )
library( "sparklyr" )
library( "SparkR" )
library( "dplyr" )
library( "tidyr" )
library( "miscTools" )
library("ggplot2")

# COMMAND ----------

# indlæs økonomi- og gødnings data
sc <- spark_connect( method = "databricks" )
datE <- tbl( sc, "ledelseoekonomi_integration_prod.ifro.oedb" )
datG <- tbl( sc, "ledelseoekonomi_integration_prod.ifro.ghi_blandrk" )
datN <- tbl( sc, "ledelseoekonomi_integration_prod.ifro.ghi" )

# omdøb og fiks variable
datE <- datE %>% rename( year = "regnskabsaar" )
datG <- datG %>% rename( cvr = "CVR" )
datG <- datG %>% mutate( cvr = as.character( cvr ) )
datN <- datN %>% rename( cvr = "CVR" )
datN <- datN %>% mutate( cvr = as.character( cvr ) )

# merge datasæt efter CVR numre
dat <- inner_join( datE, datG, by = "cvr" )
dat <- inner_join( dat, datN, by = "cvr" )

# indlæs datasæt som lokal R dataframe
dat <- dat %>% collect()
datE <- datE %>% collect()
datN <- datN %>% collect()

# COMMAND ----------

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
  # fjerkræ (høns + slagterkyllinger), 1000 stk
  PG_19f = (`005310` + `005320` / (3500 / 167) ) / 1000, # 1 årshøne = 3500 / 167 = 20.96 slagtekyllinger
  # andre husdyr, stk
  får = `005450`,
  andredyr = `005454`,
## Andre landbrugsindtægter
  # maskinstationindtægter, 10.000 kr
  PG_20 = `000205` / 10000,
  # lejeindtægter, 10.000 kr
  PG_21 = `007751` / 10000
)

# Andre kilder, 10.000 kr
dat$PG_22 <- rowSums( dat[ ,c("007752","007753","007754","007755", "007756", "007757", "007759", "007761" ) ], na.rm = TRUE ) / 10000


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
 ## DB1 omkostninger (kr)
  # udsæd
  OMK_1 = `000230` * (-1),
  # handelsgødning
  OMK_2 = `000235` * (-1),
  # planteværn
  OMK_3 = `000240` * (-1),
  # diverse vedrørende markbrug 
  OMK_4 = `000245` * (-1),
## DB2 omkostninger (kr)
  # Maskinstationomk. (ekstern kapacitet) (høst af korn og frøafgrøder + optagning + høst af grovfoder + udbringning af husdyrgødning + maskinstation diverse)
  OMK_5 = (`000310` + `000315` + `000320` + `000325` + `000330`) * (-1),
  # ernegi excl. brændstof 
  OMK_6 = `000300` * (-1),
  # brændstof
  OMK_7 = `000305` * (-1),
  # heltid/deltid variabel
  HDKODE = ifelse( norm_t_ejendom >= 1665, 1, 2 ),
  # arbejdsomkostninger (ejerlønning + personalelønninger)
  OMK_8 = ifelse( HDKODE == 1, (600000 - pmin(`007960`, 500000) ) + `000360` * (-1), (300000 - 
  pmin(`007960`, 200000) ) + `000360` * (-1) ), 
  # vedligeholdelse markredskaber
  OMK_9 = `000345` * (-1),
  # vedligeholdelse andet inventar
  OMK_10 = `000355` * (-1),
  # afskrivninger markredskaber
  OMK_11 = `000430` * (-1),
## DB3 omkostninger (kr)
  # vedligehold fast ejendom
  OMK_12 = `000335` * (-1),
  # vedligehold grundforbedringer
  OMK_13 = `000340` * (-1),
  # afskrivninger driftsbygninger
  OMK_14 = `000410` * (-1),
  # afskrivninger blandede driftsmidler
  OMK_15 = `000460` * (-1),
    # investeringer over driften
  OMK_16 = `000357` * (-1),
  # ejendomskat
  OMK_17 = `000370` * (-1),
  # forsikringer
  OMK_18 = `000375` * (-1),
  # diverse 
  OMK_19 = `000380` * (-1),
  # nedskrivninger driftsbygninger 
  OMK_20 = `000510` * (-1),
  # nedskrivninger markredskaber
  OMK_21 = `000530` * (-1),
  # nedskrivninger andet inventar
  OMK_22 = `000550` * (-1),
  # nedskrivninger blandede driftsmidler
  OMK_23 = `000560` * (-1),
## omkostninger (kr)
  # OMK III
  OMK_I = OMK_1 + OMK_2 + OMK_3 + OMK_4,
  # OMK II
  OMK_II = OMK_5 + OMK_6 + OMK_7 + OMK_8 + OMK_9 + OMK_10 + OMK_11,
  # OMK III
  OMK_III = OMK_12 + OMK_13 + OMK_14 + OMK_15 + OMK_16 + OMK_17 + OMK_18 + OMK_19 + OMK_20 + OMK_21 + OMK_22 + OMK_23, 
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
  # sukkerroer og fodersukkerroer 
  U_5 = `000110` + `007214`,
  # frøafgrøder
  U_6 = `000105`,
  # kartofler 
  U_7 = `000115`, 
  # Raps 
  U_8 = `000120`,
  # ærter mv 
  U_9 = `000125`,
  # grovfoder (sædskiftegræs + helsæd uden majs + vedvarende græs)
  U_10 = `007216` + `007218` + `007219`,
  # fodermajs 
  U_11 = `007220` + `007221`, 
  # energiafgrøder
  U_12 = `000145`,
  # gartneriafgrøder
  U_13 = `000135`,
  # Andre industriafgrøder
  U_14 = `000130`,
  # Andre landbrugsindtægter - del 2 (flerårige afgrøder: juletræer, frugttræer osv.)
  U_15 =  `000212`,
## husdyrindtægter (kr)
  # kvæg
  U_16 = `000170` + `000171` + `000172` + `000175`,
  # svin
  U_17 = `000180`,
  # fjerkræ
  U_18 = `000185`,
  # husdyr i øvrigt
  U_19 = `000200`,
## andre landbrugsindtægter (kr)
  # maskinstationindtægter og andre landbrugsindtægter - del 1 (Udlejning af maskiner, erhvervsejendom osv.)
  U_20 = ( PG_20 + PG_21 + PG_22 ) * 10000,
## totale udbytter (kr)
  # planteindtægter
  U_P = U_1 + U_2 + U_3 + U_4 + U_5 + U_6 + U_7 + U_8 + U_9 + U_10 + U_11 + U_12 + U_13 + U_14 + U_15,
  # husdyrindtægter
  U_H = U_16 + U_17 + U_18 + U_19,
  # andre landbrugsindtægter
  U_A = U_20,
  # total udbytte
  U_T = U_P + U_H + U_A
)

# definér funktion der tilpasser variable (fx, fra 1.234,56 til 1234.56)
clean_number <- function(x) {
  as.numeric(
    gsub( ",", ".", gsub( "\\.", "", x ) )
  )
}

## definér bedrifts-specifikke variable
dat <- dat %>% mutate(
  # arealer, der kan vandes, ha
  vandingsareal_ha = `003946`,
  # blandet husdyrgødning, udbragt, Kg N
  husdyrgoedning_N =  clean_number( C_603_2 ),
  # beregn total gødning, udbragt Kg N
  F_706_1 = clean_number( F_706_1 ),
  F_716   = clean_number( F_716 ),
  kvote_N   = clean_number( F_512 ),
  Q_1 = rowSums( across( c( `F_706_1`, `F_716` ) ) ),
  Q_2 = clean_number( F_512 ),
  # dyreenheder, DE
  DE_kvaeg = `005114`,
  DE_svin = `005212`,
  DE_fjerkræ = `005304`,
  # dummy variables
  EKOKODE = ifelse( `006410` == 2, 2, 1 ),
  PRODKODE = ifelse( (PG_19m + PG_19k + PG_19s + PG_19g * 100 + (PG_19f * 1000) / 10) < 11, "crops", "livestock_crops" ),
  TYPEKAT = case_when( 
    # øko-husdyr
    EKOKODE == 2 & PRODKODE == "livestock_crops" ~ "eco_mix",
    # konv-husdyr
    EKOKODE == 1 & PRODKODE == "livestock_crops" ~ "conv_mix",
    # øko-plante
    EKOKODE == 2 & PRODKODE == "crops" ~ "eco_crops",
    # konv-plante
    EKOKODE == 1 & PRODKODE == "crops" ~ "conv_crops" ),
  # CVR nummer
  cvr = as.character( cvr ),
  # egnet til analyse
  pg_egnet = ifelse( `006406` == 3, 2, 1 )
)

# COMMAND ----------

nrow( dat )

# COMMAND ----------

# fjern bedrifter med ingen planteproduktion
dat <- subset( dat, U_P / U_T != 0 ) 
nrow( dat )

# COMMAND ----------

# gem figur med andelen af gartneriproduktion i total produktion
histShareHort <- ggplot( subset(dat, TYPEKAT == "conv_crops"), aes(x = (U_13 / U_P)*100)) + geom_histogram(bins = 30, fill = "grey", color = "white") +
  labs(x = "Share of Horticultural Production in Total Crop Production (%)", y = "Frequency") + theme_bw()
ggsave( filename = "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_share_hort.png", plot = histShareHort, device = "png" )

# fjern bedrifter med gartneriproduktion
dat <- subset( dat, U_13 / U_P == 0 )
nrow( dat )

# COMMAND ----------

dat <- subset( dat, EKOKODE != 2 )
nrow( dat )

# COMMAND ----------

# fjern bedrifter der er uegnet til analyse
dat <- subset( dat, pg_egnet == 1 )
nrow( dat )

# COMMAND ----------

# tjek hvilke reference numre der tilsammen udgør det totale landbrugsareal
refNrHa <- c( 
  # arealer brugt til PG'er 
  "003120", "003130", "003136", "003140", "003145", "003150", "003160", "003190",
  "003210", "003220", "003230", "003240", "003250", "003255", "003260", "003265", 
  "003270", "003280", "003285", "003290", "003410", "003450", "003452", "003454", 
  "003456", "003460", "003500", "003520", "003524", "003525", "003540", "003590", 
  "003820", "003830", "003835", "003860", "003870", "003872", "003170", "003810",
  "003530", "003640", "003650", "003690", "003700", "003730", "003740", "003882",
  # øvrige arealer
  "003875",  "003570", "003880", "003885"
)
stopifnot( all.equal( rowSums( dat[, refNrHa ] ),  dat$landbrugsareal_eget_ha + dat$landbrugsareal_forpagtet_ha, tol = 1e-03 ) )

# reference numre for husdyr variable
refNrStk <- c( "005110", "005112", "005129", "005150", "005210", "005220", "005250", "005310", "005320" )

# navne på variable som skal med i estimeringen
omkVar <- grep( "^OMK_", names( dat ), value = TRUE )
inpVar <- grep( "Q_", names( dat ), value = TRUE )
outVar <- grep( "^U_", names( dat ), value = TRUE )
pgVar <- c( "PG_20", "PG_21", "PG_22" )
pgVarHusdyr <- grep( "^PG_19+[a-zA-Z]$", names( dat ), value = TRUE )
ekstraVar <- c( "husdyrgoedning_N", "kvote_N", "vandingsareal_ha", "DE_kvaeg", "DE_svin", "DE_fjerkræ" )

# sæt negative værdier til nul for PG'er, OMK'er og U'er
for( s in grep( "^(OMK|Q|PG|U)_", names( dat ), value = TRUE ) ){
  n <- sum( dat[[ s ]] < 0, na.rm = TRUE ) 
  if( n > 0 ){
    warning( n, " negative vaerdier i ", s, " blev sat til nul" )
    dat[[ s ]][ !is.na( dat[[ s ]] ) & dat[[ s ]] < 0 ] <- 0
  }
}

# laver forberedt SEGES datasæt
datPre <- as.data.frame( dat[, c( "cvr", "year", "EKOKODE", "TYPEKAT", "HDKODE", omkVar, inpVar, outVar, ekstraVar, pgVar, pgVarHusdyr ) ] )

# viser forberedt SEGES datasæt
display( datPre )

# gemmer datasæt som RDS fil på databricks
saveRDS( datPre, "/Volumes/ledelseoekonomi_integration_prod/ifro/output/dat_prepared/dat_prepared_SEGES.rds" )

# COMMAND ----------

display( dat[which( dat$TYPEKAT == "conv_crops"),c("U_1", "007055", "007016", "007025", "007051", "007001") ] )
summary( dat[,c("U_1", "007055", "007016", "007025", "007051") ] )

# COMMAND ----------

summary( dat[,c("007001", "U_1")])
table( (dat$`007055` + dat$`007016` + dat$`007025` - dat$`007051` ) >= 0 )
table( (dat$`007001` ) >= 0 )
compPlot( log( (dat$`007055` + dat$`007016` + dat$`007025` - dat$`007051` )[ (dat$`007055` + dat$`007016` + dat$`007025` - dat$`007051`) > 0 & dat$`007001` > 0 ] ), log( (dat$`007001` )[ (dat$`007055` + dat$`007016` + dat$`007025` - dat$`007051`) > 0 & dat$`007001` > 0 ] ) )

# COMMAND ----------

library(dplyr)
library(ggplot2)

dat <- data.frame(
  afgroede = rep(c("Spring Barley", "Winter Barley", "Winter Wheat",
                   "Rapeseed", "Legumes", "Oats", "Rye"), each = 4),
  aar      = rep(c("2022", "2023", "2024", "Normal Yield"), 7),
  udbytte  = c(
    68, 44, 55, 60,
    72, 65, 65, 70,
    87, 75, 72, 82,
    45, 39, 39, 41,
    44, 31, 36, 39,
    55, 37, 48, 51,
    66, 56, 59, 62
  )
)

# Derive factor order from Normal Yield (high → low)
normal_order <- dat %>%
  filter(aar == "Normal Yield") %>%
  arrange(desc(udbytte)) %>%
  pull(afgroede)

dat$afgroede <- factor(dat$afgroede, levels = normal_order)
dat$aar      <- factor(dat$aar, levels = c("2022", "2023", "2024", "Normal Yield"))

# pct labels — named vector must cover all levels
pct_labels <- c(
  "Spring Barley" = "Spring Barley\n-9.2%",
  "Winter Barley" = "Winter Barley\n-6.3%",
  "Winter Wheat"  = "Winter Wheat\n-11.7%",
  "Rapeseed"      = "Rapeseed\n-6.2%",
  "Legumes"       = "Legumes\n-8.4%",
  "Oats"          = "Oats\n-5.6%",
  "Rye"           = "Rye\n-4.7%"
)

ggplot(dat, aes(x = afgroede, y = udbytte, fill = aar)) +
  geom_bar(stat     = "identity",
           position = position_dodge(width = 0.75),
           width    = 0.55) +
  scale_fill_manual(values = c(
    "2022"         = "#2166ac",
    "2023"         = "#b2182b",
    "2024"         = "#4dac26",
    "Normal Yield" = "#bdbdbd"
  )) +
  scale_x_discrete(labels = pct_labels) +
  scale_y_continuous(breaks = seq(0, 90, by = 10),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x       = "",
       y       = "hkg/ha",
       fill    = "") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 10, hjust = 0.5),
    plot.caption       = element_text(hjust = 0, size = 9, color = "grey40")
  )