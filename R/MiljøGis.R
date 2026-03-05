# indlæs R pakker
packages <- c( "car","sandwich","lmtest","RColorBrewer","mgcv","foreign","xtable"
  ,"AER","stargazer", "MASS", "tidyverse", "dplyr","tidyr","sf", "writexl" )
lapply( packages, require, character.only = T )
rm( packages )

# indlæs data
if ( Sys.info()["sysname"] == "Windows" ) {
  ## Helle
  # sæt vej to bibliotek der indeholder pakker
  .libPaths("C:/Users/helle/OneDrive/Desktop/packages/packages")
  # tjek hvilke biblioteks veje er indlæst
  .libPaths()
  # check arbejds biblioteks vejen 
  getwd()
  # indlæs gis data
  mark <- st_read( "C:/Users/helle/OneDrive/Desktop/miljoeGIS/dat_raw/Marker_2024.shp" ) 
  kyst <- st_read( "C:/Users/helle/OneDrive/Desktop/miljoeGIS/dat_raw/Kystvandoplande_VP3_2024.shp" )
} else if ( Sys.info()["sysname"] == "Darwin" ) {
  ## William
  # indlæs gis data
  mark <- st_read( "/Users/William/Desktop/miljoeGIS/dat_raw/Marker_2024.shp" ) 
  kyst <- st_read( "/Users/William/Desktop/miljoeGIS/dat_raw/Kystvandoplande_VP3_2024.shp" )
}


#### udvælg kystvande ####

kyst7 <- subset( kyst, KystvandID == "111" | KystvandID == "136" | 
    KystvandID == "235" | KystvandID == "232" | KystvandID == "236" |
    KystvandID == "165" | KystvandID == "35" )



#### lav samlet datasæt ####

dat <- st_intersection( mark, kyst7 )
str( dat )

#### Overblik over afgrøder fra marker ####

head(dat) # viser de første observationer (marker) i datasættet
tail(dat) # viser de første observationer (marker) i datasættet
unique(dat$Afgroede)  # Unikke afgrødenavne
names(dat)   # Navnene på variablene
nrow(dat)    # Antal rækker= 617954
ncol(dat)  # Antal kolonner


#### formatering af sprog/bogstaver ####

# Convert encoding from Latin-1 (Windows-1252) to UTF-8 safely - R sprog: crops1$AfgNavn <- iconv(crops1$AfgNavn, from = "latin1", to = "UTF-8")


# lav en funktion der ersatter alle "æ" "ø" og "å" med "ae", "o" og "aa"
replace_danske_tegn <- function(x) {
  x <- gsub("Æ", "Ae", x, fixed = TRUE)
  x <- gsub("æ", "ae", x, fixed = TRUE)
  x <- gsub("Ø", "Oe", x, fixed = TRUE)
  x <- gsub("ø", "oe", x, fixed = TRUE)
  x <- gsub("Å", "Aa", x, fixed = TRUE)
  x <- gsub("å", "aa", x, fixed = TRUE)
  x
}
dat <- dat %>%
  mutate(
    Afgroede = iconv(Afgroede, from = "latin1", to ="UTF-8")
  )



# tjek 'Afgroede'navne
unique( dat$Afgroede ) # unique() bruges til overblik over unikke navne
unique( dat$KystvandNa)


#### ordne data ####

## Undersøge NA
colSums( is.na( dat ) )  # tæller manglende værdier for hver variabel i datasættet

# fjerner manglende værdier 
dat1 <- dat %>%
  filter( if_all( everything(), ~ !is.na(.) ) )
colSums( is.na (dat1 ) ) 
nrow(dat1)    # 142985 rækker
unique(dat1$Afgroede)


# Frasortér afgrøder med mindre end 10 ha
dat2 <- dat1 %>%
  group_by(Afgroede) %>%
  filter(sum(IMK_areal, na.rm = TRUE) >= 10) %>%  
  ungroup() 

unique(dat2$Afgroede) # 192 unikke afgrødekategorier


#### ordne afgrødekategori (afgroede) efter størrelse ####

# Lav en midlertidig version uden geometrien
dat2_ingengeo <- st_set_geometry( dat2, NULL )

afgroede_sorteret <- dat2_ingengeo %>%
  group_by( Afgroede ) %>%
  summarise( total_areal = sum( IMK_areal, na.rm = TRUE ) ) %>%
  arrange( desc( total_areal ) )

# beregn areal andelene i procent
afgroede_sorteret <- afgroede_sorteret %>%
  mutate(
    total_share = total_areal / sum(total_areal) * 100
  )

# lav datasæt med oversigt over de 'vigtigste' afgrøder
if ( Sys.info()["sysname"] == "Windows" ) {
  # Helle
  write_xlsx( afgroede_sorteret, "C:/Users/helle/OneDrive/Desktop/miljoeGIS/dat_processed/afgroede_sorteret.xlsx" )
} else if ( Sys.info()["sysname"] == "Darwin" ) {
  # William
  write_xlsx( afgroede_sorteret, "/Users/William/Desktop/miljoeGIS/dat_processed/afgroede_sorteret.xlsx" )
}


#############################################################################
## kategorisering af miljøGis data efter produktions grene
#############################################################################


  
  PG_1_Vårbyg <- c(
    "Vårbyg"
  )
  
  PG_2_Vinterbyg <- c(
    "Vinterbyg"
  )
  
  PG_3_Hvede <- c(
    "Vårhvede",
    "Vårhvede, brødhvede",
    "Vinterhvede",
    "Vinterhvede, brødhvede"
  )
  
 PG_4_Rug_Triticale <- c(
    "Vintertriticale",
    "Vårtriticale",
    "Vinterrug",
    "Vinterhybridrug",
    "Vårrug"
  )
  
  PG_5_Havre <- c(
    "Vårhavre",
    "Vinterhavre"
  )
  
  PG_6_Sukkerroer_oa <- c ( "Sukkerroer til fabrik",
                          "Korn + bælgsæd under 50% bælgsæd",
                          "Blanding af vårsåede arter",
                          "Blanding af efterårssæde arter",
                          "Vårspelt",
                          "Vinterspelt",
                          "Boghvede",
                          "Sorghum", "Græs, rullegræs"
  )


  
  PG_7_Frø <- c("Rajgræsfrø, alm.",
                "Rajgræsfrø, alm. 1. år, efterårsudlagt",
                "Rajgræs, efterårsudl. hybrid",
                "Rajsvingelfrø",
                "Rajsvingelfrø, efterårsudlagt",
                "Timothefrø",
                "Rajgræsfrø, ital.",
                "Rajgræsfrø, ital. 1. år efterårsudlagt",
                "Hundegræsfrø",
                "Engrapgræsfrø (marktype)",
                "Rødsvingelfrø",
                "Svingelfrø, bakke- (tidl. Stivbladet)",
                "Svingelfrø, strand-",
                "Engsvingelfrø",
                "Kløverfrø",
                "Spinatfrø",
                "Olieræddike til frø, radisefrø",
                "Blanding af markfrø til udsæd",
                "Purløgsfrø",
                "Chrysanthemum Garland, frø",
                "Engrapsgræsfrø (plænetype)",
                "Hvenefrø, alm. og krybende",
                "Dildfrø",
                "Rapgræsfrø, alm.",
                "Blomsterfrø",
                "Rajgræs, hybrid"
                )
  
  PG_8_Kartofler <- c ("Kartofler, spise- (pakkeri, vejsalg)",
                      "Kartofler, spise- tidligt høstede med efterafgrøder",
                      "Kartofler, friteret/chips/pommes frites",
                      "Kartofler, stivelses-",
                      "Kartofler, pulver/granules-",
                      "Kartofler, laegge- (certificerede)",
                      "Kartofler, lægge- (egen opformering)"
                      
  )
  
  PG_9_Raps_oa <- c("Vinterraps", 
                    "Vårraps", 
                    "Solsikke",
                  "Oliehør",
                  "Gul sennep"
                  )
 
  
  PG_10_Bælgsæd <- c( "Hestebønner",
                      "Ærter",
                      "Korn og bælgsæd (over 50 % bælgsæd)",
                      "Lupin",
                      "Bælgsæd blanding",
                      "Sojabønner" 
                      )
  PG_11_sædskiftegræs <- c ("Græs med kløver/lucerne, under 50 % bælgpl. (omdrift)",
             "Græs uden kløvergræs (omdrift)",
             "Græs og kløvergræs uden norm, under 50 % kløver (omdrift)",
             "Græs under 50% kløver/lucerne, lavt udbytte (omdrift)",
             "Miljøgræs MVJ-tilsagn (0 N), omdrift",
             "Græs under 50% kløver/lucerne, meget lavt udbytte (omdrift)",
             "Kløvergræs, over 50% kløver (omdrift)",
             "Græs under 50% kløver/lucerne, ekstremt lavt udbytte (omdrift)",
             "Lucernegræs, over 25% græs til slæt inkl. eget foder",
             "Græs til udegrise, omdrift",
             "Græs til fabrik (omdrift)",
             "Græs med kløver/lucerne, under 50 % bælgpl. (omdrift) efterårsudlagt i vinterkorn til grønkorn",
             "Markbræmme, på omdrift, slåning",
             "Græs og kløvergræs uden norm, over 50 % kløver (omdrift)",
             "Lucernegræs, over 50% lucerne (omdrift)",
             "Kløvergræs til fabrik",
             "Lucerne, slæt",
             "Kløver til slæt")
  
PG_12_Andet_Foder_Energi <-c("Vårbyg, helsæd",
                             "Ærtehelsæd",
                             "Korn og bælgsæd, helsæd, under 50% bælgsæd",
                             "Vinterhvede, helsæd",
                             "Vårhavre, helsæd",
                             "Korn og bælgsæd, helsæd (over 50 % bælgsæd)",
                             "Korn og bælgsæd, grønkorn, under 50% bælgsæd",
                             "Blandkorn, vårsået, helsæd",
                             "Vinterrug, helsæd",
                             "Grønkorn af vårrug",
                             "Grønkorn af vårbyg",
                             "Grønkorn af vårhvede",
                             "Grønkorn af vinterrug",
                             "Grønkorn af vinterhvede",
                             "Blanding af vårkorn, grønkorn",
                             "Grønkorn af vårhavre",
                             "Grønkorn af vinterbyg",
                             "Fodersukkerroer",
                             "Fodermarvkål",
                             "Poppel",
                             "Elefantgræs",
                             "Lavskov",
                             "Pil")

PG_13_Vedvarendegræs <-c("Miljøgræs MVJ-tilsagn (0 N), permanent",
                         "Permanent græs, normalt udbytte",
                         "Permanent græs og kløvergræs uden norm, over 50 % kløver",
                         "Permanent græs og kløvergræs uden norm, under 50 % kløver",
                         "Permanent græs, lavt udbytte",
                         "Permanent græs, meget lavt udbytte",
                         "Permanent græs, uden kløver",
                         "Permanent græs, under 50% kløver/lucerne",
                         "Permanent kløvergræs, over 50% kløver/lucerne",
                         "Græs til udegrise, permanent")

   
PG_14_Majs <- c(
    "Majs til modenhed",
    "Majs til modenhed med græsudlæg",
    "Silomajs med græsudlæg",
    "Silomajs")

PG_15_Brak_Bræmmer <- c("Brak, slåning",
                        "Bestøverbrak",
                        "Blomsterbrak",
                        "Brak langs vandløb og søer, slåning (alternativ til efterafgrøder)"
                        )

Fjernes <- c("MVJ ej udtagning, ej landbrugsareal",
             "Miljøtiltag, ej landbrugsarealer",
             "Anden skovdrift",
             "Skovrejsning (privat) - forbedring af vandmiljø og grundvandsbeskyttelse",
             "Rekreative formål",
             "Skovrejsning, direktivimplementerende uden tilsagn ved Landbrugsstyrelsen",
             "Skovrejsning (statslig) - forbedring af vandmiljø og grundvandsbeskyttelse",
             "Skovrejsning på tidl. landbrugsjord 3",
             "Skov med biodiversitetsformål",
             "Bæredygtig skovdrift",
             "Skovdrift med fjernelse af ved",
             "Skovrejsning (privat) ? kulstofbinding og grundvandsbeskyttelse",
             "Minivådområder, projekttilsagn",
             "Skovlandbrug, ikke støtteberettiget",
             "Ikke støtteberettiget landbrugsareal",
             "Bæredygtig skovdrift i Natura 2000-område",
             "Klimaskovrejsning, national ordning ej Landbrugsstyrelsen",
             "Lysåbne arealer i skov",
             "Lukket system",
             "Naturarealer, økologisk jordbrug",
             "Ærter, konsum",
             "Gulerod",
             "Løg",
             "Salat (friland)",
             "Blomkål",
             "Centnergræskar",
             "Broccoli",
             "Grøntsager, blandinger",
             "Rødbede",
             "Spidskål",
             "Sukkermajs",
             "Rødkål",
             "Porre",
             "Asparges",
             "Grøntsager, andre (friland)",
             "Savoykål",
             "Hvidkål",
             "Knoldselleri",
             "Grønkål",
             "Æbler",
             "Solbær",
             "Vindrue",
             "Sødkirsebær uden undervækst af græs",
             "Ribs",
             "Valnød (almindelig)",
             "Hyld",
             "Kastanje (ægte)",
             "Hassel (Corylus maxima)",
             "Anden træfrugt",
             "Planteskolekulturer, vedplanter, til videresalg",
             "Babyleaves",
             "Planteskolekulturer, stauder",
             "Hassel, træ (Corylus avellana)",
             "Jordbær",
             "Blandet frugt",
             "Pastinak",
             "Rodpersille",
             "Rosenkål",
             "Kinakål",
             "Anden buskfrugt",
             "Havtorn",
             "Blomme med undervækst af græs",
             "En- og to-årige planter",
             "Tagetes, sygdomssanerende plante",
             "Blåbær",
             "Linser",
             "Juletræer og pyntegrønt")
  

  
#### nyt datasæt####


dat2$Produktionsgren <- NA  # Start med tom kategori

# Funktion til tildeling af kategori
tildel_kategori<- function(afgroede) {
  if (afgroede %in% PG_1_Vårbyg) return("PG_1_Vårbyg")
  if (afgroede %in% PG_2_Vinterbyg) return("PG_2_Vinterbyg")
  if (afgroede %in% PG_3_Hvede) return("PG_3_Hvede")
  if (afgroede %in% PG_4_Rug_Triticale) return("PG_4_Rug_Triticale")
  if (afgroede %in% PG_5_Havre) return("PG_5_Havre")
  if (afgroede %in% PG_6_Sukkerroer_oa) return("PG_6_Sukkerroer_oa")
  if (afgroede %in% PG_7_Frø) return("PG_7_Frø")
  if (afgroede %in% PG_8_Kartofler) return("PG_8_Kartofler")
  if (afgroede %in% PG_9_Raps_oa) return("PG_9_Raps_oa")
  if (afgroede %in% PG_10_Bælgsæd) return("PG_10_Bælgsæd")
  if (afgroede %in% PG_11_sædskiftegræs) return("PG_11_sædskiftegræs")
  if (afgroede %in% PG_12_Andet_Foder_Energi) return("PG_12_Andet_Foder_Energi")
  if (afgroede %in% PG_13_Vedvarendegræs) return("PG_13_Vedvarendegræs")
  if (afgroede %in% PG_14_Majs) return("PG_14_Majs")
  if (afgroede %in% PG_15_Brak_Bræmmer) return("PG_15_Brak_Bræmmer")
  return(NA)
}

# Anvend funktionen på dat1$Afgrøde
dat2$Produktionsgren <- sapply(dat2$Afgroede, tildel_kategori)

# Fjern rækker med Afgrøde i Fjernes
dat2 <- dat2[!dat2$Afgroede %in% Fjernes, ]

# Tjek resultat
head(dat2$Produktionsgren)
  
str(dat1)

####Overblik####
# Frasortér afgrøder med mindre end 10 ha
mark2 <- mark1 %>%
  group_by(Afgroede) %>%
  filter(sum(IMK_areal, na.rm = TRUE) >= 10) %>%  
  ungroup() 
# behold kun afgrøder med >=10 ha

navne2<- unique(mark2$Afgroede)
navne2 # 260 unikke afgrødekategorier


## ordne afgrødekategori (afgroede) efter størrelse 

# Lav en midlertidig version uden geometrien
mark1_ingengeo <- st_set_geometry(mark1, NULL)

afgroede_sorteret <- mark1_ingengeo %>%
  group_by(Afgroede) %>%
  summarise(total_areal = sum(IMK_areal, na.rm = TRUE)) %>%
  arrange(desc(total_areal))

afgroede_sorteret
#  Vårhvede
afgroede_sorteret %>%
  filter(Afgroede == "Vårhvede, brødhvede")
print()
