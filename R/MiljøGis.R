#### R script for the project ####
# Remove  all objects from R memory 
rm(list=ls())  

# Set a path to the library containing packages
.libPaths("C:/Users/helle/OneDrive/Desktop/packages/packages")

# Check which library paths are loaded
.libPaths()

# Load selected packages# 
pack<-c("car","sandwich","lmtest","RColorBrewer","mgcv","foreign","xtable"
        ,"AER","stargazer", "MASS", "tidyverse", "dplyr","tidyr","sf")

lapply(pack, require, character.only=T)

# Check the working path
getwd()

# Specify  paths
data_path<-"C:/Users/helle/OneDrive/Desktop/MiljøGis/R"


# Set working path
setwd(data_path)

#Check the directory
dir()

mark <- st_read("Marker_2024")

tail(mark) #shows the last observations in the dataset

# Overblik over afgrøder fra marker

navne <- unique(mark$Afgroede)  # Unikke crops navne
navne

names(mark)   # Navnene på variablerne
nrow(mark)    # Antal rækker= 617954
ncol(mark)  # Antal kolonner

###############################################################################
# formatering af sprog
###############################################################################

#3 fordi R har indlæst filen "crops" som noget forkert, skal vi arbejde lidt mere med denne fil:

# Convert encoding from Latin-1 (Windows-1252) to UTF-8 safely - R sprog: crops1$AfgNavn <- iconv(crops1$AfgNavn, from = "latin1", to = "UTF-8")

## Fjerne æ, ø og å

# 1. lave en funktion der ersatter alle æ ø og å med ae, o og aa
replace_danske_tegn <- function(x) {
  x <- gsub("Æ", "Ae", x, fixed = TRUE)
  x <- gsub("æ", "ae", x, fixed = TRUE)
  x <- gsub("Ø", "Oe", x, fixed = TRUE)
  x <- gsub("ø", "oe", x, fixed = TRUE)
  x <- gsub("Å", "Aa", x, fixed = TRUE)
  x <- gsub("å", "aa", x, fixed = TRUE)
  x
}
mark <- mark %>%
  mutate(
    Afgroede = iconv(Afgroede, from = "latin1", to ="UTF-8")
  )

navne <- unique(mark$Afgroede)
navne

#############################################################################
## 3. Ordne data
###############################################################################
## Undersøge NA

colSums(is.na(mark))  # Counts the NA´s in the dataset

mark1 <- mark %>%
  filter(if_all(everything(), ~ !is.na(.)))

colSums(is.na(mark1))  # Counts the NA´s in the dataset

nrow(mark1)    # 587454 rækker

navne <- unique(mark1$Afgroede) # unique() bruges til overblik over unikke navne

navne  # 301 unikke afgrødekategorier

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
mark2_ingengeo <- st_set_geometry(mark2, NULL)

afgroede_sorteret <- mark2_ingengeo %>%
  group_by(Afgroede) %>%
  summarise(total_areal = sum(IMK_areal, na.rm = TRUE)) %>%
  arrange(desc(total_areal))

afgroede_sorteret

#############################################################################
## kategorisering af miljøGis data efter produktions grene
#############################################################################

seges_categories <- list(
  
  "Vårbyg, ha" = c(
    "Varbyg"
  ),
  
  "Vinterbyg, ha" = c(
    "Vinterbyg"
  ),
  
  "Vårhvede, ha" = c(
    "Varhvede",
    "Varhvede, brodhvede"
  ),
  
  "Vinterhvede, ha" = c(
    "Vinterhvede",
    "Vinterhvede, brodhvede"
  ),
  
  "Triticale, ha" = c(
    "Vintertriticale",
    "Vartriticale"
  ),
  
  "Rug, ha" = c(
    "Vinterrug",
    "Vinterhybridrug",
    "Varrug"
  ),
  
  "Havre, ha" = c(
    "Varhavre",
    "Vinterhavre"
  ),
  
  "Kernemajs/majs til modenhed, ha" = c(
    "Majs til modenhed",
    "Majs til modenhed med graesudlaeg"
  ),
  
  "Blandsæd (og andet korn), ha" = c(
    "Blanding af varsaede arter",
    "Blanding af efterarssaede arter",
    "Varspelt",
    "Vinterspelt",
    "Boghvede",
    "Quinoa",
    "Sorghum",
    "Korn + baelgsaed under 50% baelgsaed"
  ),
  
  "Almindelig rajgræs, ha" = c(
    "Rajgraesfro, alm.",
    "Rajgraesfro alm. 1. ar, efterarsudlagt",
    "Rajgraes, hybrid",
    "Rajgraes, efterarsudl. hybrid"
  ),
  
  "Italiensk rajgræs, ha" = c(
    "Rajgraesfro, ital.",
    "Rajgraesfro, ital. 1. ar efterarsudlagt"
  ),
  
  "Hundegræs, ha" = c(
    "Hundegraesfro"
  ),
  
  "Engrapgræs, ha" = c(
    "Engrapgraesfro (marktype)",
    "Engrapsgraesfro (plaenetype)"
  ),
  
  "Rødsvingelfrø, ha" = c(
    "Rodsvingelfro"
  ),
  
  "Bakkesvingel, ha" = c(
    "Svingelfro, bakke- (tidl. Stivbladet)"
  ),
  
  "Strand- og engsvingel, ha" = c(
    "Svingelfro, strand-",
    "Engsvingelfro"
  ),
  
  "Græsfrø (og andre græsfrøafgrøder), ha" = c(
    "Timothefro",
    "Kloverfro",
    "Hvenefro, alm. og krybende",
    "Purlogsfro",
    "Blomsterfro",
    "Blanding af markfro til udsaed",
    "Rajsvingelfro",
    "Rajsvingelfro, efterarsudlagt"
  ),
  
  "Spinatfrø, ha" = c(
    "Spinatfro"
  ),
  
  "Roefrø og andre grøntsagsfrø, ha" = c(
    "Gulerodsfro",
    "Kalfro (hvid- og rodkal)",
    "Bederoefro",
    "Bladbedefro, rodbedefro",
    "Majroefro",
    "Gronkalfro",
    "Kinesisk kalfro",
    "Olieraeddike til fro, radisefro",
    "Persillefro",
    "Dildfro",
    "Valmuefro",
    "Pastinakfro",
    "Kommenfro",
    "Timianfro",
    "Chrysanthemum Garland, fro",
    "Scorzonerrod/skorzonerrodfro",
    "Rucolafro",
    "Karsefro",
    "Gulerodsfro",
    "Baelgplanter, fro",
    "Blanding bredbladet afgrode, fro/kerne"
  ),
  
  "Sukkerroer, ha" = c(
    "Sukkerroer til fabrik"
  ),
  
  "Spisekartofler, ha" = c(
    "Kartofler, spise- (pakkeri, vejsalg)",
    "Kartofler, spise- (proces, skraellet kogte)"
  ),
  
  "Tidlige spisekartofler, ha" = c(
    "Kartofler, spise- tidligt hostede med efterafgroder"
  ),
  
  "Læggekartofler, ha" = c(
    "Kartofler, laegge- (certificerede)",
    "Kartofler, laegge- (egen opformering)"
  ),
  
  "Proceskartofler, ha" = c(
    "Kartofler, friteret/chips/pommes frites"
  ),
  
  "Stivelseskartofler, ha" = c(
    "Kartofler, stivelse-",
    "Kartofler, pulver/granules-"
  ),
  
  "Raps, ha" = c(
    "Vinterraps",
    "Varraps",
  ),
  
  "Andre olieprodukter, ha" = c(
    "Solsikke",
    "Oliehor",
    "Blanding af oliearter",
    "Gul sennep",
    "Rybs"
  ),
  
  "Lupiner, ha" = c(
    "Lupin"
  ),
  
  "Hestebønner, ha" = c(
    "Hestebonner"
  ),
  
  "Ærter til konsum, ha" = c(
    "AErter, konsum"
  ),
  
  "Ærter og andre bælgsæd, ha" = c(
    "AErter",
    "Kikaerter",
    "Linser",
    "Sojabonner",
    "Bonner, andre",
    "Baelgsaed, andre typer til modenhed blanding",
    "Baelgsaed blanding",
    "Baelgsaed, flerarig blanding",
    "Korn og baelgsaed (over 50 % baelgsaed)"
  ),
  
  "Græs/lucerne tørreri, ha" = c(
    "Klovergraes til fabrik",
    "Graes til fabrik (omdrift)",
    "Lucerne til fabrik",
    "Permanent graes til fabrik",
    "Permanent klovergraes til fabrik",
    "Permanent lucernegraes over 25% graes, til fabrik",
    "Permanent graes, fabrik, over 6 tons"
  ),
  
  "Andre industriafgrøder, ha" = c(
    "Hamp",
    "Humle",
    "Blanding, andre industriafgr.",
    "Tagetes, sygdomssanerende plante"
  ),
  
  "Grønsager, ha" = c(
    "Salat (friland)",
    "Salat (drivhus)",
    "Grontsager, blandinger",
    "Grontsager, andre (friland)",
    "Grontsager, andre (drivhus)",
    "Gulerod",
    "Knoldselleri",
    "Pastinak",
    "Rodpersille",
    "Asparges",
    "Rodbede",
    "Asieagurker",
    "Agurker",
    "Tomater",
    "Porre",
    "Log",
    "Spinat",
    "Babyleaves",
    "Gronkal",
    "Hvidkal",
    "Savoykal",
    "Spidskal",
    "Blomkal",
    "Rodkal",
    "Rosenkal",
    "Broccoli",
    "Courgette, squash",
    "Rabarber",
    "Jordskokker, konsum",
    "Bladselleri",
    "Bladpersille",
    "Purlog",
    "Krydderurter (undtagen persille og purlog)",
    "Kinakal",
    "Fodermarvkal",
    "Kalroer",
    "Moskusgraeskar",
    "Centnergraeskar",
    "Mandelgraeskar"
  ),
  
  "Frugt og bær, ha" = c(
    "AEbler",
    "Paerer",
    "Blomme med undervaekst af graes",
    "Blomme uden undervaekst af graes",
    "Sodkirsebaer med undervaekst af graes",
    "Sodkirsebaer uden undervaekst af graes",
    "Surkirsebaer med undervaekst af graes",
    "Surkirsebaer uden undervaekst af graes",
    "Blandet frugt",
    "Anden traefrugt",
    "Ribs",
    "Solbaer",
    "Solbaer, stiklingeopformering",
    "Hindbaer",
    "Jordbaer",
    "Stikkelsbaer",
    "Surbaer",
    "Blabaer",
    "Hyben",
    "Ronnebaer",
    "Havtorn",
    "Baermispel",
    "Storfrugtet tranebaer",
    "Vindrue",
    "Spisedruer",
    "Japan kvaede",
    "Traekvaede",
    "Hyld",
    "Anden buskfrugt",
    "Brombaer",
    "Hassel (Corylus maxima)",
    "Hassel, trae (Corylus avellana)",
    "Valnod (almindelig)",
    "Kastanje (aegte)"
  ),
  
  "Andre gartneriafgrøder, ha" = c(
    "Potteplanter",
    "Blomsterlog",
    "Snitblomster og snitgront",
    "Planteskolekulturer, stauder",
    "Planteskolekulturer, vedplanter, til videresalg",
    "En- og to-arige planter",
    "Stauder",
    "Smaplanter, en-arige",
    "Medicinpl., en- og toarige",
    "Medicinpl., stauder",
    "Medicinpl., vedplanter",
    "Svampe",
    "Containerplads",
    "Lukket system",
    "Graes, rullegraes"
  ),
  
  "Energiafgrøder (andre), ha" = c(
    "Elefantgraes",
    "Rorgraes"
  ),
  
  "Pil til energi, ha" = c(
    "Pil"
  ),
  
  "Majs til helsæd, ha" = c(
    "Silomajs",
    "Silomajs med graesudlaeg"
  ),
  
  "Foderroer, ha" = c(
    "Fodersukkerroer" 
  ),
  
  "Sædskiftegræs, ha" = c(
    "Graes uden klovergraes (omdrift)",
    "Graes med klover/lucerne, under 50 % baelgpl. (omdrift)",
    "Graes med klover/lucerne, under 50 % baelgpl. (omdrift) efterarsudlagt i vinterkorn til gronkorn",
    "Graes med klover/lucerne, over 50 % baelgpl. (omdrift) efterarsudlagt i vinterkorn til gronkorn",
    "Klovergraes, over 50% klover (omdrift)",
    "Graes og klovergraes uden norm, under 50 % klover (omdrift)",
    "Graes og klovergraes uden norm, over 50 % klover (omdrift)",
    "Graes under 50% klover/lucerne, lavt udbytte (omdrift)",
    "Graes  under 50% klover/lucerne, meget lavt udbytte (omdrift)",
    "Graes under 50% klover/lucerne, ekstremt lavt udbytte (omdrift)",
    "Lucernegraes, over 25% graes til slaet inkl. eget foder",
    "Lucernegraes, over 50% lucerne (omdrift)",
    "Lucerne, slaet",
    "Klover til slaet",
    "Graes med vikke og andre baelgplanter, under 50 % baelgpl.",
    "Graes til udegrise, omdrift",
    "Graes i omdrift, uden udbetaling af okologi-tilskud",
    "Miljograes MVJ-tilsagn (0 N), omdrift"
  ),
  
  "Helsæd, ha" = c(
    "Varbyg, helsaed",
    "Varhvede, helsaed",
    "Varhavre, helsaed",
    "Vinterrug, helsaed",
    "Vinterbyg, helsaed",
    "Vinterhvede, helsaed",
    "Vintertriticale, helsaed",
    "Blandkorn, varsaet, helsaed",
    "AErtehelsaed",
    "Korn og baelgsaed, helsaed, under 50% baelgsaed",
    "Korn og baelgsaed, helsaed (over 50 % baelgsaed)"
  ),
  
  "Grønkorn, ha" = c(
    "Gronkorn af varbyg",
    "Gronkorn af varhavre",
    "Gronkorn af varrug",
    "Gronkorn af varhvede",
    "Gronkorn af vinterrug",
    "Gronkorn af vinterhvede",
    "Gronkorn af vinterbyg",
    "Gronkorn af vintertriticale",
    "Gronkorn af hybridrug",
    "Gronkorn af vinterhavre",
    "Blanding af varkorn, gronkorn",
    "Blanding af vinterkorn, gronkorn",
    "Korn og baelgsaed, gronkorn, under 50% baelgsaed"
  ),
  
  "Vedvarende græs, ha" = c(
    "Permanent graes, normalt udbytte",
    "Permanent graes, under 50% klover/lucerne",
    "Permanent graes, lavt udbytte",
    "Permanent graes, meget lavt udbytte",
    "Permanent graes, uden klover",
    "Permanent graes, uden udbetaling af okologi-tilskud",
    "Permanent klovergraes, over 50% klover/lucerne",
    "Permanent graes og klovergraes uden norm, under 50 % klover",
    "Permanent graes og klovergraes uden norm, over 50 % klover",
    "Permanent lucerne og lucernegraes over 50% lucerne",
    "Graes til udegrise, permanent",
    "Honsegard, permanent graes"
  ),
  
  "Kolbemajs, ha" = c(
    "Sukkermajs",
    "Sukkermajs med graesudlaeg",
  ),  
  
  "Andet grovfoder, ha" = c(
    "Fodergulerodder"
  ),
  
  "Grøngødning økologisk, ha" = character(0),  # No direct match
  
  "20 årig udtagning (skov mv.), ha" = c(
    "20-arig Udtagning med fastholdelse, ej landbrugsareal"
  ),
  
  "Brak og bræmmer, ha" = c(
    "Brak, slaning",
    "Blomsterbrak",
    "Bestoverbrak",
    "Markbraemme, pa omdrift, slaning",
    "Markbraemme, pa omdrift, med blomsterblanding",
    "Brak langs vandlob og soer, slaning (alternativ til efterafgroder)"
  ),
  
  "Juletræer/pyntegrønt i mark, ha" = c(
    "Juletraeer og pyntegront",
    "Pyntegront, okologisk jordbrug"
  ),
  
  "Permanent miljøgræs, ha" = c(
    "Miljograes MVJ-tilsagn (0 N), permanent" 
  ),
  
  "Skov, krat, hede, mose mv., ha" = c(
    "Skovrejsning pa tidl. landbrugsjord 1",
    "Skovrejsning pa tidl. landbrugsjord 3",
    "Skovrejsning (privat) - forbedring af vandmiljo og grundvandsbeskyttelse",
    "Skovrejsning (statslig) - forbedring af vandmiljo og grundvandsbeskyttelse",
    "Skovrejsning (privat) ? kulstofbinding og grundvandsbeskyttelse",
    "Skovrejsning, direktivimplementerende uden tilsagn ved Landbrugsstyrelsen",
    "Skovrejsning i projektomrade, som ikke er omfattet af tilsagn",
    "Klimaskovrejsning, national ordning ej Landbrugsstyrelsen",
    "Offentlig skovrejsning",
    "Lavskov",
    "Anden skovdrift",
    "Skovdrift med fjernelse af ved",
    "Skov med biodiversitetsformal",
    "Baeredygtig skovdrift",
    "Baeredygtig skovdrift i Natura 2000-omrade",
    "Lysabne arealer i skov",
    "Skovlandbrug med omdriftsafgroder",
    "Skovlandbrug med permanente afgroder",
    "Skovlandbrug, ikke stotteberettiget",
    "Poppel",
    "Minivadomrader, projekttilsagn",
    "Vadomrader med udtagning",
    "Skovlandbrug med permanent graes",
    "Skovlandbrug med graes i omdrift",
  ),
  
  "Areal omlagt til økologisk drift, ha" = 
    c( "Naturarealer, okologisk jordbrug" ),  
  
  "Andre," = c(
    "Ikke stotteberettiget landbrugsareal",
    "Ovrige afgroder",
    "Intern kode: Bar jord",
    "Rekreative formal",
    "Honsegard uden plantedaekke",
    "El",
    "Klima-lavbundsprojekt, national ordning ej Landbrugsstyrelsen",
    "Miljotiltag, ej landbrugsarealer",
    "MVJ ej udtagning, ej landbrugsareal",
    "MVJ-tilsagn, Udtagning, ej landbrugsareal"
  ),  
  
  "Efterafgrøder miljø, ha" = character(0),
  "Efterafgrøder, ha" = character(0),
)
