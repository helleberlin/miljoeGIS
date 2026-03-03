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
data_path<-"C:/Users/helle/OneDrive/Desktop/MiljoeGIS/dat_raw"


# Set working path
setwd(data_path)

#Check the directory
dir()

mark <- st_read("Marker_2024.shp")

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
  
 PG_4_Rug/Triticale <- c(
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
  
  PG_6_Majs <- c(
    "Majs til modenhed",
    "Majs til modenhed med græsudlæg"
  )
  
  PG_7_Frø <- c("Rajgræsfrø, alm.",
                "Rajgræsfrø alm. 1. år, efterårsudlagt",
                "Rajgræs, hybrid",
                "Rajgræs, efterårsudl. hybrid","Rajgræsfro, ital.",
                "Rajgræsfro, ital. 1. år efterårsudlagt","Hundegræsfrø",
                "Engrapgræsfrø (marktype)", "Engrapsgræsfro (plænetype)",
                "Rødsvingelfrø","Svingelfrø, bakke- (tidl. Stivbladet)",
                "Svingelfrø, strand-","Engsvingelfrø","Timothefrø",
                "Kløverfrø","Hvenefrø, alm. og krybende",
                "Purløgsfrø", "Blomsterfrø",
                "Blanding af markfrø til udsæd", "Rajsvingelfrø",
                "Rajsvingelfrø, efterårsudlagt", "Spinatfrø", "Gulerodsfrø",
                "Kålfro (hvid- og rødkål)", "Bederoefrø", "Bladbedefrø, rødbedefrø",
                "Majroefrø", "Grønkålfrø", "Kinesisk kålfrø", "Olieræddike til frø, radisefrø",
                "Persillefrø","Dildfrø","Valmuefrø","Pastinakfrø","Kommenfrø","Timianfrø",
                "Chrysanthemum Garland, frø", "Scorzonerrod/skorzonerrodfrø",
                "Rucolafrø","Karsefrø", "Gulerodsfrø","Bælgplanter, frø",
                "Blanding bredbladet afgrøde, frø/kerne")
  
 PG_8_Sukkerroer <-c(
    "Sukkerroer til fabrik"
  )
 
 PG_9_Spisekartofler <- c( "Kartofler, spise- (pakkeri, vejsalg)",
                           "Kartofler, spise- (proces, skrællet kogte)",
                           "Kartofler, spise- tidligt høstede med efterafgrøder")
  
  PG_10_Industrikartofler <- c("Kartofler, friteret/chips/pommes frites", "Kartofler, stivelse-",
                               "Kartofler, pulver/granules-")
  
  PG_11_Raps <- c("Vinterraps", "Vårraps", "Solsikke",
                  "Oliehor",
                  "Blanding af oliearter",
                  "Gul sennep",
                  "Rybs")
  
  PG_12_Hestebønner <-c("Hestebønner")
  
  PG_13_Ærter <- c("Ærter, konsum")
  
  PG_14_Andre_salgsafgrøder <- c(
    "Blanding af vårsæde arter",
    "Blanding af efterårssæde arter",
    "Vårspelt",
    "Vinterspelt",
    "Boghvede",
    "Quinoa",
    "Sorghum",
    "Korn + baelgsaed under 50% baelgsaed","Kartofler, laegge- (certificerede)",
    "Kartofler, lægge- (egen opformering)", "Lupin", "Ærter",
    "Kikærter", "Linser", "Sojabønner", "Bønner, andre", 
    "Bælgsæd, andre typer til modenhed blanding", "Bælgsaed blanding", 
    "Bælgsaed, flerårig blanding",
    "Korn og bælgsæd (over 50 % bælgsæd)") # disse bør gennemgås 
  

  
  
  "Græs/lucerne tørreri, ha" = c(
    "Kløvergraes til fabrik",
    "Græs til fabrik (omdrift)",
    "Lucerne til fabrik",
    "Permanent græs til fabrik",
    "Permanent kløvergræs til fabrik",
    "Permanent lucernegræs over 25% græs, til fabrik",
    "Permanent græs, fabrik, over 6 tons"
  ),
  
  "Andre industriafgrøder, ha" = c(
    "Hamp",
    "Humle",
    "Blanding, andre industriafgr.",
    "Tagetes, sygdomssanerende plante"
  ),
  
  PG_15_Gartneri <- c(
    "Salat (friland)",
    "Salat (drivhus)",
    "Grøntsager, blandinger",
    "Grøntsager, andre (friland)",
    "Grøntsager, andre (drivhus)",
    "Gulerod",
    "Knoldselleri",
    "Pastinak",
    "Rodpersille",
    "Asparges",
    "Rødbede",
    "Asieagurker",
    "Agurker",
    "Tomater",
    "Porre",
    "Løg",
    "Spinat",
    "Babyleaves",
    "Grønkål",
    "Hvidkål",
    "Savoykål",
    "Spidskål",
    "Blomkål",
    "Rødkål",
    "Rosenkål",
    "Broccoli",
    "Courgette, squash",
    "Rabarber",
    "Jordskokker, konsum",
    "Bladselleri",
    "Bladpersille",
    "Purløg",
    "Krydderurter (undtagen persille og purløg)",
    "Kinakål",
    "Fodermarvkål",
    "Kålroer",
    "Moskusgræskar",
    "Centnergræskar",
    "Mandelgræskar",
    "Æbler",
    "Pærer",
    "Blomme med undervækst af græs",
    "Blomme uden undervækst af græs",
    "Sødkirsebør med undervækst af græs",
    "Sødkirsebær uden undervækst af græs",
    "Surkirsebær med undervækst af græs",
    "Surkirsebær uden undervækst af græs",
    "Blandet frugt",
    "Anden træfrugt",
    "Ribs",
    "Solbær",
    "Solbær, stiklingeopformering",
    "Hindbær",
    "Jordbær",
    "Stikkelsbær",
    "Surbær",
    "Blåbær",
    "Hyben",
    "Rønnebær",
    "Havtorn",
    "Bærmispel",
    "Storfrugtet tranebær",
    "Vindrue",
    "Spisedruer",
    "Japan kvæde",
    "Traekvæde",
    "Hyld",
    "Anden buskfrugt",
    "Brombær",
    "Hassel (Corylus maxima)",
    "Hassel, træ (Corylus avellana)",
    "Valnød (almindelig)",
    "Kastanje (ægte)",
    "Potteplanter",
    "Blomsterløg",
    "Snitblomster og snitgrønt",
    "Planteskolekulturer, stauder",
    "Planteskolekulturer, vedplanter, til videresalg",
    "En- og to-arige planter",
    "Stauder",
    "Småplanter, en-årige",
    "Medicinpl., en- og toårige",
    "Medicinpl., stauder",
    "Medicinpl., vedplanter",
    "Svampe",
    "Containerplads",
    "Lukket system",
    "Græs, rullegræs"
  )
  
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
  
PG_24_Juletræer <- c(
    "Juletræer og pyntegrønt",
    "Pyntegrønt, økologisk jordbrug"
  )
  
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
