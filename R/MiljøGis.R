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



