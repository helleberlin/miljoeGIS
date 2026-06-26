# Databricks notebook source
# indlæs nødvendige R pakker
install.packages( "abind" )
install.packages( "readxl" )
library( "abind" )
library( "readxl" )

# definér stivej til det klargjorte data
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output/"

# indlæs det forberedte datasæt
dat <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_SEGES.rds" ) )

# indlæs OP og OPZ kombinationer som R dataframe
datExpVar <- read_excel( file.path( dirPath, "uploaded/OP_combinations.xlsx" ), sheet = "Analyse" )
matrixExpVar <- as.data.frame( datExpVar )
datZVar <- read_excel( file.path( dirPath, "uploaded/OPZ_combinations.xlsx" ), sheet = "Analyse" )
zVarInfo <- as.data.frame( datZVar )
datUVar <- read_excel( file.path( dirPath, "uploaded/OP_combinations.xlsx" ), sheet = "Sheet3" )
matrixUVar <- as.data.frame( datUVar )

# lav dataframe med OP kombinationer til en matrix
matrixExpVar$Kategori <- sub( "^(OMK_[0-9]+|Q_[0-9]+).*", "\\1", matrixExpVar$Kategori )
row.names( matrixExpVar ) <- matrixExpVar$Kategori
matrixExpVar$Kategori <- NULL
names(matrixExpVar) <- sub( "^(PG_[0-9]+[a-z]?|MAST).*", "\\1", names(matrixExpVar) )
matrixExpVar <- as.matrix( matrixExpVar )

# konvertér matrix til en 3D array (Conventional & Organic, OMK, PG)
arrayExpVar <- array( NA, 
dim = c( 2, nrow( matrixExpVar ), ncol( matrixExpVar ) ), 
dimnames = list( c( "Conv", "Eco" ), rownames( matrixExpVar ), colnames( matrixExpVar ) ) )
arrayExpVar[ "Conv", , ] <- matrixExpVar %in% c(1, 2)  
arrayExpVar[ "Eco", , ] <- matrixExpVar %in% c(1, 3) 

# navne omkostninger og produktionsgrene
codesInp <- dimnames( arrayExpVar )[[2]]
codesOutEst <- dimnames( arrayExpVar )[[3]]

# ordner navne på rækker og kolonner
zVarInfo$Kategori <- sub("^(OMK_[0-9]+|Q_[0-9]+|PG_[0-9]+[a-z]?|MAST).*", "\\1", zVarInfo$Kategori)
row.names( zVarInfo ) <- zVarInfo$Kategori
zVarInfo <- zVarInfo[ zVarInfo$Kategori %in% c( codesInp, codesOutEst ), ]
zVarInfo$Kategori <- NULL

# udvid matricen til en 4D array (Conventional & Organic, Z variable, OMK, PG)
arrayZVar <- do.call( abind, c( lapply( 1:ncol( zVarInfo ), function(x)
  outer( zVarInfo[ codesInp, x ],
         zVarInfo[ c( codesOutEst, "MAST" ), x ] ) ),
  along = 0 ) )
arrayZVar <- abind( arrayZVar > 0, arrayZVar > 0, along = 0 )
dimnames( arrayZVar ) <- list( c( "Conv", "Eco" ), names( zVarInfo ), codesInp, c( codesOutEst, "MAST" ) )

# manuelt ret koefficienter for planteværn (OMK_3)
arrayZVar["Eco", "landbrugsareal_ha", "OMK_3", c( paste0( "PG_", 1:12 ), "PG_14", "PG_15", "PG_17", "PG_18", "PG_19" ) ] <- FALSE
arrayZVar[ "Eco", grep( "^JB_", dimnames( arrayZVar )[[2]], value = TRUE ), "OMK_3", paste0( "PG_", 1:19 ) ] <- FALSE
arrayZVar[ "Eco", "U_1_byg", "OMK_3", c( "PG_1", "PG_2" ) ] <- FALSE 
arrayZVar[ "Eco", "U_2_hvede", "OMK_3", "PG_3" ] <- FALSE
arrayZVar[ "Eco", "U_3_havre", "OMK_3", "PG_4" ] <- FALSE
arrayZVar[ "Eco", "U_4_rug_triticale", "OMK_3", "PG_5" ] <- FALSE
arrayZVar[ "Eco", "U_5_sukkerroer", "OMK_3", "PG_6" ] <- FALSE
arrayZVar[ "Eco", "U_6_froe", "OMK_3", "PG_7" ] <- FALSE
arrayZVar[ "Eco", "U_7_kartofler", "OMK_3", "PG_8" ] <- FALSE
arrayZVar[ "Eco", "U_8_raps", "OMK_3", "PG_9" ] <- FALSE
arrayZVar[ "Eco", "U_9_aerter", "OMK_3", "PG_10" ] <- FALSE
arrayZVar[ "Eco", "U_10_grovfoder", "OMK_3", c( "PG_11", "PG_12", "PG_13" ) ] <- FALSE
arrayZVar[ "Eco", "U_11_majs", "OMK_3", "PG_14" ] <- FALSE
arrayZVar[ "Eco", "U_12_energiafgroeder", "OMK_3", "PG_15" ] <- FALSE
arrayZVar[ "Eco", "U_13_gartneri", "OMK_3", "PG_16" ] <- FALSE
arrayZVar[ "Eco", "U_14_industriafgroeder", "OMK_3", "PG_19" ] <- FALSE
arrayZVar[ "Eco", "U_15_fleraarig_afgroeder", "OMK_3", c( "PG_15", "PG_17" ) ] <- FALSE

# tjek om koefficient ikke estimeret afhænger af Z variabel
for( i in 1:ncol( zVarInfo ) ){
  if( any( arrayZVar[, i, , codesOutEst ] > arrayExpVar[, , ] ) ){
    idx <- which( arrayZVar[, i, , codesOutEst ] > arrayExpVar[, , ], arr.ind = TRUE )[1, ]
    stop( sprintf(
      "Fejl: koefficient ikke estimeret afhænger af Z variabel. Tjek i=%d (%s)",
      i, names( zVarInfo )[i], paste( idx, collapse = ", " )
    ) )
  }
}

## ordn navne på rækker og kolonner
matrixUVar$Kategori <- sub( "^(U_[0-9]+).*", "\\1", matrixUVar$Kategori )
row.names( matrixUVar ) <- matrixUVar$Kategori
matrixUVar$Kategori <- NULL
names( matrixUVar ) <- sub( "^(PG_[0-9]+[a-z]?|MAST).*", "\\1", names(matrixUVar) )
matrixUVar <- as.matrix( matrixUVar )

# konvertér matrix til en 3D array (Conventional & Organic, U, PG)
arrayUVar <- array( NA, 
dim = c( 2, nrow( matrixUVar ), ncol( matrixUVar ) ), 
dimnames = list( c( "Conv", "Eco" ), rownames( matrixUVar ), colnames( matrixUVar ) ) )
arrayUVar[ "Conv", , ] <- matrixUVar %in% c(1, 2)  
arrayUVar[ "Eco", , ] <- matrixUVar %in% c(1, 3) 

#### gem OP- og OPZ kombinationer ####

# gem array'en der indikerer de forklarende variable 
saveRDS( arrayExpVar, file.path( dirPath, "dat_prepared/arrayExpVar.rds" ) )
# gem array der indikerer hvilke Z variable påvirker hvilke koefficienter (omega'er) 
saveRDS( arrayZVar, file.path( dirPath, "dat_prepared/arrayZVar.rds" ) )
# gem array der indikerer udbytte variable
saveRDS( arrayUVar, file.path( dirPath, "dat_prepared/arrayUVar.rds" ) )

# COMMAND ----------

as.data.frame( datExpVar )