# Databricks notebook source
## R pakker
install.packages( "abind" )
library( "abind" )
library( "dplyr" )
library( "readxl" )

## definér stivej til det klargjorte data
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output/"

## vælg år
year <- 2024

## indlæs klargjort data
dat <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )

## indlæs priors fra DST/Serie B
arrayPriorBeta <- readRDS( file.path( dirPath, "uploaded/arrayPriorBeta.rds" ) )
arrayPriorDelta <- readRDS( file.path( dirPath, "uploaded/arrayPriorDelta.rds" ) )
arrayPriorGamma <- readRDS( file.path( dirPath, "uploaded/arrayPriorGamma.rds" ) )

## indlæs OP og OPZ kombinationer
arrayExpVar <- readRDS( file.path( dirPath, "dat_prepared/arrayExpVar.rds" ) ) 
arrayZVar <- readRDS( file.path( dirPath, "dat_prepared/arrayZVar.rds" ) ) 

## vælg navne på omkosting(er) der bruges i MSc
codesInp <- dimnames( arrayExpVar )[[2]]

## match variabelnavne på variable i serie B og MSc
# navne på omkostninger fra serie B der skal bruges som priors
OKnames <- c( "OK_114" = "OMK_1", "OK_115" = "OMK_2", "OK_116" = "OMK_3", "OK_124" = "OMK_4", "OK_120" = "OMK_5", "OK_118" = "OMK_6", "OK_117" = "OMK_7", "OK_107" = "OMK_8", "OK_170" = "OMK_9", "OK_140" = "OMK_10", "OK_172" = "OMK_11" )
# navne på produktionsgrene fra serie B der skal bruges som priors
PGnames <- c( "PG_12" = "PG_1", "PG_13"  = "PG_2", "PG_11" = "PG_3", "PG_15" = "PG_4", "PG_14" = "PG_5", "PG_25" = "PG_6", "PG_22" = "PG_7", "PG_23" = "PG_8", "PG_21" = "PG_9", "PG_26" = "PG_10", "PG_44a" = "PG_11", "PG_42a" = "PG_12", "PG_45" = "PG_13", "PG_41a" = "PG_14", "PG_34f" = "PG_15", "PG_31" = "PG_16", "PG_35" = "PG_17", "PG_36"  = "PG_18", "PG_33" = "PG_19", "PG_51" = "PG_19m", "PG_53" = "PG_19k", "PG_61a" = "PG_19s", "PG_64" = "PG_19g", "PG_65" = "PG_19f", "PG_79m" = "PG_20", "PG_79b" = "PG_21", "PG_79n" = "PG_22", "MAST" = "MAST" )
# navne på Z variable fra serie B der skal bruges som priors
Znames <- c( "husdyr_N_pr_ha" = "husdyr_N_per_ha", "ha_saedskifte" = "landbrugsareal_ha", "DE_kvaeg" = "DE_kvaeg", "DE_svin" = "DE_svin", "DE_fjerkrae" = "DE_fjerkrae" )

## subset array's 
arrayPriorBeta <- arrayPriorBeta[ , names( OKnames ), names( PGnames ), ]
arrayPriorDelta <- arrayPriorDelta[ names( OKnames ), names( PGnames ), ]
arrayPriorGammaDST <- arrayPriorGamma[ names( OKnames ), names( Znames ), ]

## omdøb priors fra serie B der skal bruges i MSc
# betas
dimnames( arrayPriorBeta )[[2]] <- unname( OKnames[ dimnames( arrayPriorBeta )[[2]] ] )
dimnames( arrayPriorBeta )[[3]] <- unname( PGnames[ dimnames( arrayPriorBeta )[[3]] ] )
# deltas
dimnames( arrayPriorDelta )[[1]] <- unname( OKnames[ dimnames( arrayPriorDelta )[[1]] ] )
dimnames( arrayPriorDelta )[[2]] <- unname( PGnames[ dimnames( arrayPriorDelta )[[2]] ] )
# gammas
dimnames( arrayPriorGammaDST )[[1]] <- unname( OKnames[ dimnames( arrayPriorGammaDST )[[1]] ] )
dimnames( arrayPriorGammaDST )[[2]] <- unname( Znames[ dimnames( arrayPriorGammaDST )[[2]] ] )

## lav array for gammas
arrayPriorGamma <- array( NA, 
dim = c( length( codesInp ), dim( arrayZVar )[2], 2 ),
dimnames = list( codesInp, dimnames( arrayZVar )[[2]], c( "mean", "sd" ) ) 
)
# sæt baseline priors for gammas
if( year == 2024 ){
  for( i in dimnames( arrayPriorGamma )[[1]] ){
    for( j in dimnames( arrayPriorGamma )[[2]] ){
      if( any( arrayZVar[ , j, i, ] ) ){
        arrayPriorGamma[ i, j, "mean" ] <- 0
        arrayPriorGamma[ i, j, "sd" ] <- 0.2
      }
    }
  }
}
# sæt priors for gammas fra serie B 
for( i in dimnames( arrayPriorGammaDST )[[1]] ){
  for( j in dimnames( arrayPriorGammaDST )[[2]] ){
    if( !is.na( arrayPriorGamma[ i, j, "mean" ] ) && !is.na( arrayPriorGammaDST[ i, j, "mean" ] ) ){
      arrayPriorGamma[ i, j, "mean" ] <- arrayPriorGammaDST[ i, j, "mean" ]
      arrayPriorGamma[ i, j, "sd" ]   <- arrayPriorGammaDST[ i, j, "sd" ]
    }
  }
}

## beregn priors for nitrogen inputs ved brug af nitrogen kvote
coefConvN <- mean( dat$kvote_N[ dat$EKOKODE == 1 ] ) / mean( dat$landbrugsareal_ha[ dat$EKOKODE == 1 ] )
coefEcoN  <- mean( dat$kvote_N[ dat$EKOKODE == 2 ] ) / mean( dat$landbrugsareal_ha[ dat$EKOKODE == 2 ] )
meanLogConvN <- log( coefConvN )
meanLogEcoN  <- log( coefEcoN )
sdLogN <- 0.2
deltaMeanN <- log( coefEcoN / coefConvN )
deltaSdN  <- 0.2

for( inp in c("Q_1", "Q_2") ){
  # tilføj priors for nitrogen til arrayPriorBeta
  arrayBetaN <- array( NA,
    dim = c( 2, 1, dim( arrayPriorBeta )[3], 2 ),
    dimnames = list( dimnames( arrayPriorBeta )[[1]], inp, dimnames( arrayPriorBeta )[[3]], dimnames( arrayPriorBeta )[[4]]
    )
  )
  for( pg in dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Conv", inp, ] ] ){
    arrayBetaN[ "Conv", inp, pg, "meanLog" ] <- meanLogConvN
    arrayBetaN[ "Conv", inp, pg, "sdLog"   ] <- sdLogN
  }
  for( pg in dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Eco", inp, ] ] ){
    arrayBetaN[ "Eco", inp, pg, "meanLog" ] <- meanLogEcoN
    arrayBetaN[ "Eco", inp, pg, "sdLog"   ] <- sdLogN
  }
  arrayPriorBeta <- abind( arrayPriorBeta, arrayBetaN, along = 2 )

  # tilføj priors for udbragt nitrogen (Q_1) til arrayPriorDelta
  arrayDeltaN <- array( NA,
    dim = c( 1, dim( arrayPriorDelta )[2], 2 ),
    dimnames = list( inp, dimnames( arrayPriorDelta )[[2]], dimnames( arrayPriorDelta )[[3]]
    )
  )
  for( pg in intersect(
    dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Conv", inp, ] ],
    dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Eco",  inp, ] ] ) ){
    arrayDeltaN[ inp, pg, "mean" ] <- deltaMeanN
    arrayDeltaN[ inp, pg, "sd"   ] <- deltaSdN
  }
  arrayPriorDelta <- abind( arrayPriorDelta, arrayDeltaN, along = 1 )

  # tilføj priors for udbragt nitrogen (Q_1) til arrayPriorGamma
  arrayPriorGamma[ inp, grep( "^DE_", dimnames( arrayPriorGamma )[[2]], value = TRUE ), "mean" ] <- 0
  arrayPriorGamma[ "Q_1", grep( "^DE_", dimnames( arrayPriorGamma )[[2]], value = TRUE ), "sd" ] <- 0.2
}

### justér priors 
if( year == 2024 ){
  # arbejde
  pgVar <- dimnames(arrayPriorBeta)[[3]]
  arrayPriorBeta[ , "OMK_8", pgVar, "meanLog" ] <- log( exp( arrayPriorBeta[ , "OMK_8", pgVar, "meanLog" ] ) * 223.5 )
  # maskinservices
  arrayPriorBeta[ , "OMK_5", c( "PG_20", "PG_21", "PG_22" ), ] <- NA
  arrayPriorDelta[ "OMK_5", c( "PG_20", "PG_21", "PG_22" ), ] <- NA
}

## gem priors
saveRDS( arrayPriorBeta, file.path( dirPath, "priors/arrayPriorBeta.rds" ) )
saveRDS( arrayPriorDelta, file.path( dirPath, "priors/arrayPriorDelta.rds" ) )
saveRDS( arrayPriorGamma, file.path( dirPath, "priors/arrayPriorGamma.rds" ) )

# COMMAND ----------

arrayPriorBeta[ "Conv", "OMK_8", , ]

# COMMAND ----------

0.00120 * 223.5 * 1000