# Databricks notebook source
# indlæs pakker
install.packages( "rstan" )
install.packages( "miscTools" )
install.packages( "gridExtra" )
library( "rstan" )
library( "miscTools" )
library( "gridExtra" )
library( "ggplot2" )

# vælg år
year <- 2024

# vælg observationer for estimering
obs <- "conv" # "eco" # "conv_crops" # "eco_crops" # "eco_mix" # "conv_mix" # "all" 

# vælg antal iterationer
iter <- 2000

# brug funktion til at indlæse estimerings resultater
loadEstResult <- function( fileName ) {
  nTries <- 0
  repeat {
    model_est <- try( readRDS( fileName ) )
    nTries <- nTries + 1
    if( inherits( model_est, "try-error") ) {
      if( nTries < 10 ) {
        Sys.sleep( 60 )
        cat( "Trying again...\n" )
      } else {
        cat( "Giving up...\n" )
        model_est <- NULL
        break
      }
    } else {
      break
    }
  }
  return( model_est )
}

# vælg stien til det klargjorte datasæt
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# indlæs klargjort datasæt og OP kombinationer 
dat <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )
arrayExpVar <- readRDS( file.path( dirPath, "dat_prepared/arrayExpVar.rds" ) )

# vælg observationer der skal bruges i analysen
if( obs == "all" ) {
  dat <- dat
} else if ( obs %in% "conv" ) {
  dat <- subset( dat, EKOKODE == 1 )
} else if ( obs %in% "eco" ) {
  dat <- subset( dat, EKOKODE == 2 )
} else if( obs %in% c( "eco_crops", "eco_mix", "conv_mix", "conv_crops" ) ) {
  dat <-  subset( dat, TYPEKAT == obs )
} else {
  stop( "Ukendt model-specifikation. Brug obs = 'eco_crops', 'eco_mix', 'conv_mix', 'conv_crops', 'conv' eller 'all'" )
}

# mappe til indlæsning og gemme resultater
folderResults <- file.path( dirPath, "results" )
if( file.exists( file.path( folderResults, paste0( "datResults_", obs, "_iter", iter, ".rds" ) ) ) ){
  datOld <- readRDS( file.path( folderResults, paste0( "datResults_", obs, "_iter", iter, ".rds" ) ) )
  test <- try( all.equal( dat, datOld[ , names( dat ) ], check.attributes = FALSE ) )
  if( !isTRUE( test ) ) {
    warning( "not using the previously created 'datResults.rds'",
      " because the data used in the estimation seem to have changed" )
    rm( datOld )
    timeSavedOld <- NULL
  } else {
    timeSavedOld <- attr( datOld, "timeSaved" )
  }
} else {
  timeSavedOld <- NULL
}

# navne på filer der indeholder estimeringresultater med specificeret antal iterationer
resultFiles <- list.files( path = folderResults, 
                           pattern = paste0( "^model_est_.*_", obs, "_iter", iter, "\\.rds$" ) )

# date and times when the result files were saved
timeSaved <- rep( NA, length( resultFiles ) )
names( timeSaved ) <- resultFiles

for( fileName in resultFiles ) {
  cat( fileName, ": ", sep = "" )

  # klokkeslæt da filen blev gemt 
  timeSavedNew <- format( file.info( file.path( folderResults, fileName ) )$mtime, "%Y-%m-%d %H:%M:%S" )

  # afhængige variable (OMK)
  depVar <- sub( "model_est_([^_]+_[^_]+)_.*\\.rds", "\\1", fileName )

  # hvis modellen ikke var gen-estimeret efter dette script blev kørt tidligere, så bliver tidligere beregnet output-specifikke input mængder bedrift-specifikke koefficienter brugt 
  if( !is.null( timeSavedOld ) ) {
    if( fileName %in% names( timeSavedOld ) ) {
      if( !is.na( timeSavedOld[ fileName ] ) &&
          timeSavedNew == timeSavedOld[ fileName ] ) {
        cat( "ved brug af tidligere beregnet variable\n" )
        for( s in grep( paste0( "^(coef_|)", depVar, "_" ), names( datOld ),
          value = TRUE ) ) {
          dat[[ s ]] <- datOld[[ s ]]
        }
        timeSaved[ fileName ] <- timeSavedOld[ fileName ]
        next
      }
    }
  }
  cat( "importerer resultat fil..." )

  # indlæs estimerings resultater
  cat( "importing result file..." )
  model_est <- loadEstResult( file.path( folderResults, fileName ) )
  if( is.null( model_est ) ) {
    cat( "failed\n" )
    next
  } else {
    cat( "done\n" )
  }
  timeSaved[ fileName ] <- timeSavedNew

  # model summary
  model_sum <- summary( model_est )$summary

  # observations used in the estimations
  noZero <- attr( model_est, "noZero" )
  if( length( noZero ) != nrow( dat ) ) {
    stop( "incorrect length of vector 'noZero" )
  }

  # explanatory variables
  expVar <- attr( model_est, "expVar" )

  # correspondence between coefficients for organic farms and expVar
  eVec <- attr( model_est, "eVec" )
  
  # z variables
  zVar <- attr( model_est, "zVar" )

  # variables for output-specific input quantities
  for( v in expVar ) {
    dat[[ paste0( depVar, "_", v ) ]] <- NA
  }
  dat[[ paste0( depVar, "_scaleFactor" ) ]] <- NA

  # (scaled) mean estimates of output-specific input quantities
  for( i in 1:sum( noZero ) ) {
    expVarValues <- dat[ which( noZero )[i], expVar ]
    expVarPositive <- expVarValues > 0
    yVal <- dat[ which( noZero )[i], depVar ]
    omegaMu <- model_sum[ paste0( "omegaMu[", i, ",", 1:length(expVar), "]" ), 
      "mean" ]
    omegaSigmaSq <- model_sum[ paste0( "omegaSigmaSq[", 1:length(expVar), "]" ), 
      "mean" ]
    omegaExp <- exp( omegaMu + 0.5 * omegaSigmaSq )
    omegaExpScaled <- ( omegaExp * expVarValues * yVal /
          sum( omegaExp * expVarValues ) ) / expVarValues
    if( sum( expVarPositive ) == 1 ) {
      dat[ which( noZero )[i], paste0( depVar, "_", expVar ) ] <-
        expVarPositive * yVal
      dat[ which( noZero )[i], paste0( depVar, "_scaleFactor" ) ] <-
          ( log( yVal ) - log( expVarValues[ expVarPositive ] ) -
              omegaMu[ expVarPositive ] ) /
          sqrt( omegaSigmaSq[ expVarPositive ] )
    } else {
       obj <- function( scaleFactor ) {
          log( sum( exp( omegaMu[ expVarPositive ] +
            scaleFactor * sqrt( omegaSigmaSq[expVarPositive ] ) ) *
              expVarValues[ expVarPositive ] ) / yVal )^2
    }
    optimRes <- optimize( obj,
        lower = log( 1e-18 ), upper = log( 1e12 ), tol = 1e-6 )
    if( optimRes$objective > 1e-6 ) {
       cat( "non-convergence for input ", depVar, " at observation ",
          which( noZero )[i], " (non-zero observation ", i, ")\n", sep = "" )
          print( optimRes )
    }
    omegaMuScaled <- omegaMu[ expVarPositive ] +
          optimRes$minimum * sqrt( omegaSigmaSq[ expVarPositive ] )
    dat[ which( noZero )[i], paste0( depVar, "_",
         expVar[ expVarPositive ] ) ] <-
           exp( omegaMuScaled ) * expVarValues[ expVarPositive ] * yVal /
              sum( exp( omegaMuScaled ) * expVarValues[ expVarPositive ] )
           dat[ which( noZero )[i], paste0( depVar, "_",
              expVar[ !expVarPositive ] ) ] <- 0
           dat[ which( noZero )[i], paste0( depVar, "_scaleFactor" ) ] <-
              optimRes$minimum
    }
  }

  # if the total input quantity is zero, all output-specific input quantities
  # must be zero
  dat[ dat[[ depVar ]] == 0, paste0( depVar, "_", expVar ) ] <- 0
  # if an output quantity is zero, the corresponding output-specific input
  # quantities must be zero
  for( v in expVar ) {
    dat[ dat[[ v ]] == 0, paste0( depVar, "_", v ) ] <- 0
  }

  # check if the sum of output-specific input quantities is equal
  # to the total input quantity
  test <- all.equal( dat[[ depVar ]][ noZero ],
      rowSums( dat[ noZero, paste0( depVar, "_", expVar ), drop = FALSE ] ),
      check.attributes = FALSE )
  if( !isTRUE( test ) ) {
    stop( "the sum of output-specific input quantities is not equal",
      " to the total input quantity" )
  }

  # calculate input-output coefficients
  for( v in grep( paste0( "^", depVar, "_PG" ), names( dat ), value = TRUE ) ) {
    pgVar <- sub( "^.*(PG_.*)$", "\\1", v )
    dat[[ paste0( "coef_", depVar, "_", pgVar ) ]] <-
      dat[[ v ]] / dat[[ pgVar ]]
  }
}

# add missing output-specific input quantities and coefficients
# that should but cannot be estimated to the data set
for( inp in dimnames( arrayExpVar )[[2]] ) {
  for( out in grep( "^PG", dimnames( arrayExpVar )[[3]], value = TRUE ) ) {
    if( any( arrayExpVar[ , inp, out ] ) ) {
      if( ! paste0( inp, "_", out ) %in% names( dat ) ) {
        # tjek om der er en estimerings fil for denne inp
        inpResultFile <- any( grepl( paste0( "^model_est_", inp, "_" ), resultFiles ) )
        if( !inpResultFile ) {
          cat( inp, out, ": no estimation result file found",
            "-> skipping\n" )
          next
        }
        if( !any( dat[[ inp ]] > 0 & dat[[ out ]] > 0 ) ) {
          cat( inp, out, ": no suitable observation",
            "-> creating variable with zeros\n" )
          dat[[ paste0( inp, "_", out ) ]] <- ifelse(
            dat[[ inp ]] == 0 | dat[[ out ]] == 0, 0, NA )
        } else {
          stop( inp, "_", out, " missing in spite of suitable observations" )
        }
      }
      if( ! paste0( "coef_", inp, "_", out ) %in% names( dat ) ) {
        dat[[ paste0( "coef_", inp, "_", out ) ]] <-
          dat[[ paste0( inp, "_", out ) ]] / dat[[ out ]]
      }
    }
  }
}

# tjek datasæt med output-specifikke input mængder
display( dat )

# gem datasæt output-specifikke input mængder
attr( dat, "timeSaved" ) <- timeSaved
saveRDS( dat, file.path( folderResults, paste0( "datResults_", obs, "_iter", iter, ".rds" ) ) )
write.csv2( dat, file.path( folderResults, paste0( "datResults_", obs, "_iter", iter, ".csv" ) ), row.names = FALSE )

# COMMAND ----------

# MAGIC %md
# MAGIC ## Ekstraher og gem coefficienter

# COMMAND ----------

# opret liste til at gemme koefficienter på tværs af alle modeller
coefList <- list()

for (fileName in resultFiles) {

  depVar <- sub("model_est_([^_]+_[^_]+)_.*\\.rds", "\\1", fileName)

  # indlæs model
  model_est <- loadEstResult(file.path(folderResults, fileName))
  if (is.null(model_est)) next

  model_sum <- summary(model_est)$summary

  # ekspvar og zvar attributter
  expVar <- attr(model_est, "expVar")
  zVar   <- attr(model_est, "zVar")

  # gem parametre i liste for denne model
  coefList[[depVar]] <- list()

  # beta
  beta_rows <- model_sum[grep("^beta\\[", rownames(model_sum)), , drop = FALSE]
  if (nrow(beta_rows) > 0) {
    if (!is.null(expVar) && nrow(beta_rows) == length(expVar)) {
      rownames(beta_rows) <- paste0("beta_", expVar)
    }
    coefList[[depVar]]$beta <- beta_rows
    cat(depVar, ": beta -", nrow(beta_rows), "rækker\n")
  }

  # gamma
  gamma_rows <- model_sum[grep("^gamma\\[", rownames(model_sum)), , drop = FALSE]
  if (nrow(gamma_rows) > 0) {
    if (!is.null(zVar) && nrow(gamma_rows) == length(zVar)) {
      rownames(gamma_rows) <- paste0("gamma_", zVar)
    }
    coefList[[depVar]]$gamma <- gamma_rows
    cat(depVar, ": gamma -", nrow(gamma_rows), "rækker\n")
  }

  # alpha
  alpha_rows <- model_sum[grep("^alpha\\[", rownames(model_sum)), , drop = FALSE]
  if (nrow(alpha_rows) > 0) {
    coefList[[depVar]]$alpha <- alpha_rows
    cat(depVar, ": alpha -", nrow(alpha_rows), "rækker\n")
  }

  # delta
  delta_rows <- model_sum[grep("^delta\\[", rownames(model_sum)), , drop = FALSE]
  if (nrow(delta_rows) > 0) {
    coefList[[depVar]]$delta <- delta_rows
    cat(depVar, ": delta -", nrow(delta_rows), "rækker\n")
  }

  # omegaSigmaSq
  omegaSigmaSq_rows <- model_sum[grep("^omegaSigmaSq\\[", rownames(model_sum)), , drop = FALSE]
  if (nrow(omegaSigmaSq_rows) > 0) {
    if (!is.null(expVar) && nrow(omegaSigmaSq_rows) == length(expVar)) {
      rownames(omegaSigmaSq_rows) <- paste0("omegaSigmaSq_", expVar)
    }
    coefList[[depVar]]$omegaSigmaSq <- omegaSigmaSq_rows
    cat(depVar, ": omegaSigmaSq -", nrow(omegaSigmaSq_rows), "rækker\n")
  }

}

# gem koefficienter
saveRDS(coefList, file.path( dirPath, paste0( "download/coefficients_", obs, ".rds" ) ) )
cat("\nKoefficienter gemt i download/coefficients.rds\n")

# tjek
cat("\nTilgængelige modeller:\n")
print(names(coefList))
cat("\nTilgængelige parametre per model:\n")
for (dep in names(coefList)) {
  cat(dep, ":", paste(names(coefList[[dep]]), collapse = ", "), "\n")
}