# Databricks notebook source
# indlæs pakker
install.packages("rstan")
install.packages("miscTools")
install.packages("gridExtra")
install.packages("cowplot")
install.packages( "patchwork" )
install.packages( "png" )
library("rstan")
library("miscTools")
library("gridExtra")
library("ggplot2")
library("cowplot")
library( "patchwork" )
library( "png" )
library( "grid" )

# COMMAND ----------

# vælg observationer for estimering
obs <- "conv" # "eco" # "conv_crops" # "eco_crops" # "eco_mix" # "conv_mix" # "all"

# vælg afhængig variabel for estimering
inp <- "OMK_8"

# vælg stien til det klargjorte datasæt
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# indlæs klargjorte datasæt
#dat <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )

# indlæs koefficienter
datCoef <- readRDS( file.path( dirPath, paste0( "download/coefficients_", obs,".rds" ) ) )

# indlæs OP kombinationer
arrayExpVar <- readRDS( file.path( dirPath, "dat_prepared/arrayExpVar.rds" ) )

# indlæs priors
arrayPriorBeta <- readRDS( file.path( dirPath, "priors/arrayPriorBeta.rds" ) )
arrayPriorDelta <- readRDS( file.path( dirPath, "priors/arrayPriorDelta.rds" ) )
arrayPriorGamma <- readRDS( file.path( dirPath, "priors/arrayPriorGamma.rds" ) )
dimnames( arrayPriorGamma )[[2]] <- sub( "^(vandingsareal|landbrugsareal|DE|U)_", "log_\\1_", dimnames( arrayPriorGamma )[[2]] )
dimnames( arrayPriorGamma )[[2]] <- sub( "^(log_U_[0-9]+)_.*$", "\\1", dimnames(arrayPriorGamma)[[2]] ) 

#### Beta #### 

# beta priors
coefPrior <- arrayPriorBeta[ "Conv", inp, , "meanLog" ][ arrayExpVar[ "Conv", inp, ] ]
coefPrior <- coefPrior[ names( coefPrior ) != "MAST" ]

# estimeret betas
datCoefInp <- datCoef[[ inp ]][ "beta" ]
coefEst <- datCoefInp$beta[ , "mean" ]

# tabel med sammenligning af priors og estimeret betas
print( rbind( prior = exp( coefPrior ), est = exp( coefEst ), diff = exp( coefPrior ) - exp( coefEst ) ) )

# scatterplot med priors vs. estimeret betas
compPlot( coefPrior, coefEst, main = "", xlab = "prior beta", ylab = "est beta" )

# COMMAND ----------

## indlæs priors fra DST/Serie B
arrayPriorBetaDST <- readRDS( file.path( dirPath, "uploaded/arrayPriorBeta.rds" ) )
arrayPriorDeltaDST <- readRDS( file.path( dirPath, "uploaded/arrayPriorDelta.rds" ) )
arrayPriorGammaDST <- readRDS( file.path( dirPath, "uploaded/arrayPriorGamma.rds" ) )

# COMMAND ----------

dimnames( arrayPriorBetaDST )
log( exp( arrayPriorBetaDST[ "Conv", "OK_107", , "meanLog" ] ) * 223.5 )

# COMMAND ----------

arrayPriorBeta[ "Conv", "OMK_8", , "meanLog" ][ arrayExpVar[ "Conv", "OMK_8", ] ]

# COMMAND ----------

exp(-6.629869) * 223.5

# COMMAND ----------

0.2698176 / 223.5

# COMMAND ----------

exp(11.162)

# COMMAND ----------

print( datCoef[[ inp ]] )

# COMMAND ----------

# læs png filen
hist <- readPNG( file.path( dirPath, paste0( "download/hist_", inp, "_", obs,".png" ) ) )

# vis billedet
grid.raster( hist )