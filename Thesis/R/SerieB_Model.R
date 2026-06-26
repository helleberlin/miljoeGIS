# Databricks notebook source
# indlæs pakker
install.packages( "rstan" )
library( "rstan" )

# vælg år
year <- 2024

# vælg observationer for estimering
obs <- "conv" # "eco" # "conv_crops" # "eco_crops" # "eco_mix" # "conv_mix" # "all"

# vælg afhængig variabel for estimering
inp <- "OMK_8"

# vælg stien til det klargjorte datasæt
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# indlæs klargjorte datasæt
dat <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )

# ret stavning i dat: fjerkræ -> fjerkrae for konsistens med arrayZVar
names( dat ) <- gsub( "fjerkræ", "fjerkrae", names( dat ) )
 
# vælg observationer der skal bruges i estimeringen
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

# indlæs information om hvilke produktionsgrene er inkluderet i hvilke ligninger
arrayExpVar <- readRDS( file.path( dirPath, "dat_prepared/arrayExpVar.rds" ) ) # OP kombinationer 
arrayZVar <- readRDS( file.path( dirPath, "dat_prepared/arrayZVar.rds" ) ) # OPZ kombinationer
dimnames( arrayZVar )[[2]] <- sub( "^(vandingsareal|landbrugsareal|DE|U)_", "log_\\1_", dimnames( arrayZVar )[[2]] )
dimnames( arrayZVar )[[2]] <- sub( "^(log_U_[0-9]+)_.*$", "\\1", dimnames(arrayZVar)[[2]] )
arrayZVar <- 1 * arrayZVar # omdefinerer fra TRUE/FALSE til 1/0

# indlæs priors
arrayPriorBeta <- readRDS( file.path( dirPath, "priors/arrayPriorBeta.rds" ) )
arrayPriorDelta <- readRDS( file.path( dirPath, "priors/arrayPriorDelta.rds" ) )
arrayPriorGamma <- readRDS( file.path( dirPath, "priors/arrayPriorGamma.rds" ) )
dimnames( arrayPriorGamma )[[2]] <- sub( "^(vandingsareal|landbrugsareal|DE|U)_", "log_\\1_", dimnames( arrayPriorGamma )[[2]] )
dimnames( arrayPriorGamma )[[2]] <- sub( "^(log_U_[0-9]+)_.*$", "\\1", dimnames(arrayPriorGamma)[[2]] ) 

# all inputs
inpAll <- dimnames( arrayExpVar )[[2]]  # OP-kombinationen mellem omkostninger og produktionsgrene (PG =explanatory variable); [[2]] = omkostningsvariablenavne

# navne på OMK og PG fra serie B der skal bruges som priors
OKnames <- c( "OK_114" = "OMK_1", "OK_115" = "OMK_2", "OK_116" = "OMK_3", "OK_124" = "OMK_4", "OK_120" = "OMK_5", "OK_118" = "OMK_6", "OK_117" = "OMK_7", "OK_107" = "OMK_8", "OK_170" = "OMK_9", "OK_140" = "OMK_10", "OK_172" = "OMK_11" )
PGnames <- c( "PG_12" = "PG_1", "PG_13"  = "PG_2", "PG_11" = "PG_3", "PG_15" = "PG_4", "PG_14" = "PG_5", "PG_25" = "PG_6", "PG_22" = "PG_7", "PG_23" = "PG_8", "PG_21" = "PG_9", "PG_26" = "PG_10", "PG_44a" = "PG_11", "PG_42a" = "PG_12", "PG_45" = "PG_13", "PG_41a" = "PG_14", "PG_34f" = "PG_15", "PG_31" = "PG_16", "PG_35" = "PG_17", "PG_36"  = "PG_18", "PG_33" = "PG_19", "PG_51" = "PG_19m", "PG_53" = "PG_19k", "PG_61a" = "PG_19s", "PG_64" = "PG_19g", "PG_65" = "PG_19f", "PG_79m" = "PG_20", "PG_79b" = "PG_21", "PG_79n" = "PG_22" )

# priors for sigmaScale (identiske med estimeringerne i Serie B for 2024)
sigmaScaleAll <- read.csv2( file.path( dirPath, "uploaded/sigmaScale.csv" ), row.names = 1 )
sigmaScaleOK <- sigmaScaleAll[ names( OKnames ), paste0( "Y", year ) ]
names( sigmaScaleOK ) <- unname( OKnames )
sigmaScaleOK <- c( sigmaScaleOK, "Q_1" = 2, "Q_2" = 2 )
sigmaScaleOK["OMK_10"] <- 3
sigmaScalePG <- sigmaScaleAll[ names( PGnames ), paste0( "Y", year ) ]
names( sigmaScalePG ) <- unname( PGnames )

# sæt kontrol parametre (identiske med estimeringerne i Serie B for 2024)
adapt_delta_All <- read.csv2( file.path( dirPath, "uploaded/adapt_delta.csv" ), row.names = 1 )
adapt_delta <- adapt_delta_All[ names( OKnames ), paste0( "Y", year ) ]
names( adapt_delta ) <- unname( OKnames )
adapt_delta <- c( adapt_delta, "Q_1" = 0.8, "Q_2" = 0.8 )

stepsize_All <- read.csv2( file.path( dirPath, "uploaded/stepsize.csv" ), row.names = 1 )
stepsize <- stepsize_All[ names( OKnames ), paste0( "Y", year ) ]
names( stepsize ) <- unname( OKnames )
stepsize <- c( stepsize, "Q_1" = 0.1, "Q_2" = 0.1 )

max_treedepth_All <- read.csv2( file.path( dirPath, "uploaded/max_treedepth.csv" ), row.names = 1 )
max_treedepth <- max_treedepth_All[ names( OKnames ), paste0( "Y", year ) ]
names( max_treedepth ) <- unname( OKnames )
max_treedepth <- c( max_treedepth, "Q_1" = 10, "Q_2" = 10 )

stanIter_All <- read.csv2( file.path( dirPath, "uploaded/stanIter.csv" ), row.names = 1 )
stanIter <- stanIter_All[ names( OKnames ), paste0( "Y", year ) ]
names( stanIter ) <- unname( OKnames )
stanIter <- rep( 2000, length( stanIter ) )
names( stanIter ) <- unname( OKnames )
stanIter <- c( stanIter, "Q_1" = 1000, "Q_2" = 1000 )

#### rstan estimering ####
options( mc.cores = parallel::detectCores() )

# stan kode af model specifikation
model_spec <- "
data {
  int<lower=0> N;
  int<lower=0> K;
  int<lower=0> M;
  int<lower=0> L;
  int<lower=0> P;
  vector[N] y;
  matrix[N, K] X;
  vector[N] d;
  int e[K];
  matrix[N, L] Z;
  matrix[L, K] S;
  vector<lower=0>[N] a;
  vector[K] betaMu;
  vector<lower=0>[K] betaSigma;
  vector[M] deltaMu;
  vector<lower=0>[M] deltaSigma;
  vector[L] gammaMu;
  vector<lower=0>[L] gammaSigma;
  vector[P] alphaMu;
  vector<lower=0>[P] alphaSigma;
  vector<lower=0>[K] sigmaScale;
}
parameters {
  vector[K] beta;
  vector[M] delta;
  vector[L] gamma;
  vector<lower=0>[P] alpha;
  vector<lower=0>[K] omegaSigmaSq;
}
transformed parameters {
  vector[N] yMu;
  vector<lower=0>[N] yTau;
  vector[N] ls;
  vector[N] ll;
  matrix[N, K] omegaMu;
  {
    vector[N] yExp;
    vector[N] yVar;
    matrix[N, K] omegaExp;
    matrix[N, K] omegaVar;

    for( i in 1:N ){
      for( k in 1:K ){
        omegaMu[i,k] = beta[k];
        if( e[k] > 0 ) {
          omegaMu[i,k] += delta[ e[k] ] * d[i];
        }
        if( L > 0 ) {
          for( l in 1:L ) {
            omegaMu[i,k] += gamma[l] * S[l,k] * Z[i,l];
          }
        }
        if( P > 0 ) {
          omegaMu[i,k] += - alpha[P] * a[i];
        }
      }
    }

    for( i in 1:N ){
      for( k in 1:K ){
        omegaExp[i,k] = exp( omegaMu[i,k] + 0.5 * omegaSigmaSq[k] );
        omegaVar[i,k] = ( exp( omegaSigmaSq[k] ) - 1 ) *
          exp( 2 * omegaMu[i,k] + omegaSigmaSq[k] );
      }
    }

    for( i in 1:N ){
      yExp[i] = 0; 
      yVar[i] = 0; 
      for( k in 1:K ){
        yExp[i] += omegaExp[i,k] * X[i,k];
        yVar[i] += omegaVar[i,k] * X[i,k]^2;
      }
      yTau[i] = log( 1 + yVar[i] / yExp[i]^2 );
      yMu[i] = log( yExp[i] ) - 0.5 * yTau[i];
      ls[i] = log( y[i] ) - log ( yExp[i] );
      ll[i] = lognormal_lpdf( y[i] | yMu[i], sqrt( yTau[i] ) );
    }
  }
}
model {
  beta ~ normal( betaMu, betaSigma );
  omegaSigmaSq ~ cauchy( 0, sigmaScale );

  if( M > 0 ) {
    delta ~ normal( deltaMu, deltaSigma );
  }
  if( L > 0 ) {
    gamma ~ normal( gammaMu, gammaSigma );
  }
  if( P > 0 ) {
    alpha ~ lognormal( alphaMu, alphaSigma );
  }

  y ~ lognormal( yMu, sqrt( yTau ) );
}
"
# indlæs stan model
model <- stan_model( model_code = model_spec )

# lav mappe til resultater
folderResults <- file.path( dirPath, "results/" )
if( !dir.exists( folderResults ) ){
  dir.create( folderResults )
} 

# PG'er brugt som forklarende variabel for de respektive OMK
expVar <- dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Conv", inp, ] ]
expVar <- expVar[ grepl( "^PG_", expVar ) ] # indlæser navne for alle PG'er med 1 i OP-matrix for konventionelle 

# PG'er for hvilke vi skal have dummy variabel for økologiske landbrug
ecoVar <- dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Eco", inp, ] ] # indlæser navne for alle PG'er med 1 i OP-matrix for økologiske
ecoVar <- ecoVar[ grepl( "^PG_", ecoVar ) ]

# Estimerer kun bedrifter med positive totale omkostningskategorier
noZero_variates <- dat[[ inp ]] > 0
noZero_covariates <- rowSums( dat[ , expVar, drop = FALSE ] ) > 0 # fjerner alle observationer med positive omkostninger men nul produktionsgrene
noZero <- noZero_variates & noZero_covariates

# vektor med afhængige variabel (omkostninger)
yVec <- dat[[ inp ]][ noZero ] # laver en afhængig vektor bestpende af inp = den valge omkostningskategori, men kun for bedrifter med positive omk. og mindst en PG_ med Ha>0

# matrix med forklarende variable (PG)
xMat <- dat[ noZero, expVar, drop = FALSE ] 

# ekskluder produktionsgrene som ikke er 'produceret' af nogle som helst observationer der er inkluderet i estimeringen 
expVarInclude <- colSums( xMat ) > 0
xMat <- xMat[ , expVarInclude, drop = FALSE ]
expVar <- expVar[ expVarInclude ]
ecoVar <- ecoVar[ ecoVar %in% expVar ]

# vektor med dummy variable for økologiske landbrug
dVec <- dat$EKOKODE[ noZero ] - 1

# ekskluder koefficienter for økologisk produktion af de produktionsgrene som ikke er 'produceret' af nogle som helst økologiske landbrug der er inkluderet i estimeringen
ecoVarInclude <- 
  colSums( xMat[ dVec == 1, expVar %in% ecoVar, drop = FALSE ] ) > 0
ecoVar <- ecoVar[ ecoVarInclude ]

# laver indextal / vektor der inidkerer hvilke produktionsgrene der har en dummy variable for økologiske landbrug og hvis de har, hvilke koefficienter der tilhører den 
eVec <- sapply( expVar, function(x) which( ecoVar == x )[1] )
eVec[ is.na( eVec ) ] <- 0

# matrix der indikerer (0/1) hvilke koefficienter der afhænger af hvilke Z variable
sMat <- arrayZVar[ "Conv", dimnames( arrayZVar )[[2]], inp, expVar ]
if( is.null( dim( sMat ) ) ){
  sMat <- matrix( sMat, ncol = ncol( xMat ) )
}
sMat <- sMat[ rowSums( sMat ) > 0, , drop = FALSE ]

# matrix med Z variable
if( nrow( sMat ) > 0 ){
  zMat <- dat[ noZero, rownames( sMat ), drop = FALSE ]
  for( i in 1:nrow( sMat ) ){
    zMatLogical <- rowSums( xMat[ , sMat[ i, ] == 1, drop = FALSE ] ) > 0 # & zMat[ , i ] != 0
    if( sum( zMatLogical ) > 0 ){
      zMean <- mean( zMat[ zMatLogical, i ] )
      zMat[ , i ] <- ifelse( zMat[ , i ] != 0, zMat[ , i ] - zMean, 0 )
    }
  }
} else {
  zMat <- matrix( 0, nrow = sum( noZero ), ncol = 0 )
}

# tager højde for 'Maskinstation' i estimeringen
aVec <- dat$OMK_5[ noZero ] / yVec
# truncating extreme values because they can give NaN in STAN
aVec <- pmin( aVec, ifelse( inp == "OMK_8", 400, 100 ) ) # vedrører lønomkostninger
if( arrayExpVar[ "Conv", inp, "MAST" ] ){
  alphaMu <- arrayPriorBeta[ "Conv", inp, "MAST", "meanLog" ]
  alphaSigma <- arrayPriorBeta[ "Conv", inp, "MAST", "sdLog" ]
} else {
  alphaMu <- numeric( 0 )
  alphaSigma <- numeric( 0 )
}

# priors for beta
betaMu <- arrayPriorBeta[ "Conv", inp, expVar, "meanLog" ]
betaSigma <- arrayPriorBeta[ "Conv", inp, expVar, "sdLog" ] 

# priors for delta
deltaMu <- arrayPriorDelta[ inp, ecoVar, "mean" ]
deltaSigma <- arrayPriorDelta[ inp, ecoVar, "sd" ]

# priors for gamma
gammaMu <- arrayPriorGamma[ inp, rownames( sMat ), "mean" ]
gammaSigma <- arrayPriorGamma[ inp, rownames( sMat ), "sd" ]

# beregn priors for omegaSigma
sigmaScale <- sigmaScaleOK[ inp ] * sigmaScalePG[ expVar ]

# specifying data for estimation with STAN
data <- list( N = sum( noZero ), # antal observationer
              K = ncol( xMat ),  # antal PG'er 
              M = max( eVec ), # antal PG'er for økologer
              L = nrow( sMat ), # antallet af z variable
              P = length( alphaMu ), 
              y = yVec, # vektor med OMK_værdier (kr) 
              X = xMat, # matrice med PG-værdier (ha eller stk) 
              d = dVec, # dummy for øko/konv
              e = array( eVec ),
              Z = zMat, # matrice med mean-centreret Z variable
              S = sMat, # matrice med information of PG-Z kombinationer
              a = aVec, # vektor med forhold mellem OMK og maskinomkostninger
              betaMu = array( betaMu ), # mean priors for beta
              betaSigma = array( betaSigma ), # st. dev priors for beta
              deltaMu = array( deltaMu ), # mean priors for delta
              deltaSigma = array( deltaSigma ), # st. dev priors for delta
              gammaMu = array( gammaMu ), # mean priors for gamma
              gammaSigma = array( gammaSigma ), # st. dev priors for gamma
              alphaMu = array( alphaMu ), # mean priors for alpha
              alphaSigma = array( alphaSigma ), # st. dev priors for alpha
              sigmaScale = array( sigmaScale ) ) # priors for skalerings parameter

# initiale værdier for nogle koefficienter
set.seed( 811 )
initValues <- list()
for( chain in 1:8 ){
  initValues[[ chain ]] <- list(
    beta       = array( arrayPriorBeta[ "Conv", inp, expVar, "meanLog" ] + runif( length( expVar ), min = -0.5, max = 0.5 ) ),
    delta      = array( arrayPriorDelta[ inp, ecoVar, "mean" ] + runif( length( ecoVar ), min = -0.5, max = 0.5 ) ),
    gamma      = array( gammaMu + runif( nrow( sMat ), min = -0.5, max = 0.5 ) ),
    alpha      = array( exp( alphaMu + runif( length( alphaMu ), min = -0.5, max = 0.5 ) ) ),
    omegaSigmaSq = array( rep( 0.5, length( sigmaScale ) ) )
  )
}

# STAN estimation
startTime <- Sys.time()
model_est <- sampling( model, data = data, iter = stanIter[ inp ],
                       chains = 8, seed = 811, init = initValues,
                       control = list(
                         adapt_delta = adapt_delta[ inp ],
                         stepsize = stepsize[ inp ],
                         max_treedepth = max_treedepth[ inp ] ) )
attr( model_est, "time" ) <- Sys.time() - startTime

print( check_hmc_diagnostics( model_est ) )

# add information that we will need to process the results
attr( model_est, "noZero" ) <- noZero
attr( model_est, "expVar" ) <- expVar
attr( model_est, "eVec" ) <- eVec
attr( model_est, "zVar" ) <- rownames( sMat )

# gem estimerings resultater
saveRDS( model_est, paste0( folderResults, "model_est_", inp, "_", obs, "_iter", stanIter[ inp ], ".rds" ) )