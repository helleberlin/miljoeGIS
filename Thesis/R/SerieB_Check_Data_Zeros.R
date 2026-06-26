# Databricks notebook source
# definér stivej til det klargjorte data
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output/"

# indlæs det forberedte datasæt
dat <- readRDS( file.path( dirPath, "dat_prepared/dat_prepared_farms.rds" ) )

# indlæs array der indikere de forklarende variable
arrayExpVar <- readRDS( file.path( dirPath, "dat_prepared/arrayExpVar.rds" ) )
arrayUVar <- readRDS( file.path( dirPath, "dat_prepared/arrayUVar.rds" ) )

# omkosting(er) der bruges i estimeringen
allInp <- dimnames( arrayExpVar )[[2]]

# produktionsgrene der bruges i estimeringen
allOut <- dimnames( arrayExpVar )[[3]]
allOut <- allOut[ allOut != "MAST" ]

# udbytte kategorier hørende til omkostninger og produktionsgrene der i estimeres
allYield <- dimnames( arrayUVar )[[2]]

# alle positive PGs for hver bedrift
dat$pg_positive <- NA
for( i in 1:nrow( dat ) ){
  dat$pg_positive[i] <- paste( allOut[ dat[ i, allOut ] > 0 ], collapse = ", " )
}

# lav dataframe for potentielle OMK der ikke kan fordeles for den enkelte observation
okNoDist <- data.frame( Variable = character(), cvr = integer(), EKOKODE = integer(), value = double(), pg_positive = character() )

for( inp in allInp ){
  cat( "\n", inp, ": ", sep = "" )

  # produktionsgrene brugt some forklarende variable for totale omkostninger for hhv. konv- og øokoliske
  expVarConv <- dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Conv", inp, ] ]
  expVarConv <- expVarConv[ !expVarConv %in% c("MAST") ]
  expVarOrg <- dimnames( arrayExpVar )[[3]][ arrayExpVar[ "Eco", inp, ] ]
  expVarOrg <- expVarOrg[ !expVarOrg %in% c("MAST") ]

  # zero total inputs / costs
  zero_inp <- dat[[ inp ]] == 0
  zero_expVar <- NA
  zero_expVar[ dat$EKOKODE == 1 ] <-
    rowSums( dat[ dat$EKOKODE == 1, expVarConv, drop = FALSE ] ) == 0
  zero_expVar[ dat$EKOKODE == 2 ] <-
    rowSums( dat[ dat$EKOKODE == 2, expVarOrg, drop = FALSE ] ) == 0

  # print antal af tilfælde hvor omkostninger er positive men alle produktions er nul
  cat( sum( !zero_inp & zero_expVar ), "observationer med positive omkostninger men alle produktionsgrene nul.\n" )
  
  if( sum( !zero_inp & zero_expVar ) > 0 ){
    okNoDistNew <- cbind( inp, dat[ !zero_inp & zero_expVar, c( "cvr", "EKOKODE", inp, "pg_positive" ) ] )
    names( okNoDistNew ) <- names( okNoDist )
    okNoDist <- rbind( okNoDist, okNoDistNew )
  }
}

# lav dataframe for potentielle OMK der ikke kan fordeles for den enkelte observation
uNoDist <- data.frame( Variable = character(), cvr = integer(), EKOKODE = integer(), value = double(), pg_positive = character() )

for( u in allYield ){
  cat( "\n", u, ": ", sep = "" )

  # produktionsgrene brugt some forklarende variable for udbytter for hhv. konv- og øokoliske
  uVarConv <- dimnames( arrayUVar )[[3]][ arrayUVar[ "Conv", u, ] ]
  uVarOrg <- dimnames( arrayUVar )[[3]][ arrayUVar[ "Eco", u, ] ]

  # nul totale udbytter
  zero_u <- dat[[ u ]] == 0
  zero_uVar <- NA
  zero_uVar[ dat$EKOKODE == 1 ] <-
    rowSums( dat[ dat$EKOKODE == 1, uVarConv, drop = FALSE ] ) == 0
  zero_uVar[ dat$EKOKODE == 2 ] <-
    rowSums( dat[ dat$EKOKODE == 2, uVarOrg, drop = FALSE ] ) == 0

  # print antal af tilfælde hvor omkostninger er positive men alle produktions er nul
  cat( sum( !zero_u & zero_uVar ), "observationer med positive udbytter men produktionsgren nul.\n" )
  
  if( sum( !zero_u & zero_uVar ) > 0 ){
    uNoDistNew <- cbind( u, dat[ !zero_u & zero_uVar, c( "cvr", "EKOKODE", u, "pg_positive" ) ] )
    names( uNoDistNew ) <- names( uNoDist )
    uNoDist <- rbind( uNoDist, uNoDistNew )
  }
}

# gem datasæt med bedrifter der har positive omkostninger men alle tilhørende produktionsgrene nul
display( rbind( okNoDist, uNoDist ) )
write.csv2( okNoDist, file.path( dirPath, "check_data/OMK_kan_ikke_fordeles.csv" ), row.names = FALSE )
write.csv2( uNoDist, file.path( dirPath, "check_data/U_kan_ikke_fordeles.csv" ), row.names = FALSE )

# COMMAND ----------

# alle positive udbytter for hver bedrift
dat$U_positive <- NA
for( i in 1:nrow( dat ) ){
 dat$U_positive[i] <- paste( allYield[ dat[ i, allYield ] > 0 ], collapse = ", " )
}

# lav dataframe for potentielle OMK der ikke kan fordeles for den enkelte observation
pgNoDist <- data.frame( Variable = character(), cvr = integer(), EKOKODE = integer(), value = double(), U_positive = character() )


for( pg in allOut ){
  cat( "\n", pg, ": ", sep = "" )

  # produktionsgrene brugt some forklarende variable for udbytter for hhv. konv- og øokoliske
  pgVarConv <- dimnames( arrayUVar )[[2]][ arrayUVar[ "Conv", , pg ] ]
  pgVarOrg <- dimnames( arrayUVar )[[2]][ arrayUVar[ "Eco", , pg ] ]

  # nul totale udbytter
  zero_pg <- dat[[ pg ]] == 0
  zero_pgVar <- NA
  zero_pgVar[ dat$EKOKODE == 1 ] <-
    rowSums( dat[ dat$EKOKODE == 1, pgVarConv, drop = FALSE ] ) == 0
  zero_pgVar[ dat$EKOKODE == 2 ] <-
    rowSums( dat[ dat$EKOKODE == 2, pgVarOrg, drop = FALSE ] ) == 0

  # print antal af tilfælde hvor omkostninger er positive men alle produktions er nul
  cat( sum( !zero_pg & zero_pgVar ), "observationer med positive produktionsgren men udbytte nul.\n" )
  
  if( sum( !zero_pg & zero_pgVar ) > 0 ){
    pgNoDistNew <- cbind( pg, dat[ !zero_pg & zero_pgVar, c( "cvr", "EKOKODE", pg, "U_positive" ) ] )
    names( pgNoDistNew ) <- names( pgNoDist )
    pgNoDist <- rbind( pgNoDist, pgNoDistNew )
  }
}

# gem datasæt med bedrifter der har positive omkostninger men alle tilhørende produktionsgrene nul
display( pgNoDist )
write.csv2( pgNoDist, file.path( dirPath, "check_data/PG_kan_ikke_fordeles.csv" ), row.names = FALSE )