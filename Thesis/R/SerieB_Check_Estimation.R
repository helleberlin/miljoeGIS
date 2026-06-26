# Databricks notebook source
# indlæs pakker
install.packages("rstan")
install.packages("miscTools")
install.packages("gridExtra")
install.packages("cowplot")
install.packages( "patchwork" )
library("rstan")
library("miscTools")
library("gridExtra")
library("ggplot2")
library("cowplot")
library( "patchwork" )

# COMMAND ----------

# vælg stien til det klargjorte datasæt
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

folderDownload <- file.path(dirPath, "download")

file.rename(
  file.path(folderDownload, "coefficients.rds"),
  file.path(folderDownload, "coefficients_conv_crops.rds")
)

# COMMAND ----------

# vælg model specifikation og antal iterationer
obs  <- "conv"
iter <- 2000

# brug funktion til at indlæse estimerings resultater
loadEstResult <- function(fileName) {
  nTries <- 0
  repeat {
    model_est <- try(readRDS(fileName))
    nTries <- nTries + 1
    if (inherits(model_est, "try-error")) {
      if (nTries < 10) {
        Sys.sleep(60)
        cat("Trying again...\n")
      } else {
        cat("Giving up...\n")
        model_est <- NULL
        break
      }
    } else {
      break
    }
  }
  return(model_est)
}

# vælg stien til det klargjorte datasæt
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# indlæs datasæt, priors og OP kombinationer
dat            <- readRDS(file.path(dirPath, "dat_prepared/dat_prepared_farms.rds"))
arrayPriorBeta <- readRDS(file.path(dirPath, "priors/arrayPriorBeta.rds"))
arrayExpVar    <- readRDS(file.path(dirPath, "dat_prepared/arrayExpVar.rds"))

# mappe til indlæsning af resultater og gemning af estChecks
folderResults  <- file.path(dirPath, "results")
folderDownload <- file.path(dirPath, "download")

# navne på alle estimerings filer for denne obs med specificeret antal iterationer
resultFiles <- list.files(path    = folderResults,
                          pattern = paste0("^model_est_.*_", obs, "_iter", iter, "\\.rds$"))
cat("Fandt", length(resultFiles), "modeller med iter =", iter, ":\n")
print(resultFiles)

# navn på estChecks fil 
estChecksFile    <- file.path(folderDownload, paste0( "estChecks_", obs, ".rds" ) )
estChecksFileOld <- estChecksFile

# lav en dataframe for at tjekke resultaternes pålidelighed
depVarAll <- sub(paste0("_", obs, "_iter", iter, "\\.rds$"), "",
                 sub("^model_est_", "", resultFiles))

estChecks <- data.frame(
  depVar       = depVarAll,
  timeSaved    = NA, duration     = NA,
  nObsEst      = NA, divPercent   = NA,
  satTreeDepth = NA, nLowBfmi     = NA,
  maxRhatCoef  = NA, maxRhatOmu   = NA,
  RhatLp       = NA, maxLs        = NA,
  stringsAsFactors = FALSE)
row.names(estChecks) <- estChecks$depVar

# indlæs tidligere estChecks hvis den findes
if (file.exists(estChecksFileOld)) {
  estChecksOld <- readRDS(estChecksFileOld)
} else {
  estChecksOld <- NULL
}

# loop over alle modeller
for (fileName in resultFiles) {

  depVar    <- sub(paste0("_", obs, "_iter", iter, "\\.rds$"), "",
                   sub("^model_est_", "", fileName))
  timeSaved <- format(file.info(file.path(folderResults, fileName))$mtime,
                      "%Y-%m-%d %H:%M:%S")
  figFile   <- file.path(folderResults, "figures",
                         paste0("estChecks_", depVar, "_", obs, "_iter", iter, ".png"))

  cat(depVar, ": ", sep = "")

  # tjek om estChecks ER opdateret OG figur allerede findes
  if (!is.null(estChecksOld) && depVar %in% estChecksOld$depVar) {
    if (!is.na(estChecksOld[depVar, "timeSaved"]) &&
        estChecksOld[depVar, "timeSaved"] == timeSaved &&
        file.exists(figFile)) {
      cat("bruger tidligere information\n")
      estChecks[depVar, ] <- estChecksOld[depVar, ]
      next
    }
  }

  cat("importerer resultat fil...")
  model_est <- loadEstResult(file.path(folderResults, fileName))
  if (is.null(model_est)) {
    cat("failed\n")
    next
  } else {
    cat("done\n")
  }

  # model summary og attributter
  model_sum <- summary(model_est)$summary
  noZero    <- attr(model_est, "noZero")
  expVar    <- attr(model_est, "expVar")

  # udfyld estChecks
  estChecks[depVar, "timeSaved"]    <- timeSaved
  estChecks[depVar, "duration"]     <- as.double(attr(model_est, "time"), units = "mins")
  estChecks[depVar, "nObsEst"]      <- sum(noZero)
  estChecks[depVar, "divPercent"]   <- 100 * mean(get_divergent_iterations(model_est))
  estChecks[depVar, "satTreeDepth"] <- 100 * mean(get_max_treedepth_iterations(model_est))
  estChecks[depVar, "nLowBfmi"]     <- length(get_low_bfmi_chains(model_est))
  estChecks[depVar, "maxRhatCoef"]  <- max(model_sum[grep("^[abdgo]", rownames(model_sum)), "Rhat"])
  estChecks[depVar, "maxRhatOmu"]   <- max(model_sum[grep("^omegaMu", rownames(model_sum)), "Rhat"], na.rm = TRUE)
  estChecks[depVar, "RhatLp"]       <- model_sum["lp__", "Rhat"]
  estChecks[depVar, "maxLs"]        <- max(abs(model_sum[grep("^ls", rownames(model_sum)), "mean"]))

  #### outlier detection ####
  llIQR        <- quantile(model_sum[grep("^ll", rownames(model_sum)), "mean"], 0.75) -
                  quantile(model_sum[grep("^ll", rownames(model_sum)), "mean"], 0.25)
  llfence      <- unname(min(
    quantile(model_sum[grep("^ll", rownames(model_sum)), "mean"], 0.25) - 1.5 * llIQR,
    sort(model_sum[grep("^ll", rownames(model_sum)), "mean"])[8]))
  llOut        <- which(model_sum[grep("^ll", rownames(model_sum)), "mean"] < llfence)
  lsIQR        <- quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.75) -
                  quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.25)
  lsfenceLower <- unname(min(
    quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.25) - 1.5 * lsIQR,
    sort(model_sum[grep("^ls", rownames(model_sum)), "mean"])[8]))
  lsfenceUpper <- unname(max(
    quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.75) + 1.5 * lsIQR,
    sort(model_sum[grep("^ls", rownames(model_sum)), "mean"], decreasing = TRUE)[8]))
  lsOut        <- which(
    model_sum[grep("^ls", rownames(model_sum)), "mean"] < lsfenceLower |
    model_sum[grep("^ls", rownames(model_sum)), "mean"] > lsfenceUpper)
  obsOut       <- sort(unique(c(llOut, lsOut)))

  #### outlier tabel ####
  result_df <- data.frame()
  if (length(obsOut) > 0) {
    potOut     <- sort(dat$cvr[which(noZero)[obsOut]])
    pgAll      <- grep("^PG", dimnames(arrayExpVar)[[3]], value = TRUE)
    coef_prior <- exp(arrayPriorBeta["Conv", depVar, pgAll, "meanLog"])
    coef_est   <- rep(NA, length(pgAll))
    coef_est[pgAll %in% expVar] <- exp(model_sum[grep("^beta", rownames(model_sum)), "mean"])
    for (v in potOut) {
      obsNo  <- which(dat$cvr[which(noZero)] == v)
      pg_obs <- dat[dat$cvr == v, pgAll]
      tab    <- rbind(pg_obs     = pg_obs,
                      coef_prior = coef_prior,
                      OMK        = coef_prior * pg_obs,
                      coef_est   = coef_est,
                      OMK_est    = coef_est * pg_obs)
      result_df <- rbind(result_df, data.frame(
        cvr            = v,
        observed_OMK   = prettyNum(round(dat[dat$cvr == v, depVar]),              big.mark = ","),
        prior_beta_OMK = prettyNum(round(sum(tab["OMK",     !is.na(coef_prior) & pg_obs > 0, drop = FALSE])), big.mark = ","),
        est_beta_OMK   = prettyNum(round(sum(tab["OMK_est", !is.na(coef_est)   & pg_obs > 0, drop = FALSE])), big.mark = ","),
        est_yMedian    = prettyNum(round(exp(
          model_sum[grep("^yMu",  rownames(model_sum)), "mean"][obsNo])), big.mark = ","),
        est_yMean      = prettyNum(round(exp(
          model_sum[grep("^yMu",  rownames(model_sum)), "mean"][obsNo] +
          0.5 * model_sum[grep("^yTau", rownames(model_sum)), "mean"][obsNo])), big.mark = ",")))
    }
  }

  #### gem figurer ####
  fig_height <- ifelse(nrow(result_df) > 0, 12, 6)

  png(figFile, width = 2400, height = fig_height * 150, res = 150)

  if (nrow(result_df) > 0) {
    layout(matrix(c(1, 2, 3, 3, 4, 4), nrow = 2, byrow = TRUE))
  } else {
    par(mfrow = c(1, 3))
  }

  hist(model_sum[grep("^ls", rownames(model_sum)), "mean"], 30,
       main = depVar, xlab = "log( observed input / predicted input )")

  hist(model_sum[grep("^ll", rownames(model_sum)), "mean"], 30,
       main = depVar, xlab = "log-likelihood values")

  obsFigure <- rep(TRUE, sum(noZero))
  repeat {
    lsValues <- model_sum[grep("^ls", rownames(model_sum))[obsFigure], "mean"]
    llValues <- model_sum[grep("^ll", rownames(model_sum))[obsFigure], "mean"]
    lsRange  <- range(lsValues) + c(-0.05, 0.05) * diff(range(lsValues))
    llRange  <- range(llValues) + c(-0.05, 0.05) * diff(range(llValues))
    plot(lsValues, llValues, xlim = lsRange, ylim = llRange,
         main = depVar,
         xlab = "log( observed input / predicted input )",
         ylab = "log-likelihood values")
    obsFigureOut <- obsFigure & seq_len(sum(noZero)) %in% obsOut
    if (any(obsFigureOut)) {
      lsValuesOut <- model_sum[grep("^ls", rownames(model_sum))[obsFigureOut], "mean"]
      llValuesOut <- model_sum[grep("^ll", rownames(model_sum))[obsFigureOut], "mean"]
      virknrOut   <- dat$cvr[which(noZero)[obsFigureOut]]
      points(lsValuesOut, llValuesOut, col = "red")
      text(lsValuesOut, llValuesOut, virknrOut, pos = 3, cex = 0.65)
    }
    obsFigureExtreme <- rep(FALSE, sum(obsFigure))
    lsRange25 <- c(crossprod(c(0.75, 0.25), lsRange))
    if (sum(lsValues <= lsRange25) <= 3) obsFigureExtreme <- obsFigureExtreme | lsValues <= lsRange25
    lsRange75 <- c(crossprod(c(0.25, 0.75), lsRange))
    if (sum(lsValues >= lsRange75) <= 3) obsFigureExtreme <- obsFigureExtreme | lsValues >= lsRange75
    llRange25 <- c(crossprod(c(0.75, 0.25), llRange))
    if (sum(llValues <= llRange25) <= 3) obsFigureExtreme <- obsFigureExtreme | llValues <= llRange25
    if (any(obsFigureExtreme)) {
      obsFigure[obsFigure] <- !obsFigureExtreme
    } else {
      break
    }
  }

  if (nrow(result_df) > 0) {
    grid.arrange(tableGrob(result_df, rows = NULL), ncol = 1)
  }

  dev.off()
  cat(depVar, ": figur gemt -", figFile, "\n")

}

# vis og gem samlet estChecks med iter information
display(estChecks)
saveRDS(estChecks, estChecksFile)
cat("\nestChecks gemt i", estChecksFile, "\n")

# COMMAND ----------

# vælg model specifikation og antal iterationer
obs  <- "conv"
iter <- 2000

# vælg stien til det klargjorte datasæt
dirPath <- "/Volumes/ledelseoekonomi_integration_prod/ifro/output"

# stivej til gemte resultater
folderResults  <- file.path(dirPath, "results")

# brug funktion til at indlæse estimerings resultater
loadEstResult <- function(fileName) {
  nTries <- 0
  repeat {
    model_est <- try(readRDS(fileName))
    nTries <- nTries + 1
    if (inherits(model_est, "try-error")) {
      if (nTries < 10) {
        Sys.sleep(60)
        cat("Trying again...\n")
      } else {
        cat("Giving up...\n")
        model_est <- NULL
        break
      }
    } else {
      break
    }
  }
  return(model_est)
}

# vis figurer direkte i notebook for én model
fileName  <- "model_est_OMK_8_conv_iter2000.rds"
depVar    <- sub(paste0("_", obs, "_iter.*\\.rds$"), "", sub("^model_est_", "", fileName))
model_est <- loadEstResult( file.path(folderResults, fileName) )
model_sum <- summary(model_est)$summary
noZero    <- attr(model_est, "noZero")
expVar    <- attr(model_est, "expVar")

# histogram ls og ll
ls_data <- data.frame( value = model_sum[grep("^ls", rownames(model_sum)), "mean"] )
ll_data <- data.frame( value = model_sum[grep("^ll", rownames(model_sum)), "mean"] )

# Plot 1: log observed/predicted
p1 <- ggplot(ls_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "grey", color = "white") +
  labs( title = "", x = "log( observed input / predicted input )", y = "Frequency" ) +
  theme_bw()

# Plot 2: log-likelihood
p2 <- ggplot(ll_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "grey", color = "white") +
  labs( title = "", x = "Log-likelihood values", y = "Frequency" ) +
  theme_bw()

# Combine side by side
histCostModel <- p1 + p2
ggsave( filename = paste0( "/Volumes/ledelseoekonomi_integration_prod/ifro/output/download/hist_", depVar, "_", obs, ".png" ), plot = histCostModel, device = "png" )

# COMMAND ----------

histCostModel

# COMMAND ----------

# scatter plot med outliers
par(mfrow = c(1, 1))
lsIQR        <- quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.75) -
                quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.25)
lsfenceLower <- unname(min(
  quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.25) - 1.5 * lsIQR,
  sort(model_sum[grep("^ls", rownames(model_sum)), "mean"])[8]))
lsfenceUpper <- unname(max(
  quantile(model_sum[grep("^ls", rownames(model_sum)), "mean"], 0.75) + 1.5 * lsIQR,
  sort(model_sum[grep("^ls", rownames(model_sum)), "mean"], decreasing = TRUE)[8]))
lsOut <- which(
  model_sum[grep("^ls", rownames(model_sum)), "mean"] < lsfenceLower |
  model_sum[grep("^ls", rownames(model_sum)), "mean"] > lsfenceUpper)

llIQR   <- quantile(model_sum[grep("^ll", rownames(model_sum)), "mean"], 0.75) -
            quantile(model_sum[grep("^ll", rownames(model_sum)), "mean"], 0.25)
llfence <- unname(min(
  quantile(model_sum[grep("^ll", rownames(model_sum)), "mean"], 0.25) - 1.5 * llIQR,
  sort(model_sum[grep("^ll", rownames(model_sum)), "mean"])[8]))
llOut   <- which(model_sum[grep("^ll", rownames(model_sum)), "mean"] < llfence)
obsOut  <- sort(unique(c(llOut, lsOut)))

obsFigure <- rep(TRUE, sum(noZero))
repeat {
  lsValues <- model_sum[grep("^ls", rownames(model_sum))[obsFigure], "mean"]
  llValues <- model_sum[grep("^ll", rownames(model_sum))[obsFigure], "mean"]
  lsRange  <- range(lsValues) + c(-0.05, 0.05) * diff(range(lsValues))
  llRange  <- range(llValues) + c(-0.05, 0.05) * diff(range(llValues))
  plot(lsValues, llValues, xlim = lsRange, ylim = llRange,
       main = depVar,
       xlab = "log( observed input / predicted input )",
       ylab = "log-likelihood values")
  obsFigureOut <- obsFigure & seq_len(sum(noZero)) %in% obsOut
  if (any(obsFigureOut)) {
    points(model_sum[grep("^ls", rownames(model_sum))[obsFigureOut], "mean"],
           model_sum[grep("^ll", rownames(model_sum))[obsFigureOut], "mean"],
           col = "red")
    # text(model_sum[grep("^ls", rownames(model_sum))[obsFigureOut], "mean"],
    #     model_sum[grep("^ll", rownames(model_sum))[obsFigureOut], "mean"],
    #     dat$cvr[which(noZero)[obsFigureOut]], pos = 3, cex = 0.65)
  }
  obsFigureExtreme <- rep(FALSE, sum(obsFigure))
  lsRange25 <- c(crossprod(c(0.75, 0.25), lsRange))
  if (sum(lsValues <= lsRange25) <= 3) obsFigureExtreme <- obsFigureExtreme | lsValues <= lsRange25
  lsRange75 <- c(crossprod(c(0.25, 0.75), lsRange))
  if (sum(lsValues >= lsRange75) <= 3) obsFigureExtreme <- obsFigureExtreme | lsValues >= lsRange75
  llRange25 <- c(crossprod(c(0.75, 0.25), llRange))
  if (sum(llValues <= llRange25) <= 3) obsFigureExtreme <- obsFigureExtreme | llValues <= llRange25
  if (any(obsFigureExtreme)) {
    obsFigure[obsFigure] <- !obsFigureExtreme
  } else {
    break
  }
}

# COMMAND ----------

# tabel med information om potentielle outliers 
if( length( obsOut ) > 0 ) {
  # potentiel outlier data
  potOut <- sort( dat$cvr[ which( noZero )[ obsOut ] ] )
  
  # vælger outlier(s) der skal tjekkes
  result_df <- data.frame()
  # forventede input-output koefficienter
  pgAll <- dimnames( arrayExpVar )[[3]]
  pgAll <- grep( "^PG", pgAll, value = TRUE )
  coef_prior <- exp( arrayPriorBeta[ "Conv", depVar, pgAll, "meanLog" ] )
  coef_est <- rep( NA, length( pgAll ) )
  coef_est[ pgAll %in% expVar ] <-
    exp( model_sum[ grep( "^beta", rownames( model_sum ) ), "mean" ] )
  for( v in potOut ) {
    obsNo <- which( dat$cvr[ which( noZero ) ] == v )
    # observerede omfang af produktionsgrene
    pg_obs <- dat[ dat$cvr == v, pgAll ]
    tab <- rbind( pg_obs = pg_obs, coef_prior = coef_prior,
                  OMK = coef_prior * pg_obs,
                  coef_est = coef_est,
                  OMK_est = coef_est * pg_obs )
    # dataframe med 'cvr', observerede- and forudsagte OMK værdier
    result_df <- rbind( result_df, data.frame(
      cvr = v,
      observed_OMK = prettyNum( round( dat[ dat$cvr == v, depVar ] ),
                               big.mark = "," ),
      prior_beta_OMK = prettyNum( round( sum(
        tab[ "OMK" , !is.na( coef_prior ) & pg_obs > 0, drop = FALSE ] ) ),
        big.mark = "," ),
      est_beta_OMK = prettyNum( round( sum(
        tab[ "OMK_est" , !is.na( coef_est ) & pg_obs > 0, drop = FALSE ] ) ),
        big.mark = "," ),
      est_yMedian = prettyNum( round( exp(
        model_sum[ grep( "^yMu", rownames( model_sum ) ), "mean" ][ obsNo ] ) ),
        big.mark = "," ),
      est_yMean = prettyNum( round( exp(
        model_sum[ grep( "^yMu", rownames( model_sum ) ), "mean" ][ obsNo ] +
          0.5 * model_sum[ grep( "^yTau", rownames( model_sum ) ), "mean" ][ obsNo ] ) ),
        big.mark = "," ) ) )
  }
}

# printer dataframen
display( result_df )
grid.arrange( tableGrob( result_df, rows = NULL ), ncol = 1 )