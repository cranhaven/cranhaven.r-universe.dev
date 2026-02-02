#' Internal function to format the results of measurement invariance tests
#'
#' @param FIT a list of fitted lavaan models.
#' @param fit.measures a vector of names of fit indices, passed to \code{lavaan:fitMeasures}.
#' @param method a vector of method names for calculating robust test statistic(s). See \code{lavaan:lavTestLRT}.
#' @param quiet If \code{quiet=FALSE} (default), a summary is printed out containing an overview of the different models that are fitted, together with some model comparison tests and fit measures. The results of equivalence testing will also be printed if equivalence testing is used. If \code{quiet=TRUE}, no summary is printed but results will be stored in the object.
#' @author The maintainer, Ge Jiang, adapted the original source code of printInvarianceResult() in the \pkg{lavaan} and \pkg{semTools} packages written by Yves Rosseel, Sunthud Pornprasertmanit, and Terrence D. Jorgensen (permission obtained).
#' @noRd


printInvarianceResult <- function(FIT, fit.measures, method, quiet = FALSE) {

  if ('fit.strict.residuals'%in%names(FIT)){
    keep1 <- anova(FIT$fit.strict.residuals, FIT$fit.scalar)
  }

  # compare models
  NAMES <- names(FIT); names(FIT) <- NULL
  lavaanLavTestLRT <- function(...) { lavaan::lavTestLRT(...) }
  TABLE <- suppressWarnings(do.call(lavaanLavTestLRT, c(FIT, list(model.names = NAMES, method = method))))

  #not meaningful comparision
  ind <- match(c('fit.configural.g1', 'fit.configural.g2', 'fit.combine.groups'), rownames(TABLE))
  TABLE[ind[!is.na(ind)], c('Chisq diff', 'Df diff', 'Pr(>Chisq)')] <- NA
  if ('fit.strict.residuals'%in%rownames(TABLE)){
    TABLE[match('fit.strict.residuals', rownames(TABLE)), ] <- keep1[2,]
  }

  if(length(fit.measures) == 1L && fit.measures == "default") {
    if(length(lavaan::lavInspect(FIT[[1]], "test")) > 1L) {
      fit.measures <- c("cfi.scaled", "rmsea.scaled")
    } else {
      fit.measures <- c("cfi", "rmsea")
    }
  }

  # add some fit measures
  if(length(fit.measures)) {

    FM <- lapply(FIT, lavaan::fitMeasures, fit.measures)
    FM.table1 <- sapply(fit.measures, function(x) sapply(FM, "[[", x))
    if(length(FM) == 1L) {
      FM.table1 <- rbind( rep(as.numeric(NA), length(fit.measures)),
                          FM.table1 )
    }
    if(length(FM) > 1L) {
      FM.table2 <- rbind(as.numeric(NA),
                         abs(apply(FM.table1, 2, diff)))
      colnames(FM.table2) <- paste(colnames(FM.table2), ".delta", sep="")
      FM.TABLE <- as.data.frame(cbind(FM.table1, FM.table2))
    } else {
      FM.TABLE <- as.data.frame(FM.table1)
    }
    rownames(FM.TABLE) <- rownames(TABLE)
    class(FM.TABLE) <- c("lavaan.data.frame", "data.frame")
  }

  #not meaningful comparision
  ind <- match(c('fit.configural.g1', 'fit.configural.g2', 'fit.combine.groups'), rownames(FM.TABLE))
  FM.TABLE[ind[!is.na(ind)], c('cfi.delta', 'rmsea.delta')] <- NA
  if ('fit.strict.residuals'%in%rownames(FM.TABLE)){
    ind1 <- match('fit.scalar', rownames(FM.TABLE))
    ind2 <- match('fit.strict.residuals', rownames(FM.TABLE))
    FM.TABLE[ind2, 3:4] <- abs(FM.TABLE[ind1, 1:2] - FM.TABLE[ind2, 1:2])
  }

  if(!quiet){
    message("\n")
    message("Measurement invariance models \n\n")
    message(paste(paste("Model", seq_along(FIT), ":", NAMES), collapse = "\n"))
    message("\n\n")

    print(TABLE)
  }

  if(length(fit.measures)) {
    if(!quiet){
      message("\n", "Fit measures \n\n")
      pFM <- FM.TABLE
      pFM[] <- sprintf("%8.3f",unlist(FM.TABLE))
      pFM[is.na(FM.TABLE)] <- ''
      print(pFM)
      message("\n")
    }
    return(data.frame(TABLE, FM.TABLE))
  } else {
    return(TABLE)
  }
}

