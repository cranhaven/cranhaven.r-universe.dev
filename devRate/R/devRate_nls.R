#' Compute non-linear regression
#'
#' Determine the nonlinear least-squares estimates of the parameters of a
#'   nonlinear model, on the basis of the \code{nls} function from package
#'   \code{stats}.
#'
#' @param eq The name of the equation. See \code{devRateEqList} for the list
#'   of equations
#' @param temp The temperature (vector).
#' @param devRate The development rate \code{(days)^-1} (vector).
#' @param startValues Starting values for the regression (list).
#' @param dfData A data.frame with the temperature in the first column and the
#'   development rate in the second column (alternative to the use of temp and
#'   devRate).
#' @param algo The abbreviated name of the algorithm used for model fitting (
#'   "GN" for Gauss-Newton algorithm, "LM" for Levenberg-Marquardt algorithm;
#'   "GN" is the default value).
#' @param ... Additional arguments for the \code{nls} function.
#' @return An object of class \code{nls} (except for Stinner et al. 1974 and
#'   Lamb 1992 where the function returns a list of two objects of class
#'   \code{nls}).
#' @details \code{startValues} for equations by Stinner et al. 1974 and Lamb
#'   1992 are composed of two equations: one for the temperatures below the
#'   optimal temperature and another for the temperatures above the optimal
#'   temperature. For these equations, \code{startValues} should be a list
#'   of two lists, where the second element only contain starting estimates not
#'   specified in the first element, e.g., for Stinner et al.:
#'   \code{startValues <- list(list(C = 0.05, k1 = 5, k2 = -0.3), list(Topt = 30))},
#'   and for Lamb 1992:
#'   \code{startValues <- list(list(Rm = 0.05, Tmax = 35, To = 15), list(T1 = 4))}
#' @details The temperature should be provided as a vector in argument \code{temp} and
#'   development rate in another vector in argument \code{devRate}. However, it is
#'   possible to use the function with a data.frame containing the temperature in the
#'   first column and the development rate in the sceond column, using the argument
#'   \code{dfData}
#' @details NULL is returned when an unknown algorithm is entered.
#' @examples
#' ## Example with a linear model (no starting estimates)
#' myT <- 5:15
#' myDev <- -0.05 + rnorm(n = length(myT), mean = myT, sd = 1) * 0.01
#' myNLS <- devRateModel(
#'   eq = campbell_74,
#'   temp = myT,
#'   devRate = myDev)
#' ## Example with a non-linear model (starting estimates)
#' myT <- seq(from = 0, to = 50, by = 10)
#' myDev <- c(0.001, 0.008, 0.02, 0.03, 0.018, 0.004)
#' myNLS <- devRateModel(
#'   eq = stinner_74,
#'   temp = myT,
#'   devRate = myDev,
#'   startValues = list(
#'     list(C = 0.05, k1 = 5, k2 = -0.3),
#'     list(Topt = 30)))
#' ## Example with a data.frame instead of two vectors for temperature and
#' ## development rate
#' myDF <- data.frame(myT, myDev)
#' myNLS <- devRateModel(
#'   eq = campbell_74,
#'   dfData = myDF)
#' @export
devRateModel <- function(
  eq, temp, devRate, startValues, dfData = NULL, algo = "GN", ...){
  if (!is.null(dfData)){
    temp <- dfData[, 1]
    devRate <- dfData[, 2]
  }
  if(algo == "GN"){
    ### handling exception for <stinner_74> and <lamb_92>
    if(eq$id == "eq040" | eq$id == "eq150"){
      tTh <- temp[devRate == max(devRate, na.rm = TRUE)]
      tTh <- unique(tTh[!is.na(tTh)])
      if(length(tTh) > 1){
        meanDevRates <- sapply(seq_along(tTh), function(ti){
          mean(devRate[temp == tTh[ti]], na.rm = TRUE)})
        tTh <- tTh[meanDevRates == max(meanDevRates)][1]
        # if(length(tTh) > 1){tTh <- tTh[1]}
      }
      part1_temp <- temp[temp <= tTh]
      part2_temp <- temp[temp >= tTh]
      part1_devRate <- devRate[temp <= tTh]
      part2_devRate <- devRate[temp >= tTh]

      if(eq$id == "eq040") {
        nls_devRate1 <- stats::nls(
          formula = eq[[1]][[1]],
          data = data.frame(rT = part1_devRate, T = part1_temp),
          start = startValues[[1]],
          ...)
        newEq <- gsub("C", stats::coef(nls_devRate1)[1], eq$eqAlt[2])
        newEq <- gsub("k1", stats::coef(nls_devRate1)[2], newEq)
        newEq <- gsub("k2", stats::coef(nls_devRate1)[3], newEq)
        newEq <- paste0("rT ~ ", newEq)
        nls_devRate2 <- stats::nls(
          formula = newEq,
          data = data.frame(rT = part2_devRate, x = part2_temp),
          start = startValues[[2]])
        nls_devRate <- list(nls_devRate1, nls_devRate2)
      }
      if(eq$id == "eq150") {
        nls_devRate1 <- stats::nls(
          formula = eq[[1]][[1]],
          data = data.frame(rT = part1_devRate, T = part1_temp),
          start = startValues[[1]],
          ...)
        newEq <- gsub("Rm", stats::coef(nls_devRate1)[1], eq$eqAlt[2])
        newEq <- gsub("Tmax", stats::coef(nls_devRate1)[2], newEq)
        newEq <- gsub("To", stats::coef(nls_devRate1)[3], newEq)
        newEq <- paste0("rT ~ ", newEq)
        nls_devRate2 <- stats::nls(
          formula = newEq,
          data = data.frame(rT = part2_devRate, x = part2_temp),
          start = startValues[[2]],
          ...)
        nls_devRate <- list(nls_devRate1, nls_devRate2)
      }
    } else {

      if(eq$id == "eq030"){
        nls_devRate <- stats::nls(
          formula = eq[[1]],
          data = data.frame(rT = devRate, T = temp),
          start = list(aa = 1, bb = 1), ...)
      } else {
        if(eq$id == "eq110"){
          nls_devRate <- stats::nls(
            formula = eq[[1]],
            data = data.frame(rT = devRate, T = temp),
            start = list(a0 = 1, a1 = 1, a2 = 1), ...)
        } else {
          if(eq$id == "eq120"){
            nls_devRate <- stats::nls(
              formula = eq[[1]],
              data = data.frame(rT = devRate, T = temp),
              start = list(a0 = 1, a1 = 1, a2 = 1, a3 = 1), ...)
          } else {
            if(eq$id == "eq130"){
              nls_devRate <- stats::nls(
                formula = eq[[1]],
                data = data.frame(rT = devRate, T = temp),
                start = list(a0 = 1, a1 = 1, a2 = 1, a3 = 1, a4 = 1), ...)
            } else {
              nls_devRate <- stats::nls(
                formula = eq[[1]],
                data = data.frame(rT = devRate, T = temp),
                start = startValues, ...)
            }
          }
        }
      }
    }
  }else{
    if(algo == "LM"){
      ### handling exception for <stinner_74> and <lamb_92>
      if(eq$id == "eq040" | eq$id == "eq150"){
        tTh <- temp[devRate == max(devRate, na.rm = TRUE)]
        tTh <- unique(tTh[!is.na(tTh)])
        if(length(tTh) > 1){
          meanDevRates <- sapply(seq_along(tTh), function(ti){
            mean(devRate[temp == tTh[ti]], na.rm = TRUE)})
          tTh <- tTh[meanDevRates == max(meanDevRates)][1]
          # if(length(tTh) > 1){tTh <- tTh[1]}
        }
        part1_temp <- temp[temp <= tTh]
        part2_temp <- temp[temp >= tTh]
        part1_devRate <- devRate[temp <= tTh]
        part2_devRate <- devRate[temp >= tTh]

        if(eq$id == "eq040") {
          nls_devRate1 <- minpack.lm::nlsLM(
            formula = eq[[1]][[1]],
            data = data.frame(rT = part1_devRate, T = part1_temp),
            start = startValues[[1]],
            ...)
          newEq <- gsub("C", stats::coef(nls_devRate1)[1], eq$eqAlt[2])
          newEq <- gsub("k1", stats::coef(nls_devRate1)[2], newEq)
          newEq <- gsub("k2", stats::coef(nls_devRate1)[3], newEq)
          newEq <- paste0("rT ~ ", newEq)
          nls_devRate2 <- minpack.lm::nlsLM(
            formula = newEq,
            data = data.frame(rT = part2_devRate, x = part2_temp),
            start = startValues[[2]])
          nls_devRate <- list(nls_devRate1, nls_devRate2)
        }
        if(eq$id == "eq150") {
          nls_devRate1 <- minpack.lm::nlsLM(
            formula = eq[[1]][[1]],
            data = data.frame(rT = part1_devRate, T = part1_temp),
            start = startValues[[1]],
            ...)
          newEq <- gsub("Rm", stats::coef(nls_devRate1)[1], eq$eqAlt[2])
          newEq <- gsub("Tmax", stats::coef(nls_devRate1)[2], newEq)
          newEq <- gsub("To", stats::coef(nls_devRate1)[3], newEq)
          newEq <- paste0("rT ~ ", newEq)
          nls_devRate2 <- minpack.lm::nlsLM(
            formula = newEq,
            data = data.frame(rT = part2_devRate, x = part2_temp),
            start = startValues[[2]],
            ...)
          nls_devRate <- list(nls_devRate1, nls_devRate2)
        }
      } else {

        if(eq$id == "eq030"){
          nls_devRate <- minpack.lm::nlsLM(
            formula = eq[[1]],
            data = data.frame(rT = devRate, T = temp),
            start = list(aa = 1, bb = 1), ...)
        } else {
          if(eq$id == "eq110"){
            nls_devRate <- minpack.lm::nlsLM(
              formula = eq[[1]],
              data = data.frame(rT = devRate, T = temp),
              start = list(a0 = 1, a1 = 1, a2 = 1), ...)
          } else {
            if(eq$id == "eq120"){
              nls_devRate <- minpack.lm::nlsLM(
                formula = eq[[1]],
                data = data.frame(rT = devRate, T = temp),
                start = list(a0 = 1, a1 = 1, a2 = 1, a3 = 1), ...)
            } else {
              if(eq$id == "eq130"){
                nls_devRate <- minpack.lm::nlsLM(
                  formula = eq[[1]],
                  data = data.frame(rT = devRate, T = temp),
                  start = list(a0 = 1, a1 = 1, a2 = 1, a3 = 1, a4 = 1), ...)
              } else {
                nls_devRate <- minpack.lm::nlsLM(
                  formula = eq[[1]],
                  data = data.frame(rT = devRate, T = temp),
                  start = startValues, ...)
              }
            }
          }
        }
      }
    }else{
      warning(paste0("error: algorithm ", algo," unknown"))
      return(NULL)
    }
  }
  return(nls_devRate)
}

#' Report model output from the NLS fit
#'
#' Provide a custom output of the NLS fit.
#'
#' @param myNLS An object of class NLS
#' @param doPlots A boolean to get the residual plot (default = FALSE)
#' @return A list of six objects (summary of the NLS fit; confidence intervals
#'   for the model parameters; test of normality; test of independence; AIC, BIC)
#' @examples
#' myT <- 5:15
#' myDev <- -0.05 + rnorm(n = length(myT), mean = myT, sd = 1) * 0.01
#' myNLS <- devRateModel(
#'   eq = campbell_74,
#'   temp = myT,
#'   devRate = myDev,
#'   startValues = list(aa = 0, bb = 0))
#' devRatePrint(myNLS)
#'
#' rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
#'    13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#'    25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
#' mEggs <- devRateModel(
#'   eq = taylor_81,
#'   temp = rawDevEggs[,1],
#'   devRate = rawDevEggs[,2],
#'   startValues = list(Rm = 0.05, Tm = 30, To = 5))
#' devRatePrint(myNLS = mEggs)
#' @export
devRatePrint <- function(myNLS, doPlots = FALSE){
  temp <- get("T", myNLS$m$getEnv())
  devRate <- get("rT", myNLS$m$getEnv())
  cat("##################################################\n### Parameter estimates and overall model fit\n##################################################\n")
  print(summary(myNLS))
  cat("##################################################\n### Confidence intervals for parameters\n##################################################\n")
  print(stats::confint.default(myNLS))
  cat("\n")
  cat("##################################################\n### Residuals distribution and independence\n##################################################\n")
  cat("### Normality of the residual distribution\n")
  print(stats::shapiro.test(stats::residuals(myNLS)))
  if(doPlots == TRUE){
    opar <- graphics::par(mfrow = c(1,2))
    graphics::plot(temp,
         devRate,
         main = paste0(
           "Obs. versus fitted (cor: ",
           round(stats::cor(devRate, stats::predict(myNLS)), digits = 4),
           ")")
    ) # cor gives some estimation of the goodness of fit
    cat("### See plots for observed versus fitted values, and Normal Q-Q Plot\n\n")
    graphics::points(temp,
           stats::predict(myNLS),
           lty = 2,
           lwd = 2,
           col = 2
    )
    stats::qqnorm(stats::residuals(myNLS))
    stats::qqline(stats::residuals(myNLS))
    graphics::par(opar)
  }
  cat("### Regression of the residuals against a lagged version of themselves\n")
  cat("### and testing if the slope of the resulting relationship is significantly\n")
  cat("### different from 0:\n")
  N <- length(stats::residuals(myNLS))
  indTest <- stats::lm(stats::residuals(myNLS)[-N] ~ stats::residuals(myNLS)[-1])
  print(summary(indTest))
  cat("##################################################\n### Comparing models\n##################################################\n")
  cat("### Using AIC and BIC\n")
  cat(paste0("Akaike Information Criterion (AIC): ", stats::AIC(myNLS), "\n"))
  cat(paste0("Bayesian Information Criterion (BIC): ", stats::BIC(myNLS), "\n"))
  # returns list of tests
  objReturn <-list(sumNLS = summary(myNLS),
                   confint = stats::confint.default(myNLS),
                   normRD = stats::shapiro.test(stats::residuals(myNLS)),
                   indTest = summary(indTest),
                   AIC = stats::AIC(myNLS),
                   BIC = stats::BIC(myNLS))
  return(objReturn)
}



