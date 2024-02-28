#' @title Sample the Multivariate t Distribution
#'
#' @description Take a selected number of samples from the multivariate t distribution (mvt).
#'
#' @details Using the provided data the function creates a fit of the
#'  Šesták–Berggren kinetic model and then draws a selected number of
#'   samples from the mvt of the model parameters.
#'
#' @param data Dataframe containing accelerated stability data (required).
#' @param y Name of decreasing variable (e.g. concentration) contained within data
#'  (required).
#' @param .time Time variable contained within data (required).
#' @param K Kelvin variable (numeric or column name) (optional).
#' @param C Celsius variable (numeric or column name) (optional).
#' @param validation Validation dummy variable (column name) (optional).
#' @param draw Number of samples to draw from mvt (required).
#' @param parms Starting values for the parameters as a list - k1, k2, k3, and c0 (optional).
#' @param reparameterisation Use alternative parameterisation of the one-step
#'  model which aims to reduce correlation between k1 and k2.
#' @param zero_order Set kinetic order, k3, to zero (straight lines).
#'
#' @return A matrix containing parameter draws from the mvt distribution.
#'
#' @examples #load antigenicity data.
#' data(antigenicity)
#'
#' #Basic use of the step1_sample_mvt function with C column defined and 1000 draws.
#' sample1 <- step1_sample_mvt(data = antigenicity, y = "conc", .time = "time",
#'  C = "Celsius", draw = 1000)
#'
#' #Basic use of the step1_sample_mvt function with K column defined and 50000 draws
#' sample2 <- step1_sample_mvt(data = antigenicity, y = "conc", .time = "time",
#'  K = "K", draw = 50000)
#'
#' #reparameterisation is TRUE and 10000 draws.
#' sample3 <- step1_sample_mvt(data = antigenicity, y = "conc", .time = "time",
#' C = "Celsius", reparameterisation = TRUE, draw = 10000)
#'
#' @importFrom stats vcov coef runif confint rnorm quantile qt complete.cases
#' @importFrom minpack.lm nls.lm
#' @importFrom mvtnorm rmvt
#'
#' @export step1_sample_mvt

step1_sample_mvt <- function (data, y, .time, K = NULL, C = NULL, validation = NULL,
                        draw, parms = NULL, reparameterisation = FALSE, zero_order = FALSE){

  if (is.null(K) & is.null(C))
    stop("Select the temperature variable in Kelvin or Celsius")
  if (!is.null(parms) & !is.list(parms))
    stop("The starting values for parameters must be a list, or keep as NULL")


  if(!is.null(C) & !is.null(K)) {

    data[, C] <- ifelse(is.na(data[, C]) & !is.na(data[, K]),
                        data$K - 273.15,
                        data[, C])

    data[, K] <- ifelse(is.na(data[, K]) & !is.na(data[, C]),
                        data$C + 273.15,
                        data[, K])
  }

  data <- data[complete.cases(data[, c(C,K,y,.time)]), ]

  dat = data

  if (!is.null(validation))
    if (!all(dat[,validation] %in% c(0,1)))
      stop("Validation column must contain 1s and 0s only")

  if (is.null(K))
    dat$K = dat[, C] + 273.15
  if (is.null(C)) {
    dat$C = dat[, K] - 273.15
    C = "C"}

  Kref = mean(dat$K)
  dat$Celsius = as.factor(dat[, C])
  dat$time = dat[, .time]
  dat$y = dat[, y]
  if(!is.null(validation)){
    dat$validation = ifelse(dat[,validation] == 0, "Fit", "Validation")
    if(validation != "validation"){
      dat <- dat[, !names(dat) %in% c(validation)]
    }
  }
  if(.time != "time"){
    dat <- dat[, !names(dat) %in% c(.time)]
  }
  if(y != "y"){
    dat <- dat[, !names(dat) %in% c(y)]
  }

  dat_full <- dat
  if(!is.null(validation)){
    dat <- dat[dat$validation == "Fit",]
  }

  if(reparameterisation & zero_order){ # reparameterisation and k3 is 0
    MyFctNL = function(parms) { # Make function
      k1 = parms$k1
      k2 = parms$k2
      c0 = parms$c0
      Model = c0 - c0 * dat$time * exp(k1 - k2/dat$K +
                                         k2/Kref)
      residual = dat$y - Model
      return(residual)
    }

    # Fit model :
    if (!is.null(parms)) {
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }
    else {
      repeat {
        parms = list(k1 = stats::runif(1, 0, 40), k2 = stats::runif(1,
                                                                    1000, 20000), c0 = mean(dat$y[dat$time == 0]))
        fit = suppressWarnings(minpack.lm::nls.lm(par = parms,
                                                  fn = MyFctNL, lower = rep(0, length(parms))))
        res = tryCatch({
          summary(fit)
        }, error = function(e) e, warning = function(w) w)
        res2 = tryCatch({
          stats::vcov(fit)
        }, error = function(e) e)
        if (any(stats::coef(fit) != parms) & !inherits(res, "error") &
            !inherits(res2, "error"))
          (break)()
      }
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }


    SIG = stats::vcov(fit)



        pred_fct = function(coef.fit)
        {
          degrad = pred$time * exp(coef.fit[1] - coef.fit[2] / pred$K + coef.fit[2] / Kref)
          conc = coef.fit[3] - coef.fit[3]*degrad
          return(conc)
        }
        # Multi T bootstrap
        rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 3) + matrix(nrow = draw, ncol = 3, byrow = TRUE, coef(fit))




  }else if(!reparameterisation & zero_order){ # no reparameterisation and k3 is 0
    MyFctNL = function(parms) { # make function
      k1 = parms$k1
      k2 = parms$k2
      c0 = parms$c0

      Model = c0 - c0 * dat$time * exp(k1 - k2 / dat$K)


      residual = dat$y - Model
      return(residual)
    }
    if (!is.null(parms)) { # fit model
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }
    else {
      repeat {
        parms = list(k1 = stats::runif(1, 0, 40), k2 = stats::runif(1,
                                                                    1000, 20000), c0 = mean(dat$y[dat$time == 0]))
        fit = suppressWarnings(minpack.lm::nls.lm(par = parms,
                                                  fn = MyFctNL, lower = rep(0, length(parms))))
        res = tryCatch({
          summary(fit)
        }, error = function(e) e, warning = function(w) w)
        res2 = tryCatch({
          stats::vcov(fit)
        }, error = function(e) e)
        if (any(stats::coef(fit) != parms) & !inherits(res, "error") &
            !inherits(res2, "error"))
          (break)()
      }
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }

    SIG = vcov(fit)

        pred_fct = function(coef.fit)
        {

          degrad = pred$time * exp(coef.fit[1] - coef.fit[2] / pred$K)
          conc = coef.fit[3] - coef.fit[3]*degrad
          return(conc)
        }
        # Multi T bootstrap
        rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 3) + matrix(nrow = draw, ncol = 3, byrow = TRUE, coef(fit))



  }else if(reparameterisation & !zero_order){ #reparameterisation and k3 is not zero
    MyFctNL = function(parms) {
      k1 = parms$k1
      k2 = parms$k2
      k3 = parms$k3
      c0 = parms$c0
      Model = c0 - c0 * (1 - ((1 - k3) * (1/(1 - k3) - dat$time *
                                            exp(k1 - k2/dat$K + k2/Kref)))^(1/(1 - k3)))
      residual = dat$y - Model
      return(residual)
    }
    if (!is.null(parms)) { # Fit the model
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }
    else {
      repeat {
        parms = list(k1 = stats::runif(1, 0, 60), k2 = stats::runif(1,
                                                                    1000, 20000), k3 = stats::runif(1, 0, 11), c0 = mean(dat$y[dat$time == 0]))
        fit = suppressWarnings(minpack.lm::nls.lm(par = parms,
                                                  fn = MyFctNL, lower = rep(0, length(parms))))
        res = tryCatch({
          summary(fit)
        }, error = function(e) e, warning = function(w) w)
        res2 = tryCatch({
          stats::vcov(fit)
        }, error = function(e) e)
        if (any(stats::coef(fit) != parms) & !inherits(res, "error") &
            !inherits(res2, "error"))
          (break)()
      }
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }

    k3 = coef(fit)[3]
    if (k3 == 0){print("k3 is fitted to be exactly 0, we strongly suggest using option zero_order = TRUE")
    }else if(confint(fit,'k3')[1] < 0 && confint(fit,'k3')[2] > 0){print(paste0("The 95% Wald Confidence Interval for k3 includes 0, k3 is estimated as ",signif(k3,4),". We suggest considering option zero_order = TRUE"))}

    SIG = vcov(fit)

        pred_fct = function(coef.fit)
        {
          degrad = 1 - ((1 - coef.fit[3]) * (1/(1 - coef.fit[3]) - pred$time * exp(coef.fit[1] - coef.fit[2] / pred$K + coef.fit[2] / Kref)))^(1/(1-coef.fit[3]))
          conc = coef.fit[4] - coef.fit[4]*degrad
          return(conc)
        }
        # Multi T bootstrap
        rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 4) + matrix(nrow = draw, ncol = 4, byrow = TRUE, coef(fit))


  }else if(!reparameterisation & !zero_order){ # No re-parameterisation and k3 not zero
    MyFctNL = function(parms) {
      k1 = parms$k1
      k2 = parms$k2
      k3 = parms$k3
      c0 = parms$c0

      test = c0 - c0 * (1 - ((1 - k3) * (1/(1 - k3) - dat$time * exp(k1 - k2 / dat$K)))^(1/(1-k3)))

      residual = dat$y - test
      return(residual)

    }
    if (!is.null(parms)) { # Fitting the model
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }
    else {
      repeat {
        parms = list(k1 = stats::runif(1, 0, 60), k2 = stats::runif(1,
                                                                    1000, 20000), k3 = stats::runif(1, 0, 11), c0 = mean(dat$y[dat$time == 0]))
        fit = suppressWarnings(minpack.lm::nls.lm(par = parms,
                                                  fn = MyFctNL, lower = rep(0, length(parms))))
        res = tryCatch({
          summary(fit)
        }, error = function(e) e, warning = function(w) w)
        res2 = tryCatch({
          stats::vcov(fit)
        }, error = function(e) e)
        if (any(stats::coef(fit) != parms) & !inherits(res, "error") &
            !inherits(res2, "error"))
          (break)()
      }
      fit = minpack.lm::nls.lm(par = parms, fn = MyFctNL, lower = rep(0,
                                                                      length(parms)))
    }

    k3 = coef(fit)[3]
    if (k3 == 0){print("k3 is fitted to be exactly 0, we strongly suggest using option zero_order = TRUE")
    }else if(confint(fit,'k3')[1] < 0 && confint(fit,'k3')[2] > 0){print(paste0("The 95% Wald Confidence Interval for k3 includes 0, k3 is estimated as ",signif(k3,4),". We suggest considering option zero_order = TRUE"))}

    SIG = vcov(fit)

        pred_fct = function(coef.fit)
        {
          degrad = 1 - ((1 - coef.fit[3]) * (1/(1 - coef.fit[3]) - pred$time * exp(coef.fit[1] - coef.fit[2] / pred$K)))^(1/(1-coef.fit[3]))
          conc = coef.fit[4] - coef.fit[4]*degrad
          return(conc)
        }
        # Multi T bootstrap
        rand.coef = rmvt(draw, sigma = SIG, df = nrow(dat) - 4) + matrix(nrow = draw, ncol = 4, byrow = TRUE, coef(fit))



  }

  if(zero_order){
    colnames(rand.coef) <- c("k1","k2","c0")
  }else{
    colnames(rand.coef) <- c("k1","k2","k3","c0")
  }

  return(rand.coef)

}

globalVariables(c('pred'))


