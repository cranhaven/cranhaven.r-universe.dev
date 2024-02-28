#' @title Step1 Down Model Root Mean Square Error Calculation
#'
#' @description Calculate Root Mean Square Error (RMSE) for the one-step Šesták–Berggren kinetic model.
#'
#' @details Calculate RMSE for the one-step Šesták–Berggren kinetic (non-linear) model using
#'  user provided parameters.
#'
#' @param data Dataframe containing accelerated stability data (required).
#' @param y Name of decreasing variable (e.g. concentration) contained within data (required).
#' @param .time Time variable contained within data (required).
#' @param K Kelvin variable (numeric or column name) (optional).
#' @param C Celsius variable (numeric or column name) (optional).
#' @param parms Values for the parameters as a list - k1, k2, k3, and c0. If multiple are provided all combinations will be used (required).
#' @param reparameterisation Use alternative parameterisation of the one-step
#'  model which aims to reduce correlation between k1 and k2.
#'
#' @return A data frame containing one row for each RMSE calculation
#'
#' @examples #load antigenicity and potency data.
#' data(antigenicity)
#' data(potency)
#'
#' #Basic use of the step1_down_rmse function with C column defined.
#' rmse1 <- step1_down_rmse(data = antigenicity, y = "conc", .time = "time",
#'  C = "Celsius", parms = list(c0 = c(96,98,100), k1 = c(42,45),
#'   k2 = c(12000,12500), k3 = c(8,9,10)))
#'
#' #Basic use of the step1_down_rmse function with K column defined.
#' rmse2 <- step1_down_rmse(data = antigenicity, y = "conc", .time = "time",
#'  K = "K", parms = list(c0 = c(98), k1 = c(42,45), k2 = c(12500), k3 = c(8,9)))
#'
#' #reparameterisation is TRUE.
#' rmse3 <- step1_down_rmse(data = antigenicity, y = "conc", .time = "time",
#'  C = "Celsius", parms = list(c0 = c(100,95), k1 = c(2,2.5), k2 = c(12000,13000),
#'   k3 = c(9,10)), reparameterisation = TRUE)
#'
#' @importFrom dplyr %>% mutate
#'
#' @export step1_down_rmse

step1_down_rmse <- function (data, y, .time, K = NULL, C = NULL,
                        parms, reparameterisation = FALSE){

  if (is.null(K) & is.null(C))
    stop("Select the temperature variable in Kelvin or Celsius")
  if (!is.list(parms))
    stop("The starting values for parameters must be a list")

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

  if (is.null(K))
    dat$K = dat[, C] + 273.15
  if (is.null(C)) {
    dat$C = dat[, K] - 273.15
    C = "C"}

  Kref = mean(dat$K)
  dat$Celsius = as.factor(dat[, C])
  dat$time = dat[, .time]
  dat$y = dat[, y]
  if(.time != "time"){
    dat <- dat[, !names(dat) %in% c(.time)]
  }
  if(y != "y"){
    dat <- dat[, !names(dat) %in% c(y)]
  }

  result_grid <- expand.grid(parms) %>% mutate(rmse = NA)

  if(reparameterisation){
    for (i in 1:dim(result_grid)[1]){
      c0 <- result_grid[i,]$c0
      k1 <- result_grid[i,]$k1
      k2 <- result_grid[i,]$k2
      k3 <- result_grid[i,]$k3

      dat$Degradation = 1 - ((1 - k3) * (1/(1 - k3) - dat$time * exp(k1 - k2 / dat$K + k2 / Kref)))^(1/(1-k3))
      dat$Response    = c0 - c0*dat$Degradation
      dat$sqrResidual = (dat$Response - dat$y)^2

      result_grid[i,'rmse'] <- sqrt(mean(dat$sqrResidual))
    }
  }else{
    for (i in 1:dim(result_grid)[1]){
      c0 <- result_grid[i,]$c0
      k1 <- result_grid[i,]$k1
      k2 <- result_grid[i,]$k2
      k3 <- result_grid[i,]$k3

      dat$Degradation = 1 - ((1 - k3) * (1/(1 - k3) - dat$time * exp(k1 - k2 / dat$K)))^(1/(1-k3))
      dat$Response = c0 - c0*dat$Degradation
      dat$sqrResidual = (dat$Response - dat$y)^2

      result_grid[i,'rmse'] <- sqrt(mean(dat$sqrResidual))
    }
  }

  return(result_grid)

}
