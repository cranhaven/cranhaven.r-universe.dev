#'Logit
#'
#'Calculating the logit of a probability
#'
#'@param p a numerical value defining the probability to be transformed into logit scale.
#'
#'@return \code{logit} a numerical value in logit scale.
#'@export
logit <- function(p) {
  out <- log(p/(1 - p))
  return(out)
}

comb <- function(...) {
  mapply('rbind', ..., SIMPLIFY=FALSE)
}

#'Standardization of the dose
#'
#'Standardizing a dose between 0 and 1.
#'
#'@param dose a numerical value defining the dose to be standardized.
#'@param min_dose a numerical value defining the lower bound of the support of
#'the MTD.
#'@param max_dose a numerical value defining the upper bound of the support of
#'the MTD.
#'
#'@return \code{standardized dose} a numerical value between 0 and 1.
#'
#'@export
standard_dose <- function(dose, min_dose, max_dose) {

  out <- (dose - min_dose)/(max_dose - min_dose)
  return(out)
}

#'Inverse standardization of the dose
#'
#'Unstandardizing a dose between minimum and maximum doses.
#'
#'@param dose a numerical value defining the standardized dose to be unstandardized.
#'@param min_dose a numerical value defining the lower bound of the support of
#'the MTD.
#'@param max_dose a numerical value defining the upper bound of the support of
#'the MTD.
#'
#'@return \code{dose} a numerical value between \code{min_dose} and \code{max_dose}.
#'
#'@export
inv_standard_dose <- function(dose, min_dose, max_dose) {

  out <- dose*(max_dose - min_dose) + min_dose
  return(out)
}

round_down <- function(dose, grid){

  dif <- dose - grid
  index <- ifelse(length(which(dif >= 0)) == 0, 1, max(which(dif >= 0)))
  out <- grid[index]
  return(out)
}

round_nearest <- function(dose, grid){

  dif <- abs(dose - grid)
  index <- ifelse(length(which.min(dif)) == 0, 1, which.min(dif))
  out <- grid[index]
  return(out)
}

rounding_system <- function(dose, grid, rounding) {

  dose <- matrix(dose, ncol = 1)

  if (rounding == "down")
    out <- apply(dose, 1, round_down, grid =  grid)
  if (rounding == "nearest")
    out <- apply(dose, 1, round_nearest, grid = grid)

  return(out)

}

next_dose <- function(data) {
  UseMethod("next_dose")
}

jags <- function(data, n_adapt, burn_in, n_mcmc, n_thin, n_chains) {
  UseMethod("jags")
}

feasibility <- function(alpha, strategy, rate, dlt, resolution){
  if (strategy == "constant")
    next_alpha <- alpha[1]
  if (strategy == "increasing")
    next_alpha <- ifelse(round(alpha[length(alpha)], 3) >= 0.5, 0.5,
                         round((alpha[length(alpha)] + rate), 3))
  if (strategy == "conditional")
    next_alpha <-
      ifelse(round(alpha[length(alpha)], 3) >= 0.5, 0.5,
             ifelse(round((alpha[1] + sum(resolution*(1 - dlt))*rate), 1) >= 0.5,
                    0.5, round((alpha[1] + sum(resolution*(1 - dlt))*rate), 3)))

  out <- next_alpha
  return(out)
}





