#' Tests if a fit is a semi parametric or nonparametric model.
#'
#'
#' @param fit The result of the fit, obtained through the function MixNRMI1 or
#' MixNRMI2.
#' @return TRUE if the fit is a semiparametric model
#' @examples
#'
#' set.seed(150520)
#' data(acidity)
#' x <- enzyme
#' out <- MixNRMI1(enzyme, extras = TRUE, Nit = 10)
#' BNPdensity:::is_semiparametric(out)
is_semiparametric <- function(fit) {
  return(!is.null(fit$S))
}

convert_nan_to_0 <- function(vec) {
  ifelse(is.nan(vec), yes = 0, no = vec)
}

#' Repeat the common scale parameter of a semiparametric model to match the
#' dimension of the location parameters.
#'
#'
#' @param semiparametric_fit The result of the fit, obtained through the
#' function MixNRMI1.
#' @return an adequate list of vectors of sigmas
fill_sigmas <- function(semiparametric_fit) {
  mapply(FUN = function(means, sigma) {
    rep(sigma, length(means))
  }, semiparametric_fit$means, semiparametric_fit$S)
}



#' Create a plotting grid from non-censored data.
#'
#'
#' @param data Non-censored input data from which to compute the grid.
#' @param npoints Number of points on the grid.
#' @return a vector containing the plotting grid
grid_from_data_noncensored <- function(data, npoints = 100) {
  data_range <- max(data) - min(data)
  return(seq(min(data) - 0.1 * data_range, max(data) + 0.1 * data_range, length.out = 100))
}



#' Create a plotting grid from censored data.
#'
#'
#' @param data Censored input data from which to compute the grid.
#' @param npoints Number of points on the grid.
#' @return a vector containing the plotting grid
grid_from_data_censored <- function(data, npoints = 100) {
  max_ <- max(max(data$left, na.rm = T), max(data$right, na.rm = T))
  min_ <- min(min(data$left, na.rm = T), min(data$right, na.rm = T))
  data_range_adapted <- (max_ - min_) / sqrt(nrow(data))
  return(seq(min_ - 0.1 * data_range_adapted, max_ + 0.1 * data_range_adapted, length.out = 100))
}



#' Create a plotting grid from censored or non-censored data.
#'
#'
#' @param data Input data from which to compute the grid.
#' @param npoints Number of points on the grid.
#' @return a vector containing the plotting grid
grid_from_data <- function(data, npoints = 100) {
  if (is_censored(data)) {
    grid_from_data_censored(data, npoints = npoints)
  } else {
    grid_from_data_noncensored(data, npoints = npoints)
  }
}




#' Test if the data is censored
#'
#'
#' @param dat The dataset to be tested
#' @return TRUE if the data is censored
#' @examples
#'
#' data(salinity)
#' BNPdensity:::is_censored(salinity)
is_censored <- function(dat) {
  if (is.null(ncol(dat))) {
    FALSE
  } else {
    TRUE
  }
}




#' Compute the grid for thinning the MCMC chain
#'
#' This function creates an real grid then rounds it. If the grid is fine
#' enough, there is a risk that rounding ties, i.e. iteration which are kept
#' twice. To avoid this, if the total number of iterations is smaller than
#' twice the number of iterations desired after thinning, the chain is not
#' thinned.
#'
#' @param Nit Length of the MCMC chain
#' @param thinning_to Desired number of iterations after thinning.
#' @return an integer vector of the MCMC iterations retained.
compute_thinning_grid <- function(Nit, thinning_to = 10) {
  if (Nit <= 2 * thinning_to) { # Factor 2 to reduce the probability of having the same iterations selected twice
    it_retained <- 1:Nit
  } else {
    it_retained <- round(seq(1, Nit, length.out = thinning_to))
  }
  return(it_retained)
}


#' Add x and y
#'
#' This is a helper function for use in Reduce() over a list of vectors
#'
#' @param x first argument of the sum
#' @param y second argument of the sum
#'
#' @return x + y
#'
add <- function(x, y) {
  x + y
}

#' Extract the Conditional Predictive Ordinates (CPOs) from a fitted object
#'
#' @param object A fit obtained through one of the NRMI functions
#' @param ... Additional arguments (not used)
#'
#' @return A vector of Conditional Predictive Ordinates (CPOs)
#' @export
cpo <- function(object, ...) {
  UseMethod("cpo")
}

#' Extract the Conditional Predictive Ordinates (CPOs) from a fitted object
#'
#' @param object A fit obtained through one of the NRMI functions
#' @param ... Additional arguments (not used)
#'
#' @return A vector of Conditional Predictive Ordinates (CPOs)
#' @export
cpo.default <- function(object, ...) {
  stop("No 'cpo' method available for objects of class: ", class(object))
}
