
#' simulate_latent_growth_curve
#'
#' simulate data for a latent growth curve model with five measurement occasions.
#' The time-distance between these occasions differs between subjects.
#' @param N sample size
#' @return data set with columns y1-y5 (observations) and t_1-t_5 (time of
#' observation)
#' @export
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @examples
#' set.seed(123)
#' dataset <- simulate_latent_growth_curve(N = 100)
#'
#' model <- "
#'   I =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
#'   S =~ data.t_1 * y1 + data.t_2 * y2 + data.t_3 * y3 + data.t_4 * y4 + data.t_5 * y5
#'
#'   I ~ int*1
#'   S ~ slp*1
#'
#'   # set intercepts of manifest variables to zero
#'   y1 ~ 0*1; y2 ~ 0*1; y3 ~ 0*1; y4 ~ 0*1; y5 ~ 0*1;
#'   "
#'
#' mod <- mxsem(model = model,
#'              data = dataset) |>
#'   mxTryHard()
simulate_latent_growth_curve <- function(N = 100){
  Tpoints <- 5
  I <- rnorm(N, mean = 1)
  S <- rnorm(N, mean = .4)

  dataset <- matrix(
    NA,
    nrow = N,
    ncol = 2*Tpoints,
    dimnames = list(NULL, c(paste0("y", 1:Tpoints), paste0("t_", 1:Tpoints)))
  )

  for(i in 1:N){
    dataset[i,paste0("t_", 1:Tpoints)] <- cumsum(c(0, runif(n = Tpoints-1, min = .3, max = 2)))
    dataset[i,paste0("y", 1:Tpoints)] <- 1*I[i] + dataset[i,paste0("t_", 1:Tpoints)]*S[i] + rnorm(Tpoints, 0, .2)
  }

  return(dataset)
}

#' simulate_moderated_nonlinear_factor_analysis
#'
#' simulate data for a moderated nonlinear factor analysis.
#' @param N sample size
#' @return data set with variables x1-x3 and y1-y3 representing repeated measurements
#' of an affect measure. It is assumed that the autoregressive effect is different
#' depending on covariate k
#' @export
#' @importFrom stats rnorm
#' @examples
#' library(mxsem)
#' set.seed(123)
#' dataset <- simulate_moderated_nonlinear_factor_analysis(N = 2000)
#'
#' model <- "
#' xi =~ x1 + x2 + x3
#' eta =~ y1 + y2 + y3
#' eta ~ a*xi
#'
#' # we need two new parameters: a0 and a1. These are created as follows:
#' !a0
#' !a1
#' # Now, we redefine a to be a0 + k*a1, where k is found in the data
#' a := a0 + data.k*a1
#' "
#'
#' mod <- mxsem(model = model,
#'              data = dataset) |>
#'   mxTryHard()
#'
#' omxGetParameters(mod)
simulate_moderated_nonlinear_factor_analysis <- function(N){

  k <- sample(c(TRUE,FALSE), N, replace = TRUE)

  a <- .7 - .2*k
  xi <- rnorm(N)
  eta <- a*xi + rnorm(N, sd = .5)

  dataset <- matrix(NA,
                    nrow = N,
                    ncol = 7,
                    dimnames = list(NULL, c(paste0("x",1:3), paste0("y", 1:3), "k")))
  for(i in 1:N){
    dataset[i,paste0("x",1:3)] <- c(1,.8,.9) * xi[i] + rnorm(3,0,.2)
    dataset[i,paste0("y",1:3)] <- c(1,.8,.9) * eta[i] + rnorm(3,0,.2)
    dataset[i,"k"] <- k[i]
  }
  return(dataset)
}
