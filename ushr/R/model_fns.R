#' Compute the biphasic model curve
#'
#' This function calculates the biphasic model, V(t), for a vector of input times, t
#' @param params named numeric vector of all parameters needed to compute the biphasic model, V(t)
#' @param timevec numeric vector of the times, t, at which V(t) should be calculated
#' @return numeric vector of viral load predictions, V(t), for each time point in 'timevec'
#' @export
#' @examples
#'
#' get_biphasic(params = c(A = 10000, delta = 0.68, B = 1000, gamma = 0.03),
#'              timevec = seq(1, 100, length.out = 100))
#'
get_biphasic <- function(params, timevec){
    if(length(params) < 4){
        stop("The biphasic model needs 4 parameters: A, delta, B, gamma")
    }

    params["A"] * exp(- params["delta"] * timevec) + params["B"] * exp(- params["gamma"] * timevec)
}


#' Compute the single phase model curve
#'
#' This function calculates the single phase model, V(t), for vector of input times, t
#' @param params named numeric vector of all parameters needed to compute the single phase model, V(t)
#' @param timevec numeric vector of the times, t, at which V(t) should be calculated
#' @return numeric vector of viral load predictions, V(t), for each time point in 'timevec'
#' @export
#' @examples
#'
#' get_singlephase(params = c(B = 1000, gamma = 0.68), timevec = seq(1, 100, length.out = 100))
#'
get_singlephase <- function(params, timevec){
    if(length(params) < 2){
        stop("The single phase model needs 2 parameters: B, gamma")
    }
  params["B"] * exp(- params["gamma"] * timevec)
}


#' Compute the model for a given subject's data and best-fit parameters
#'
#' This function calculates the biphasic or single phase model given a subject's data and best-fit parameters
#' @param data data frame with columns for the subject's identifier ('id') and timing of sampling ('time')
#' @param best_param named numeric vector of best fit parameters obtained from fitting the biphasic or single phase model to the subjects data
#' @param param_names character vector containing the names of the parameters in 'best_param'
#' @param whichcurve character indicating which model function should be used. Use 'get_biphasic' for the biphasic model, or 'get_singlephase' for the single phase model. Defaults to 'get_biphasic'.
#' @return data frame with columns for the sampling times ('time'), fitted viral load predictions ('fit'), and the corresponding subject identifier ('id')
#' @export
#' @examples
#'
#' nobs <- 7
#' example_param <- c(A = 10000, delta = 0.03, B = 1000, gamma = 0.68)
#'
#' vldata <- get_biphasic(params = example_param, timevec = seq(5, 100, length.out = nobs))
#'
#' subjectdata <- data.frame(id = 123, time = seq(5, 100, length.out = nobs),
#'                           vl = 10^ (log10(vldata) + rnorm(nobs, 0, 0.2)))
#'
#' get_curve(data = subjectdata, best_param = example_param, param_names = names(example_param))

get_curve <- function(data, best_param, param_names, whichcurve = get_biphasic){

    mint <- min(data$time) - 1
    maxt <- max(data$time) + 1

    tscale <- seq(mint, maxt, 0.5)
    tscale <- tscale[tscale >= 0]

    modelfit <- data.frame('time'= tscale,'fit'= whichcurve(params = best_param[param_names], tscale),
                           'id' = rep(data$id[1], times = length(tscale)))

    return(modelfit)
}

