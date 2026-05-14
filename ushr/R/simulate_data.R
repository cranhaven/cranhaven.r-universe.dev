#' Create data function
#'
#' This function simulates example data that can be used to explore model fitting and plotting within the package. Subjects are assumed to be observed at regular intervals until either the end of the study or they are lost to follow up.
#'
#' @param nsubjects numeric value indicating the number of subjects you want to simulate data for. Default is 10.
#' @param detection_threshold numeric value indicating the detection threshold of the assay used to measure viral load. Measurements below this value will be assumed to represent undetectable viral load levels. Default value is 20.
#' @param censortime numeric value indicating the maximum time point to include in the analysis. Default value is 365.
#' @param max_datapoints numeric value indicating the maximum number of data points collected from any subject. Defaults to 24.
#' @param min_datapoints numeric value indicating the minimum number of data points collected from any subject. Defaults to 6.
#' @param sd_noise numeric value indicating the standard deviation level to be used when adding noise to the simulated data (on the log10 scale). Default value is 0.1
#' @param param_noise numeric vector indicating the standard deviation to be used when selecting parameter values (on the log scale). Order of entries should be: A, delta, B, gamma. Default value is c(1.5, 0.1, 1.5, 0.1).
#' @param mean_params named numeric vector indicating the mean parameter values for the subject decay curves. Default is c(A = 10000, delta = 0.3, B = 10000, gamma = 0.03).
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
simulate_data <- function(nsubjects = 10, detection_threshold = 20, censortime = 365,
                          max_datapoints = 24, min_datapoints = 6,
                          sd_noise = 0.1, param_noise = c(1.5, 0.1, 1.5, 0.1),
                          mean_params = c(A = 10000, delta = 0.3, B = 10000, gamma = 0.03)){

    if (!is.numeric(c(nsubjects, detection_threshold, censortime, max_datapoints, min_datapoints, sd_noise) )) {
        stop("The following arguments must be numeric: nsubjects, detection_threshold, censortime, max_datapoints, min_datapoints, sd_noise")
    }

    if (!is.numeric(mean_params)) {
        stop("The 'mean_params' argument must have numeric values for A, delta, B, and gamma.")
    }

    if (!all(c("A", "delta", "B", "gamma") %in% names(mean_params))) {
        stop("The 'mean_params' argument must be a named vector with values for A, delta, B, and gamma.")
    }

   # 1. Give IDs to all subjects
    ids <- paste0("S", 1:nsubjects)

    # 2. Simulate parameters for each subject
    params <- exp(stats::rnorm(n = nsubjects * length(mean_params),
                        mean = log(mean_params),
                        sd = param_noise)) %>%
        matrix(byrow = T, ncol = length(mean_params))

    colnames(params) <- names(mean_params)

    params <- t(apply(params, 1, switch_simulated_params))

    paramdat <- params %>% as_tibble() %>% mutate(id = ids)

    # 3. Choose number of observations for all subjects
    npoints <- sample(min_datapoints:max_datapoints, replace = TRUE, size = nsubjects)

    # 4. Choose timing of observations for all subjects (simulate_time),
    # 5. then get viral load at those timepoints using the biphasic model and simulated parameters (simulate_vl)

    data <- data.frame(index = 1:nsubjects, npoints = npoints, id = ids) %>% group_by(id) %>%
        do(simulate_time(.$npoints, censortime, .$id, .$index, max_datapoints)) %>%
        do(simulate_vl(timevec = .$time, params = params[.$index[1],], id = .$id)) %>% ungroup()

    # 6. Add noise to viral load measurements
    simulated_data <- data %>% mutate(vl = add_noise(vl, sd_noise),
                                      vl = ifelse(vl < detection_threshold, detection_threshold/2, vl),
                                      id = as.character(id)) %>% left_join(paramdat)

    return(simulated_data)
}


# #' Simulate timepoints for subjects at random.
# #'
# #' This function simulates observed timepoints randomly for each subject.
# #'
# #' @param npoints numeric value indicating the number of observations to be sampled.
# #' @param censortime numeric value indicatingthe maximum time point to inculde in the analysis.
# #' @param id subject id. Can be numeric or a character.
# #' @param index numeric identifier for each subject/model combination.
# #' @param max_datapoints numeric value indicating the maximum number of data points collected from any subject.
# #'
# simulate_time_random <- function(npoints, censortime, id, index, max_datapoints){
#
#     initial_phase <- floor(censortime/2)
#
#     if (npoints > max_datapoints/2) {
#         npoints_initial <- floor(npoints/2)
#
#         timepoints_initial <- sort(sample(1:initial_phase, size = npoints_initial, replace = FALSE))
#         timepoints_late <- sort(sample((initial_phase + 1):censortime, size = npoints - npoints_initial, replace = FALSE))
#
#         timepoints <- c(timepoints_initial, timepoints_late)
#     } else {
#         timepoints <- sort(sample(1:initial_phase, size = npoints, replace = FALSE))
#     }
#
#     return(data.frame(time = timepoints, id = id, index = index))
# }


#' Simulate timepoints for subjects according to fixed design.
#'
#' This function simulates observed timepoints for each subject according to a fixed sampling design.
#'
#' @param npoints numeric value indicating the number of observations to be sampled.
#' @param censortime numeric value indicating the maximum time point to include in the analysis.
#' @param id subject id. Can be numeric or a character.
#' @param index numeric identifier for each subject/model combination.
#' @param max_datapoints numeric value indicating the maximum number of data points collected from any subject.
#'
simulate_time_fixed <- function(npoints, censortime, id, index, max_datapoints){

    sample_sequence <- seq(1, censortime, length.out = max_datapoints)

    timepoints <- sample_sequence[1:npoints]

    return(data.frame(time = timepoints, id = id, index = index))
}

#' Simulate timepoints for subjects
#'
#' This function chooses the correct function for sampling observation times.
#'
#' @param npoints numeric value indicating the number of observations to be sampled.
#' @param censortime numeric value indicating the maximum time point to include in the analysis.
#' @param id subject id. Can be numeric or a character.
#' @param index numeric identifier for each subject/model combination.
#' @param max_datapoints numeric value indicating the maximum number of data points collected from any subject.
#'
simulate_time <- function(npoints, censortime, id, index, max_datapoints){

        output <- simulate_time_fixed(npoints, censortime, id, index, max_datapoints)

    return(output)
}

#' Simulate vl for subjects
#'
#' This function simulates observed vl for each subject.
#'
#' @param params named numeric vector of parameter values to simulate the biphasic model.
#' @param timevec numeric vector of observed timepoints.
#' @param id subject id. Can be numeric or a character.
#'
simulate_vl <- function(params, timevec, id){
    vl <- get_biphasic(params, timevec)

    return(data.frame(time = timevec, vl = vl, id = id))
}


#' Add noise to viral load observations
#'
#' This function adds noise to vl measurements for each subject.
#'
#' @param vl numeric vector of viral load measurements.
#' @param sd_noise numeric value indicating the standard deviation level to be used when adding noise to the simulated data (on the log10 scale).
#'
add_noise <- function(vl, sd_noise){
    logvl <- log10(vl) + stats::rnorm(n = length(vl), mean = 0, sd = sd_noise)

    return(10^(logvl))
}


#' Switch names of simulated rate parameters
#'
#' This function switches the names of delta and gamma estimates if gamma > delta.
#' @param params matrix of parameter estimates
#'
switch_simulated_params <- function(params){

    if (params["gamma"] > params["delta"]) {

        tmpRate <- params["gamma"]

        params["gamma"] <- params["delta"]
        params["delta"] <- tmpRate
    }
    if (params["B"] > params["A"]) {

        tmpConst <- params["B"]

        params["B"] <- params["A"]
        params["A"] <- tmpConst
    }

    return(params)
}
