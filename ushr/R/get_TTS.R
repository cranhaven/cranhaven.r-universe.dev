#' Prepare input data for non-parametric TTS calculations.
#'
#' This function prepares the raw input data for TTS interpolation. Individuals whose data do not meet specific inclusion criteria are removed (see Vignette for more details).
#'
#' Steps include:
#' 1. Setting values below the suppression threshold to half the suppression threshold (following standard practice).
#' 2. Filtering out subjects who do not suppress viral load below the suppression threshold by a certain time.
#' 3. Filtering out subjects who do not have a decreasing sequence of viral load (within some buffer range).
#' @param data raw data set. Must be a data frame with the following columns: 'id' - stating the unique identifier for each subject; 'vl' - numeric vector stating the viral load measurements for each subject; 'time' - numeric vector stating the time at which each measurement was taken.
#' @param suppression_threshold numeric value indicating the suppression threshold: measurements below this value will be assumed to represent viral suppression. Typically this would be the detection threshold of the assay. Default value is 20.
#' @param uppertime the maximum time point to include in the analysis. Subjects who do not suppress viral load below the suppression threshold within this time will be discarded from model fitting. Units are assumed to be the same as the 'time' column. Default value is 365.
#' @param censor_value positive numeric value indicating the maximum time point to include in the analysis. Subjects who do not suppress viral load below the detection threshold within this time will be discarded. Units are assumed to be the same as the 'time' column. Default value is 365.
#' @param decline_buffer the maximum allowable deviation of values away from a strictly decreasing sequence in viral load. This allows for e.g. measurement noise and small fluctuations in viral load. Default value is 500.
#' @param initial_buffer numeric (integer) value indicating the maximum number of initial observations from which the beginning of each trajectory will be chosen. Default value is 3.
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' filter_dataTTS(data = simulated_data)
#'
filter_dataTTS <- function(data, suppression_threshold = 20,
                           uppertime = 365, censor_value = 10,
                           decline_buffer = 500, initial_buffer = 3){

    # Check that data frame includes columns for 'id', 'time', 'vl'
    if (!(all(c("vl", "time", "id") %in% names(data)))) {
        stop("Data frame must have named columns for 'id', 'time', and 'vl'")
    }

    if (censor_value > suppression_threshold) {
        warning("censor_value must be less than or equal to the suppression threshold. Defaulting to half the suppression threshold.")

        censor_value <- 0.5 * suppression_threshold
    }

    if (censor_value < 0) {
        warning("censor_value must be positive. Defaulting to half the suppression threshold.")

        censor_value <- 0.5 * suppression_threshold
    }

    # 1. Change everything <= suppression_threshhold to censor_Value
    data_filtered <- data %>% mutate(vl = case_when(vl <= suppression_threshold ~ censor_value,
                                                    vl >= suppression_threshold ~ vl) ) %>%
        # 2. Look at only those who reach control within user defined uppertime
        filter(time <= uppertime) %>% group_by(id) %>%
        filter(any(vl <= suppression_threshold)) %>% ungroup() %>%
        # 3a. Isolate data from the highest VL measurement (from points 1 - 3) to the first point below detection
        filter(!is.na(vl)) %>% group_by(id) %>%
        slice(which.max(vl[1:initial_buffer]):Position(function(x) x <= suppression_threshold, vl)) %>%
        ungroup() %>%
        # 3b. Only keep VL sequences that are decreasing with user defined buffer...
        group_by(id) %>% filter(all(vl <= cummin(vl) + decline_buffer))

    return(data_filtered)
}


#' Biphasic root function
#'
#' This function defines the root equation for the biphasic model, i.e. V(t) - suppression_threshold = 0.
#'
#' @param timevec numeric vector of the times, t, at which V(t) should be calculated
#' @param params named vector of all parameters needed to compute the biphasic model, V(t)
#' @param suppression_threshold suppression threshold: measurements below this value will be assumed to represent viral suppression. Typically this would be the detection threshold of the assay. Default value is 20.
#' @export
#'
biphasic_root <- function(timevec, params, suppression_threshold){
    value <- params["A"] * exp (- timevec * params["delta"]) + params["B"] * exp( - timevec * params["gamma"]) - suppression_threshold
    as.numeric(value)
}


#' Single phase root function
#'
#' This function defines the root equation for the single phase model, i.e. V(t) - suppression_threshold = 0.
#'
#' @param timevec numeric vector of the times, t, at which V(t) should be calculated
#' @param params named vector of all parameters needed to compute the single phase model, V(t)
#' @param suppression_threshold suppression threshold: measurements below this value will be assumed to represent viral suppression. Typically this would be the detection threshold of the assay. Default value is 20.
#' @export
#'
single_root <- function(timevec, params, suppression_threshold){
    if (all(c("B", "gamma") %in% params)) {
        value <- params["B"] * exp( - timevec * params["gamma"]) - suppression_threshold
    } else{
        value <- params["Bhat"] * exp( - timevec * params["gammahat"]) - suppression_threshold
    }

    as.numeric(value)
}


#' Triphasic root function
#'
#' This function defines the root equation for the triphasic model, i.e. V(t) - suppression_threshold = 0.
#'
#' @param timevec numeric vector of the times, t, at which V(t) should be calculated
#' @param params named vector of all parameters needed to compute the triphasic model, V(t)
#' @param suppression_threshold suppression threshold: measurements below this value will be assumed to represent viral suppression. Typically this would be the detection threshold of the assay. Default value is 20.
#' @export
#'
triphasic_root <- function(timevec, params, suppression_threshold){
    value <- params["A"] * exp (- timevec * params["delta"]) + params["A_b"] * exp (- timevec * params["delta_b"]) + params["B"] * exp( - timevec * params["gamma"]) - suppression_threshold
    as.numeric(value)
}


#' Parametric TTS function
#'
#' This function computes the parametric form of the time to suppression
#'
#' @param params named vector of all parameters needed to compute the suppression model, V(t)
#' @param rootfunction specifies which function should be used to calculate the root: biphasic or single phase.
#' @param suppression_threshold suppression threshold: measurements below this value will be assumed to represent viral suppression. Typically this would be the detection threshold of the assay. Default value is 20.
#' @param uppertime numeric value indicating the maximum time that will be considered. Default value is 365.
#' @export
#'
get_parametricTTS <- function(params, rootfunction, suppression_threshold, uppertime){
    TTS <- rep(NA, nrow(params))

    for (i in 1:nrow(params)){
        TTS[i] = stats::uniroot(rootfunction, lower = 1, upper = uppertime,
                         params = params[i,], suppression_threshold = suppression_threshold)$root
    }
    return(TTS)
}


#' Non-parametric TTS function
#'
#' This function computes the non-parametric form of the time to suppression
#'
#' @param vl numeric vector of viral load measurements.
#' @param suppression_threshold numeric value for the suppression threshold: measurements below this value will be assumed to represent viral suppression. Typically this would be the detection threshold of the assay. Default value is 20.
#' @param time numeric vector indicating the time when vl measurements were taken.
#' @param npoints numeric value indicating the number of interpolation points to be considered.
#' @export
#'
get_nonparametricTTS <- function(vl, suppression_threshold, time, npoints){

    TTS <- time[which(vl == suppression_threshold)[1]]
    firstbelow <- which(vl < suppression_threshold)[1]

    if(is.na(TTS) | (!is.na(time[firstbelow]) & (time[firstbelow] < TTS)) ){
        lastabove <- time[firstbelow - 1]

        yax <- c(vl[firstbelow - 1], vl[firstbelow])
        xax <- c(lastabove, time[firstbelow])

        interpolation <- stats::approx(xax, yax, n = npoints)
        TTS <- interpolation$x[interpolation$y <= suppression_threshold][1]
    }
    return(TTS)

}


#' Time to suppression (TTS) function
#'
#' This function calculates the time to suppress HIV below a specified threshold.
#'
#' Options include: parametric (i.e. using the fitted model) or non-parametric (i.e. interpolating the processed data).
#' @param model_output output from fitting model. Only required if parametric = TRUE.
#' @param data raw data set. Must be a data frame with the following columns: 'id' - stating the unique identifier for each subject; 'vl'- numeric vector stating the viral load measurements for each subject; 'time'  - numeric vector stating the time at which each measurement was taken. Only required if parametric = FALSE.
#' @param suppression_threshold suppression threshold: measurements below this value will be assumed to represent viral suppression. Typically this would be the detection threshold of the assay. Default value is 20.
#' @param uppertime the maximum time interval to search for the time to suppression. Default value is 365.
#' @param censor_value positive numeric value indicating the maximum time point to include in the analysis. Subjects who do not suppress viral load below the detection threshold within this time will be discarded. Units are assumed to be the same as the 'time' column. Default value is 365.
#' @param decline_buffer the maximum allowable deviation of values away from a strictly decreasing sequence in viral load. This allows for e.g. measurement noise and small fluctuations in viral load. Default value is 500.
#' @param initial_buffer numeric (integer) value indicating the maximum number of initial observations from which the beginning of each trajectory will be chosen. Default value is 3.
#' @param parametric logical TRUE/FALSE indicating whether time to suppression should be calculated using the parametric (TRUE) or non-parametric (FALSE) method. If TRUE, a fitted model object is required. If FALSE, the raw data frame is required. Defaults to TRUE.
#' @param ARTstart logical TRUE/FALSE indicating whether the time to suppression should be represented as time since ART initiation. Default = FALSE. If TRUE, ART initiation times must be included as a data column named 'ART'.
#' @param npoints numeric value of the number of interpolation points to be considered. Default is 1000.
#' @return a data frame containing all individuals who fit the inclusion criteria, along with their TTS estimates, and a column indicating whether the parametric or nonparametric approach was used.
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' get_TTS(data = simulated_data, parametric = FALSE)
#'
get_TTS <- function(model_output = NULL, data = NULL,
                    suppression_threshold = 20, uppertime = 365, censor_value = 10,
                    decline_buffer = 500, initial_buffer = 3,
                    parametric = TRUE, ARTstart = FALSE, npoints = 1000){

    # 1. Parametric TTS ----------------------------------------------------------------
    if(parametric == TRUE){

        if(is.null(model_output)){
            stop("Model output not found. You must supply the fitted model to calculate parametric TTS values. Try ?get_model_fits.")
        }

        if (length(model_output$triphasicCI) > 0) {
            # Triphasic
            triphasic_params <- model_output$triphasicCI %>%
                select(-lowerCI, -upperCI) %>% spread(param, estimate) %>%
                mutate(TTS = get_parametricTTS(params = ., rootfunction = triphasic_root, suppression_threshold, uppertime),
                       model = "triphasic", calculation = "parametric")

            # All
            TTS_output <- triphasic_params %>% select(id, TTS, model, calculation)

        } else if (length(model_output$biphasicCI) > 0 & length(model_output$singleCI) > 0) {
            # Biphasic
            biphasic_params <- model_output$biphasicCI %>%
                select(-lowerCI, -upperCI) %>% spread(param, estimate) %>%
                mutate(TTS = get_parametricTTS(params = ., rootfunction = biphasic_root, suppression_threshold, uppertime),
                       model = "biphasic", calculation = "parametric")

            # Single phase
            single_params <- model_output$singleCI %>%
                select(-lowerCI, -upperCI) %>% spread(param, estimate) %>%
                mutate(TTS = get_parametricTTS(params = ., rootfunction = single_root, suppression_threshold, uppertime),
                       model = "single phase", calculation = "parametric")

            # All
            TTS_output <- biphasic_params %>% full_join(single_params) %>%
                select(id, TTS, model, calculation)

        } else if (length(model_output$biphasicCI) > 0 & length(model_output$singleCI) == 0) {
            # Biphasic
            biphasic_params <- model_output$biphasicCI %>%
                select(-lowerCI, -upperCI) %>% spread(param, estimate) %>%
                mutate(TTS = get_parametricTTS(params = ., rootfunction = biphasic_root, suppression_threshold, uppertime),
                       model = "biphasic", calculation = "parametric")

            # All
            TTS_output <- biphasic_params %>% select(id, TTS, model, calculation)

        } else if (length(model_output$biphasicCI) == 0 & length(model_output$singleCI) > 0) {
            # Single phase
            single_params <- model_output$singleCI %>%
                select(-lowerCI, -upperCI) %>% spread(param, estimate) %>%
                mutate(TTS = get_parametricTTS(params = ., rootfunction = single_root, suppression_threshold, uppertime),
                       model = "single phase", calculation = "parametric")

            # All
            TTS_output <- single_params %>% select(id, TTS, model, calculation)
        }

    }

    # 2. Non-parametric TTS ----------------------------------------------------------------
    if(parametric == FALSE){

        if(is.null(data)){
            stop("Data not found. You must supply the data to calculate non-parametric TTS values")
        }

        # Check dataframe includes columns for 'id', 'time', 'vl'
        if(!(all(c("vl", "time", "id") %in% names(data)))){
            stop("Data frame must have named columns for 'id', 'time', and 'vl'")
        }

        # Filter out subjects to focus on those who reach suppression below the specified threshold.
        data_filtered <- filter_dataTTS(data, suppression_threshold, uppertime, censor_value, decline_buffer, initial_buffer)

        if( nrow(data_filtered) == 0){
            stop("No individual trajectories remained after filtering. Do you need to set the arguments used by filter_dataTTS? (see ?filter_dataTTS)")
        }

        TTS_output <- data_filtered %>%
            mutate(TTS = get_nonparametricTTS(vl, suppression_threshold, time, npoints)) %>%
            ungroup() %>% distinct(id, TTS) %>% mutate(calculation = "non-parametric")
    }

    if(ARTstart == TRUE){
        print("Calculating TTS as time since ART initiation...")

        if(is.null(data$ART)){
            print("Data frame is missing ART column. Returning original TTS values.")
        } else {
            ARTdata <- data %>% distinct(id, ART)

            TTS_output <- TTS_output %>% left_join(ARTdata) %>% mutate(TTS = TTS - ART)
        }
    }
    return(TTS_output)
}
