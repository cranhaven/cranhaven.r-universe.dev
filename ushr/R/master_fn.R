#' Master function
#'
#' This function performs the entire analysis, from data filtering to fitting the biphasic/single phase models. The biphasic/single phase models should be used when ART comprises of RTI/PIs.
#'
#' Steps include:
#' 1. Processing the raw data.
#' 2. Fitting the biphasic model to subjects with eligible data e.g. those with enough data points and reliable confidence interval estimates.
#' 3. Fitting the single phase model to the remaining subjects.
#' @param filter Logical TRUE/FALSE indicating whether the data should be processed (highly recommended) prior to model fitting. Default is TRUE.
#' @param data raw data set. Must be a data frame with the following columns: 'id' - stating the unique identifier for each subject; 'vl' - numeric vector stating the viral load measurements for each subject; 'time'- numeric vector stating the time at which each measurement was taken.
#' @param detection_threshold numeric value indicating the detection threshold of the assay used to measure viral load. Measurements below this value will be assumed to represent undetectable viral levels. Default value is 20.
#' @param censortime numeric value indicating the maximum time point to include in the analysis. Subjects who do not suppress viral load below the detection threshold within this time will be discarded from model fitting. Units are assumed to be same as the 'time' measurements. Default value is 365.
#' @param censor_value positive numeric value indicating the maximum time point to include in the analysis. Subjects who do not suppress viral load below the detection threshold within this time will be discarded. Units are assumed to be the same as the 'time' column. Default value is 365.
#' @param decline_buffer numeric value indicating the maximum allowable deviation of values away from a strictly decreasing sequence in viral load. This allows for e.g. measurement noise and small fluctuations in viral load. Default value is 500.
#' @param initial_buffer integer value indicating the maximum number of initial observations from which the beginning of each trajectory will be chosen. Default value is 3.
#' @param threshold_buffer numeric value indicating the range above the detection threshold which represents potential skewing of model fits. Subjects with their last two data points within this range will have the last point removed. Default value is 10.
#' @param VL_max_decline numeric value indicating the maximum allowable difference between first and second viral load measurements. Default is 10,000.
#' @param CI_max_diff numeric value indicating the maximum allowable relative difference between lower and upper 95\% confidence intervals i.e. (upper CI - lower CI)/lower CI. Default is 1000.
#' @param n_min_single numeric value indicating the minimum number of data points required to be included in the analysis. Defaults to 3. It is highly advised not to go below this threshold.
#' @param n_min_biphasic numeric value indicating the minimum number of data points required to fit the biphasic model. Defaults to 6. It is highly advised not to go below this threshold.
#' @param nsuppression numerical value (1 or 2) indicating whether suppression is defined as having one observation below the detection threshold, or two sustained observations. Default value is 1.
#' @param forward_param_transform_fn list of transformation functions to be used when fitting the model in optim. Defaults to log transformations for all parameters (to allow unconstrained optimization).
#' @param inv_param_transform_fn list of transformation functions to be used when back-transforming the transformed parameters. Should be the inverse of the forward transformation functions. Defaults to exponential.
#' @param initial_params named numeric vector of initial parameter guesses. Defaults to c(A = 10000, delta = 0.68, B = 1000, gamma = 0.03).
#' @param searchmethod optimization algorithm to be passed to 'optim()'. Defaults to 'Nelder-Mead'.
#' @return a list containing the filtered data ('data_filtered'); parameter estimates for the biphasic and single phase models ('biphasicCI' and 'singleCI'); and predictions from the biphasic and single phase models ('biphasic_fits' and 'single_fits').
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' model_output <- ushr(data = simulated_data)
#'
ushr <- function(data,
                           ## User-defined variables
                           filter = TRUE,
                           detection_threshold = 20,
                           censortime = 365,
                           censor_value = 10,
                           decline_buffer = 500,
                           initial_buffer = 3,
                           threshold_buffer = 10,
                           VL_max_decline = 1e4,
                           CI_max_diff = 1e3,
                           n_min_single = 3,
                           n_min_biphasic = 6,
                           nsuppression = 1,
                           #  Parameter tranformations for optimizer
                           forward_param_transform_fn = list(log, log, log, log),
                           inv_param_transform_fn = list(exp, exp, exp, exp),
                           ## User defined fitting variables:
                           initial_params = c(A = 10000, delta = 0.68, B = 1000, gamma = 0.03),
                           searchmethod = "Nelder-Mead"){

    if (!is.data.frame(data)) {
        stop("Input 'data' must be a data frame")
    }

    if (!is.numeric(c(detection_threshold, censortime, decline_buffer, initial_buffer, n_min_single, n_min_biphasic,
              threshold_buffer, VL_max_decline, CI_max_diff, nsuppression) )) {
        stop("The following arguments must be numeric: detection_threshold, censortime, decline_buffer, initial_buffer,
              n_min_single, n_min_biphasic, threshold_buffer, VL_max_decline, CI_max_diff, nsuppression")
    }

    if (floor(initial_buffer) != initial_buffer) {
        initial_buffer <- floor(initial_buffer)
        warning(paste0("initial_buffer must be a whole number: rounding down to ", floor(initial_buffer)))
    }

    ## 1. Data processing  ----------------------------------------------------------------
    if (filter) {
        data_filtered <- filter_data(data, detection_threshold, censortime, censor_value,
                                     decline_buffer, initial_buffer, n_min_single, threshold_buffer, nsuppression)
    } else {
        data_filtered <- data
    }

    # Number of subjects after filtering
    id_filtered = unique(data_filtered$id)
     if (length(id_filtered) == 0) {
         stop("No subjects were suitable for model fitting after the data was filtered.")
     }

    # Get transformed parameters -----------------------------------------
    transformed_params <- get_transformed_params(params = initial_params,
                                                 param_transform_fn = forward_param_transform_fn)
    param_names <- names(initial_params)

    # 2. Fit biphasic model ------------------------------------------
    free_param_index <- c(TRUE, TRUE, TRUE, TRUE)

    biphasicmodel <- fit_model(data = data_filtered, id_vector = id_filtered, param_names = param_names,
                               initial_params = initial_params, free_param_index = free_param_index,
                               n_min_biphasic = n_min_biphasic,
                               model_list = "four", whichcurve = get_biphasic,
                               forward_param_transform_fn = forward_param_transform_fn,
                               inv_param_transform_fn = inv_param_transform_fn,
                               searchmethod = searchmethod)

    if (nrow(biphasicmodel$fitted) > 0) {
        biphasicCI <- get_CItable(biphasicmodel$CIlist, param_names, free_param_index, fitted = biphasicmodel$fitted)

        # Flag subjects with unreliable CIs i.e. if at least one of the relative CI ranges is > CI_max_diff
        badCI <- biphasicCI %>% filter(relativerange > CI_max_diff) %>% distinct(id)

        biphasicCI <- biphasicCI %>% filter(!(id %in% badCI$id)) %>% select(id, param, estimate, lowerCI, upperCI)

        if (nrow(biphasicCI) > 0) {
            #biphasicCI <- switch_params(biphasicCI)
            biphasicCI <- biphasicCI %>% group_by(id) %>% do(switch_params(.)) %>% ungroup()
        }

        biphasic_fits <- bind_rows(biphasicmodel$model_fitlist) %>% filter(!(id %in% badCI$id))
    } else {
        biphasicCI <- data.frame()
        badCI <- c()
        biphasic_fits <- data.frame()
    }

    # 3. Fit single phase for the remaining ids ------------------------------------------

    # Choose subjects (include biphasic fits with unreliable CIs)
    single_ids <- sort(union(biphasicmodel$notfitted$id, badCI$id))

    # Take out first datapoint for ids with sharp initial decline i.e. with VL diff > VL_max_decline
    # (suggests fast first phase with too few data points to model)

    cutdat <- data_filtered %>% select(id, time, vl) %>%
        arrange(id, time) %>% filter(id %in% single_ids) %>%
        group_by(id) %>% slice(1:2) %>% mutate(vl_diff = max(vl) - min(vl)) %>%
        filter(vl_diff > VL_max_decline) %>% distinct(id, vl_diff)

    data_cut <- data_filtered %>% group_by(id) %>% do(remove_vl0(.$id, which_ids = cutdat$id, .)) %>% ungroup()

    # Fit model on remaining data
    free_param_index <- c(FALSE, FALSE, TRUE, TRUE)

    if (length(single_ids) > 0) {
        singlemodel <- fit_model(data = data_cut, id_vector = single_ids, param_names = param_names,
                                 initial_params = initial_params, free_param_index = free_param_index,
                                 n_min_biphasic = n_min_biphasic,
                                 model_list = "two", whichcurve = get_singlephase,
                                 forward_param_transform_fn = forward_param_transform_fn,
                                 inv_param_transform_fn = inv_param_transform_fn,
                                 searchmethod = searchmethod)

        single_fits <-  bind_rows(singlemodel$model_fitlist)

        singleCI <- get_CItable(singlemodel$CIlist, param_names, free_param_index, fitted = singlemodel$fitted) %>%
            select(id, param, estimate, lowerCI, upperCI) %>%
            mutate(param = ifelse(param == "B", "Bhat", param),
                   param = ifelse(param == "gamma", "gammahat", param))
    } else {
        single_fits <- data.frame()
        singleCI <- data.frame()
    }


    output <- list(data_filtered = data_cut,
                   biphasicCI = biphasicCI, singleCI = singleCI,
                   biphasic_fits = biphasic_fits, single_fits = single_fits)

    return(output)
}


