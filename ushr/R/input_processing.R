#' Prepare input data
#'
#' This function prepares the raw input data for model fitting.
#'
#' Steps include:
#' 1. Setting values below the detection threshold to half the detection threshold (following standard practice).
#' 2. Filtering out subjects who do not suppress viral load below the detection threshold by a certain time.
#' 3. Filtering out subjects who do not have a decreasing sequence of viral load (within some buffer range).
#' 4. Filtering out subjects who do not have enough data for model fitting.
#' 5. Removing the last data point of subjects with the last two points very close to the detection threshold. This prevents skewing of the model fit.
#' Further details can be found in the Vignette.
#' @param data raw data set. Must be a data frame with the following columns: 'id' - stating the unique identifier for each subject; 'vl' - numeric vector with the viral load measurements for each subject; 'time' - numeric vector of the times at which each measurement was taken.
#' @param detection_threshold numeric value indicating the detection threshold of the assay used to measure viral load. Measurements below this value will be assumed to represent undetectable viral load levels. Default value is 20.
#' @param censortime numeric value indicating the maximum time point to include in the analysis. Subjects who do not suppress viral load below the detection threshold within this time will be discarded. Units are assumed to be the same as the 'time' column. Default value is 365.
#' @param censor_value positive numeric value indicating the maximum time point to include in the analysis. Subjects who do not suppress viral load below the detection threshold within this time will be discarded. Units are assumed to be the same as the 'time' column. Default value is 365.
#' @param decline_buffer numeric value indicating the value assigned to measurements below the detection threshold. Must be less than or equal to the detection threshold.
#' @param initial_buffer numeric (integer) value indicating the maximum number of initial observations from which the beginning of each trajectory will be chosen. Default value is 3.
#' @param n_min_single numeric value indicating the minimum number of data points required to be included in the analysis. Defaults to 3. It is highly advised not to go below this threshold.
#' @param threshold_buffer numerical value indicating the range above the detection threshold which represents potential skewing of model fits. Subjects with their last two data points within this range will have the last point removed. Default value is 10.
#' @param nsuppression numerical value (1 or 2) indicating whether suppression is defined as having one observation below the detection threshold, or two sustained observations. Default value is 1.
#' @import dplyr
#' @return data frame of individuals whose viral load trajectories meet the criteria for model fitting. Includes columns for 'id', 'vl', and 'time'.
#' @export
#' @examples
#'
#' set.seed(1234567)
#'
#' simulated_data <- simulate_data(nsubjects = 20)
#'
#' filter_data(simulated_data)
#'
filter_data <- function(data, detection_threshold = 20,
                        censortime = 365, censor_value = 10,
                        decline_buffer = 500, initial_buffer = 3,
                        n_min_single = 3, threshold_buffer = 10, nsuppression = 1){

    # Check that data frame includes columns for 'id', 'time', 'vl'
    if (!(all(c("vl", "time", "id") %in% names(data)))) {
        stop("'data' must be a data frame with named columns for 'id', 'time', and 'vl'")
    }

    if (!is.numeric(data$time)) {
        stop("Column for the time of observations ('time') must be numeric")
    }

    if (is.factor(data$id)) {
        data$id <- as.character(data$id)
    }

    if (any(is.na(data$id))) {
        warning("Some subjects have missing IDs; removing these from the data")

        data <- data %>% filter(!is.na(id))
    }

    if (censor_value > detection_threshold) {
        warning("censor_value must be less than or equal to the detection threshold. Defaulting to half the detection threshold.")

        censor_value <- 0.5 * detection_threshold
    }

    if (censor_value < 0) {
        warning("censor_value must be positive. Defaulting to half the detection threshold.")

        censor_value <- 0.5 * detection_threshold
    }


    if (!(nsuppression %in% c(1,2))) {
        warning("nsuppression must take the numeric value 1 or 2 to define the criteria for reaching suppression; reverting to default nsuppression = 1")

        nsuppression <- 1
    }

    if (nsuppression == 1) {
        # 1. Change everything <= detection_threshhold to 1/2 * detection_threshhold
        data_filtered <- data %>% mutate(vl = case_when(vl <= detection_threshold ~ censor_value,
                                                        vl >= detection_threshold ~ vl) ) %>%
            # 2. Look at only those who reach control within user defined censortime
            filter(time <= censortime) %>% group_by(id) %>%
            filter(any(vl <= detection_threshold)) %>% ungroup() %>%
            filter(!is.na(vl))

    } else if (nsuppression == 2) {
        data_filtered <- data %>% mutate(vl = case_when(vl <= detection_threshold ~ censor_value,
                                                        vl >= detection_threshold ~ vl) ) %>%
            filter(!is.na(vl)) %>%
            filter(time <= censortime) %>% group_by(id) %>%
            # NOW: must have 2 consecutive measurements below threshold
            mutate(firstbelow = intersect(which(vl <= detection_threshold),
                                          which(vl <= detection_threshold) + 1)[1] - 1 ) %>%
            mutate(firstbelow = time[firstbelow]) %>%
            filter(time <= firstbelow) %>% ungroup()
    }

    # 3a. Isolate data from the highest VL measurement (from points 1 - 3) to the first point below detection
    if (nrow(data_filtered) > 0) {
        data_filtered <- data_filtered %>% group_by(id) %>%
            slice(which.max(vl[1:initial_buffer]):Position(function(x) x <= detection_threshold, vl)) %>%
            ungroup() %>%
            # 3b. Only keep VL sequences that are decreasing with user defined buffer...
            group_by(id) %>% filter(all(vl <= cummin(vl) + decline_buffer)) %>%
            # 4. ...AND have min # dps above the detection threshold
            filter(length(vl[vl > detection_threshold]) >= n_min_single)
    }

    # 5. Remove last data point of subjects with last two points very close to the threshold to prevent skewing model fit
    if (nrow(data_filtered) > 0) {
        data_filtered <- data_filtered %>% group_by(id) %>%
            mutate(n = n(), index = 1:n(),
                   tag = ifelse(vl[n-1] - vl[n] < threshold_buffer, TRUE, FALSE) ) %>%
            filter(!(tag == TRUE & index == n)) %>%
            ungroup() %>% select(- index, -n, -tag)
}
    return(data_filtered)
}
