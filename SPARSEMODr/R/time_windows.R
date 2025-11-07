#' Similar to a named list but with custom error checking and data manipulation for the SPARSEMODr model.
#'
#' @author Seth Borkovec 2020 (revised June 2021)
#'
#' @param beta A numeric vector of beta values. Optionally a list of beta numeric vectors--one for every population as used in the control.
#' @param dist_phi A numeric vector of distance parameters.
#' @param m A numeric vector of movement frequencies.
#' @param imm_frac A numeric vector for immigration fractions.
#' @param window_length An integer vector of the number of days in the time window period.
#' @param start_dates A vector of Date objects indicating the start date for the time window.
#' @param end_dates A vector of Date objects indicating the end date for the time window.
#' @param daily A vector of Date objects for daily input data instead of time windows.
#' @return A time_windows object. Data can be extracted from the object as a named list of vectors.
#' @examples
#' tw <- time_windows(r0=input_r0, dist_phi=input_dist_phi, m=input_m, imm_frac=input_imm_frac, window_length=input_window_length)
#' tw <- time_windows(input_r0, input_dist_phi, input_m, input_imm_frac, input_window_length)
#' tw <- time_windows(r0=input_r0, dist_phi=input_dist_phi, m=input_m, imm_frac=input_imm_frac, start_dates=input_start_dates, end_dates=input_end_dates)
#' tw <- time_windows(r0=input_r0, dist_phi=input_dist_phi, m=input_m, imm_frac=input_imm_frac, daily=input_daily)



# he number of populations must be
# equal to the number of populations used in the covid19_control or seir_control.
time_windows <- function(beta=NULL,
                         dist_phi=NULL,
                         m=NULL,
                         imm_frac=NULL,
                         hosp_rate=NULL,
                         recov_hosp=NULL,
                         icu_rate=NULL,
                         death_rate=NULL,
                         window_length=NULL,
                         start_dates=NULL,
                         end_dates=NULL,
                         daily=NULL,
                         r0 = NULL)
{
    # Check if data is Date type
    is.Date <- function(obj) inherits(obj, "Date")
    # Validate deprecated:
    if (!is.null(r0)) stop("Parameter r0 is not supported in this version. You should use beta. See manual.")
    # Validate required arguments are not NULL
    if (is.null(beta)) stop("Parameter beta cannot be omitted.")
    if (is.null(dist_phi)) stop("Parameter dist_phi cannot be omitted.")
    if (is.null(m)) stop("Parameter m cannot be omitted.")
    if (is.null(imm_frac)) stop("Parameter imm_frac cannot be omitted.")

    # Allows users to provide a single beta vector
    if (!is.list(beta)) {
        temp_beta <- beta
        beta <- list()
        beta[[1]] <- temp_beta
    }

    # Number of populations
    n_pop <- length(beta)

    # Number of entries
    total_windows <- length(beta[[1]])

    # Validate that only one date input option was chosen
    if (is.null(window_length) && is.null(start_dates) && is.null(daily)) stop("You must provide one of the following options: window_length, start_dates with end_dates, or daily.")
    if ((!is.null(window_length) && (!is.null(start_dates) || !is.null(end_dates) || !is.null(daily))) ||
        (!is.null(daily) && (!is.null(start_dates) || !is.null(end_dates) || !is.null(window_length))) ||
        (!is.null(start_dates) && (!is.null(window_length) || !is.null(daily))))
    {
        stop("You may provide only one of the following options: window_length, start_dates with end_dates, or daily.")
    }
    if (!is.null(start_dates) && is.null(end_dates)) stop("You must provide end_dates with start_dates.")

    # Validate that hosp_rate, recov_hosp, icu_rate, death_rate are only provided for daily windows
    if (is.null(daily) && (!is.null(hosp_rate) || !is.null(recov_hosp) || !is.null(icu_rate) || !is.null(death_rate))) stop("You may only provide hosp_rate, recov_hosp, icu_rate, death_rate when using daily time windows.")

    # Validate that the lengths match and contain valid data
    if (!all(dist_phi >= 0)) stop("Values of dist_phi must be greater than or equal to zero.")
    if (!all(m >= 0)) stop("Values of m must be greater than or equal to zero.")
    if (!all(imm_frac >= 0)) stop("Values of imm_frac must be greater than zero.")
    if (!all(imm_frac <= 1)) stop("Values of imm_frac must be less than or equal to one.")
    if (total_windows != length(dist_phi)) stop("Length of beta does not match length of dist_phi.")
    if (total_windows != length(m)) stop("Length of beta does not match length of m.")
    if (total_windows != length(imm_frac)) stop("Length of beta does not match length of imm_frac.")
    if (!is.null(window_length) && (total_windows != length(window_length))) stop("Length of beta does not match length of window_length.")

    # Validate that all of beta vectors are the same length and have valid data
    for (this_pop in 1:n_pop) {
        if (!all(beta[[this_pop]] >= 0)) stop("Values of beta must be greater than or equal to zero.")
        if (length(beta[[this_pop]]) != total_windows) {stop("The lengths of beta in each population must be the same.")}
    }

    if (!is.null(start_dates))
    {
        if (!is.Date(start_dates)) stop("Vector start_dates does not contain valid Dates.")
        if (!is.Date(end_dates)) stop("Vector end_dates does not contain valid Dates.")
        if (length(start_dates) != length(end_dates)) stop("The lengths of start_dates and end_dates do not match.")
        if (total_windows != length(start_dates)) stop("Length of beta does not match the length of start_dates and end_dates.")
    }
    if (!is.null(daily))
    {
        if (!is.Date(daily)) stop("Vector daily does not contain valid Dates.")
        if (total_windows != length(daily)) stop("Length of beta does not match length of daily.")
    }

    # Calculate window_lengths if needed
    if (!is.null(daily))
    {
        # Validate no overlap, no gaps, and is sequential
        for (index in 2:length(daily))
        {
            difference <- as.numeric(daily[index] - daily[index - 1])
            if (difference == 0) stop(paste("Daily entry at daily[", index, "] overlaps with the previous entry."))
            if (difference > 1) stop(paste("There is a gap between daily[", index, "] and daily[", (index - 1), "]."))
            if (difference < 0) stop(paste("The entry at daily[", index, "] is not sequential."))
        }
        window_length <- c(rep(1, length(daily)))
    }
    if (!is.null(start_dates))
    {
        for (index in 1:length(start_dates))
        {
            # Validate no overlap, no gaps, and is sequential
            if (index > 1)
            {
                difference <- as.numeric(start_dates[index] - end_dates[index - 1])
                if (difference > 1) stop(paste("There is a gap between start_dates[", index, "] and end_dates[", (index - 1), "]."))
                if (difference < 1) stop(paste("The entry at start_dates[", index, "] overlaps with the previous entry."))
            }
            window_length <- append(window_length, as.numeric(end_dates[index] - start_dates[index] + 1))
        }
    }

    # Function to verify that parameter is within (0,1]
    checkIfInZeroToOne <- function(parameter, name)
    {
        if ((!all(parameter > 0)) || (!all(parameter <= 1))) stop(paste("Error: The values of ", name, " must be within the range (0, 1]."))
    }

    # Function to verify the parameter is not negative. Warning for greater than one
    checkIfGreaterThanZero <- function(parameter, name)
    {
        if (!all(parameter >= 0)) stop(paste0("Error: The value of ", name, " must be greater than or equal to zero."))
        if (!all(parameter <= 1)) warning(paste0("Warning: The value of ", name, ", is greater than one and unusual. Are you sure you want this?"))
    }

    # Set hosp_rate, recov_hosp, icu_rate, death_rate if not provided
    if (is.null(hosp_rate)) {
        hosp_rate <- rep(0.175, total_windows)
    } else {
        if (total_windows != length(hosp_rate)) stop("Length of beta does not match length of hosp_rate.")
        checkIfInZeroToOne(hosp_rate, "hosp_rate")
    }
    if (is.null(recov_hosp)) {
        recov_hosp <- rep(1/7.0, total_windows)
    } else {
        if (total_windows != length(recov_hosp)) stop("Length of beta does not match length of recov_hosp.")
        checkIfGreaterThanZero(recov_hosp, "recov_hosp")
    }
    if (is.null(icu_rate)) {
        icu_rate <- rep(0.20, total_windows)
    } else {
        if (total_windows != length(icu_rate)) stop("Length of beta does not match length of icu_rate.")
        checkIfInZeroToOne(icu_rate, "icu_rate")
    }
    if (is.null(death_rate)) {
        death_rate <- rep(0.60, total_windows)
    } else {
        if (total_windows != length(death_rate)) stop("Length of beta does not match length of death_rate.")
        checkIfInZeroToOne(death_rate, "death_rate")
    }

    # Automatic calculation for total_windows and t_max
    t_max <- sum(window_length)

    # Assign the values to the class fields
    value <- list(beta = beta,
                  dist_phi = dist_phi,
                  m = m,
                  imm_frac = imm_frac,
                  hosp_rate = hosp_rate,
                  recov_hosp = recov_hosp,
                  icu_rate = icu_rate,
                  death_rate = death_rate,
                  window_length = window_length,
                  total_windows = total_windows,
                  t_max = t_max)

    # Create the S3 class
    attr(value, "class") <- "time_windows"
    value
}
