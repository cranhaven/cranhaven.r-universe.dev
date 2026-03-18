#' Tests custom incidence models
#'
#' Runs checks to assess whether a custom incidence model is
#' suitable for use in \code{prevalence}. Provides useful
#' diagnostic messages if any issues are encountered.
#'
#' @param object An incidence model to be tested
#' @param data Registry data in the form of a data frame. Ideally
#'   will be the same source that will be used for the prevalence
#'   estimation later on.
#' @param timeframe How long to generate incident cases for in days.
#'   This is disease-specific, but the default of ten years
#'   should work well for most diseases.
#'
#' @return The dummy incident population that has been generated to allow
#' for further diagnostics to be run.
#'
#' @export
validate_incidence_model <- function(object, data, timeframe=3652) {

    # Check has required attributes
    if (is.null(object$call)) {
        stop("Error: object does not have a call attribute.")
    }
    if (is.null(class(object)) | (length(class(object)) == 1 & class(object)[1] == 'list')) {
        stop("Error: object does not have a unique class")
    }

    # Try and fit model to data, check no errors
    tryCatch(eval(object$call),
             error=function(e) {
                 stop("Error: Can't fit model to new data. ", e)
             })

    # Test has method "draw_incident_population"
    if (!any(sapply(paste("draw_incident_population", class(object), sep='.'), exists))) {
        stop("Error: cannot find draw_incident_population method for object of class ", class(object))
    }

    # Draw an incident population and test its output
    tryCatch({
        inc_pop <- draw_incident_population(object, data, timeframe, covars=NULL)
        if (!class(inc_pop) %in% c("data.frame", "data.table")) {
            message("Error: Output class of draw_incident_population is not data.frame or data.table")
        }
        if (nrow(inc_pop) == 0) {
            message("Error: No incident individuals were created.")
        }
        if ((nrow(inc_pop) / timeframe)*DAYS_IN_YEAR < 10) {
            message("Warning: Low incidence encountered, less than 10 cases a year.")
        }
        if (!typeof(inc_pop[[1]]) %in% c('double', 'integer')) {
            message("Error: The first column is not numeric (either double or integer) as the entry time should be.")
        }
        if (any(inc_pop[[1]] <= 0)) {
            message("Error: Some entry values are less than or equal to zero.")
        }
        if (any(inc_pop[[1]] > timeframe)) {
            message("Error: Some entry values are greater than the timeframe permitted.")
        }},
        error=function(e) {
            stop("Error encountered: ", e)
        })
    message("Incidence model passed all tests.")
    inc_pop
}

#' Tests that a custom survival object has the required attributes
#' for use in the \code{prevalence} function.
#'
#' Runs checks to assess whether a custom survival model is
#' suitable for use in \code{prevalence}. Provides useful
#' diagnostic messages if any issues are encountered.
#'
#' @param object The custom survival object.
#' @param data Registry data in the form of a data frame. Ideally
#'   will be the same source that will be used for the prevalence
#'   estimation later on.
#' @param timeframe Maximum time at which to test survival probability in days.
#' If not supplied then chooses random values over a period of 10 years, which
#' should be suitable for many diseases.
#' @param sample_size The number of randomly drawn individuals to predict
#'   sample size for.
#'
#' @return None. Instead, messages get displayed to the
#' console.
#'
#' @export
validate_survival_model <- function(object, data, timeframe=3652, sample_size=10) {
    # Check has required attributes
    if (is.null(object$call)) {
        stop("Error: object does not have a call attribute.")
    }
    if (is.null(class(object)) | (length(class(object)) == 1 & class(object)[1] == 'list')) {
        stop("Error: object does not have a unique class")
    }

    # Try and fit model to data, check no errors
    tryCatch(eval(object$call),
             error=function(e) {
                 stop("Error: Can't fit model to new data. ", e)
             })

    # Test has method "extract_covars"
    if (!any(sapply(paste("extract_covars", class(object), sep='.'), exists))) {
        stop("Error: cannot find extract_covars method for object of class ", class(object))
    }

    # Check that output of extract covars are in the same data frame
    tryCatch({
        covars <- extract_covars(object)
        if (!is.null(covars)) {
            if (typeof(covars) != 'character') {
                stop("Error: extract_covars is returning a non-character vector: ", covars)
            }
            if (!covars %in% colnames(data)) {
                stop("Error: cannot find results of extract_covars (", covars, ") as columns
                     in registry data.")
            }
        }},
        error=function(e) {
            stop("Error encountered: ", e)
        })

    # Test has method "predict_survival_probability"
    if (!any(sapply(paste("predict_survival_probability", class(object), sep='.'), exists))) {
        stop("Error: cannot find predict_survival_probabilit method for object of class ", class(object))
    }

    tryCatch({
        # Run predict_survival_probability with a small subset of the
        # data
        sub_data <- data[sample(1:nrow(data), sample_size), ]
        times <- runif(sample_size, 1, timeframe)
        preds <- predict_survival_probability(object, newdata=sub_data, times = times)
        if (is.null(preds)) {
            stop("Error: no survival predictions were output from predict_survival_probability.")
        }
        if (length(preds) != sample_size) {
            stop("Error: Do not have as many survival probability estimates as number of individuals in newdata and times.")
        }
        if (any(preds < 0 | preds > 1)) {
            stop("Error: Found survival probabilities outside of the interval [0,1].")
        }

        # Check for monotonically decreasing
        nmono <- 50
        mono_df <- data[rep(sample(1:nrow(data), 1), nmono), ]
        mono_times <- sort(runif(nmono, 1, timeframe))
        mono_preds <- predict_survival_probability(object, newdata=mono_df, times = mono_times)
        if (! all(mono_preds == cummin(mono_preds))) {
            stop("Error: survival probabilities are not monotonically decreasing.")
        }
        },
        error=function(e) {
            stop("Error encountered: ", e)
        })
    message("Survival model passed all tests.")
    preds
}
