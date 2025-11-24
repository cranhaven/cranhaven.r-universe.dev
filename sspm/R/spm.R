#' Fit an SPM model
#'
#' Fit an spm model to a sspm object
#'
#' @param sspm_object **\[sspm_dataset\]** An object of class
#'     [sspm_dataset][sspm_dataset-class].
#' @param formula **\[formula\]** A formula definition of the form
#'     response ~ smoothing_terms + ...
#' @inheritDotParams mgcv::bam
#'
#' @return
#' An object of type `sspm_fit`.
#'
#' @examples
#' \dontrun{
#' sspm_model_fit <- sspm_model %>%
#'     spm(log_productivity ~ sfa +
#'     weight_per_km2_all_predators_lag_1 +
#'     smooth_space(by = weight_per_km2_borealis_with_catch) +
#'     smooth_space(),
#'     family = mgcv::scat)
#' }
#'
#' @export
setGeneric(name = "spm",
           def = function(sspm_object,
                          formula,
                          ...) {
             standardGeneric("spm")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm
setMethod(f = "spm",
          signature(sspm_object = "sspm",
                    formula = "missing"),
          function(sspm_object, formula, ...) {
            cli::cli_alert_danger(" Argument 'formula' missing with no default")
          }
)

#' @export
#' @rdname spm
setMethod(f = "spm",
          signature(sspm_object = "sspm",
                    formula = "formula"),
          function(sspm_object, formula, ...) {

            # 1. Is there a splitting scheme?
            if (!is_split(sspm_object)) {
              stop("Data must be split with a test/train column.")
            }

            # 1. Get data
            all_data <- spm_smoothed_data(sspm_object)
            # train_data <- all_data %>%
            #   dplyr::filter(.data$train_test == TRUE)

            # 2. call map_formula
            time <- spm_time(sspm_object)
            boundaries <- spm_boundaries(sspm_object)

            # Pass onto the sspm_dataset method
            sspm_formula <- map_formula(data_frame = all_data,
                                        boundaries = boundaries,
                                        formula = formula,
                                        time = time,
                                        ...)

            # Call the fit function
            the_fit <- fit_spm(sspm_object = sspm_object,
                                sspm_formula = sspm_formula,
                                ...)

            sspm_fit <- new("sspm_fit",
                            smoothed_data = all_data,
                            time = spm_time(sspm_object),
                            uniqueID = spm_unique_ID(sspm_object),
                            formula = sspm_formula,
                            boundaries = spm_boundaries(sspm_object),
                            fit = the_fit)

            return(sspm_fit)

          }
)
