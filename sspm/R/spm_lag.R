#' Create lagged columns in a sspm smoothed data slot
#'
#' This function is a wrapper around [lag][dplyr::lag] (note that not all
#' arguments are supported). The default value for the lag is the mean of the
#' series.
#'
#' @inheritParams map_formula
#' @param vars **\[character\]** Names of the variables to lag.
#' @inheritParams dplyr::lag
#'
#' @return
#' Updated `sspm_object`.
#'
#' @examples
#' \dontrun{
#' sspm_model <- sspm_model %>%
#'     spm_lag(vars = c("weight_per_km2_borealis_with_catch",
#'                      "weight_per_km2_all_predators"),
#'                      n = 1)
#' }
#'
#' @export
setGeneric(name = "spm_lag",
           def = function(sspm_object,
                          vars,
                          n = 1,
                          default = "mean",
                          ...) {
             standardGeneric("spm_lag")
           }
)

# Methods -----------------------------------------------------------------

#' @export
#' @rdname spm_lag
setMethod(f = "spm_lag",
          signature(sspm_object = "sspm"),
          function(sspm_object, vars, n, default, ...) {

            smoothed_data <- spm_smoothed_data(sspm_object)
            bounds <- spm_boundaries(sspm_object)

            smoothed_data <- lag_data_frame(smoothed_data, bounds, vars, n, default, ...)

            spm_smoothed_data(sspm_object) <- smoothed_data
            return(sspm_object)
          }
)

#' @export
#' @rdname spm_lag
setMethod(f = "spm_lag",
          signature(sspm_object = "sspm_fit"),
          function(sspm_object, vars, n, default, ...) {

            smoothed_data <- spm_smoothed_data(sspm_object)
            bounds <- spm_boundaries(sspm_object)

            smoothed_data <- lag_data_frame(smoothed_data, bounds, vars, n, default, ...)

            spm_smoothed_data(sspm_object) <- smoothed_data
            return(sspm_object)
          }
)

# -------------------------------------------------------------------------

lag_data_frame <- function(smoothed_data, boundaries, vars, n, default, ...){

  for (var in vars) {

    if (var %in% colnames(smoothed_data)) {

      var_name <- paste0(var, "_lag_", n)

      if (is.character(default)) {

        if (default == "mean") {
          def_val <- mean(smoothed_data[[var]], na.rm = TRUE)
        } else {
          stop("Defaulting scheme not recognized")
        }

      } else {
        def_val <- default
      }

      # Calculate the lag, grouping by patch ID and boundaries
      smoothed_data <- smoothed_data %>%
        dplyr::group_by(.data$patch_id,
                        .data[[spm_boundary(boundaries)]]) %>%
        dplyr::mutate(!!var_name := dplyr::lag(x = .data[[var]],
                                               n = n, default = def_val, ...)) %>%
        dplyr::ungroup() %>%
        dplyr::relocate(dplyr::all_of(var_name), .after = dplyr::all_of(var))

    } else {

      cli::cli_alert_danger(paste0(" Column ", cli::col_magenta(var),
                                   " does not exist in smoothed data."))

    }
  }

  return(smoothed_data)
}
