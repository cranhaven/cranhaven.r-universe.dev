wrap_2RM <- function(
  d, time_var = "Timestamp", output_epoch = "default",
  max_mets = 20, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  feature_calc = TRUE, shrink_output = TRUE, verbose = FALSE,
  method = c(
    "Crouter 2006", "Crouter 2010",
    "Crouter 2012", "Hibbing 2018"
  ),
  ..., met_name = "METs", tag = ""
) {


  ## Setup

    if (verbose) cat("\n...Getting predictions for the", method, "method")

    method <- match.arg(method)

    use_default <- is_default(output_epoch)

    if (use_default) {
      output_epoch <-
        lookup_epoch(method, "unique") %>%
        lubridate::period(.)
    }


  ## Automated feature calculation currently only applies
  ## to Hibbing method (and only for the non-IMU models)

    if (feature_calc & "Hibbing 2018" %in% method) {

      if (verbose) cat(
        "\n...Calculating 1-s features for the HIBBING 2018 method"
      )

      d %<>% generic_features(time_var, ...)

    }


  ## Get initial results

    results <-
      TwoRegression::TwoRegression(
        d, method, verbose = FALSE, time_var = time_var,
        max_mets = max_mets, warn_high_low = warn_high_low, ...
      ) %>%
      dplyr::select(
        dplyr::all_of(time_var),
        dplyr::any_of(c("ENMO", "GVM", "Direction")),
        dplyr::matches("CV10s"),
        dplyr::matches(met_name)
      ) %>%
      met_expand(
        met_name, tag, met_mlkgmin,
        -Inf, Inf, RER, warn_high_low
      )


  ## Process further if desired

    if (shrink_output) results %<>% dplyr::select(
      dplyr::all_of(time_var),
      dplyr::matches(tag)
    )

    return_vals(
      results, time_var,
      output_epoch, verbose,
      default_override = use_default
    )

}
