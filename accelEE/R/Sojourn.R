sojourn <- function(
  d,  time_var = "Timestamp", output_epoch = "default",
  min_mets = 1, max_mets = 20, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  shrink_output = TRUE, verbose = FALSE,
  axis1 = "Axis1", axis2 = "Axis2", axis3 = "Axis3",
  vector.magnitude = "Vector.Magnitude",
  method = c("SIP", "Sojourn 1x", "Sojourn 3x"),
  ..., met_name = "METs", tag = ""
) {

  ## Setup

    method <- match.arg(method)
    if (verbose) cat("\n...Getting predictions for the", method, "method")

  ## Get initial results

    results <-
      switch(
        method,
        "SIP" =
          Sojourn::sojourn_3x_SIP(d, ...) %>%
          met_expand(
            met_name, tag, met_mlkgmin,
            min_mets, max_mets, RER, warn_high_low
          ),
        "Sojourn 1x" =
          Sojourn::soj_1x_original(d[[axis1]], ...) %>%
          dplyr::mutate(!!as.name(time_var) := d[[time_var]]) %>%
          met_expand(
            met_name, tag, met_mlkgmin,
            min_mets, max_mets, RER, warn_high_low
          ),
        "Sojourn 3x" =
          Sojourn::soj_3x_original(
            d[[axis1]], d[[axis2]], d[[axis3]], d[[vector.magnitude]], ...
          ) %>%
          dplyr::mutate(!!as.name(time_var) := d[[time_var]]) %>%
          met_expand(
            met_name, tag, met_mlkgmin,
            min_mets, max_mets, RER, warn_high_low
          ),
        stop(
          "Failed to find a Sojourn method corresponding with the",
          " provided value (", method, ")", call. = FALSE
        )
      )

  ## Process further if desired

    if (method == "SIP") time_var <- "Timestamp"

    if (shrink_output) results %<>% dplyr::select(
      dplyr::all_of(time_var),
      dplyr::matches(tag)
    )

    return_vals(
      results, time_var,
      output_epoch, verbose
    )


}
