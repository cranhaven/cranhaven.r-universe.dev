# Linear ------------------------------------------------------------------

hildebrand_linear <- function(
  d, time_var = "Timestamp", output_epoch = "default",
  min_vo2_mlkgmin = 3, max_vo2_mlkgmin = 70, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  feature_calc = TRUE, shrink_output = TRUE, verbose = FALSE,
  age = c("youth", "adult"), monitor = c("ActiGraph", "GENEActiv"),
  location = c("hip", "wrist"), enmo_name = "ENMO", ...
) {

  if (verbose) cat(
    "\n...Getting predictions for the",
    " Hildebrand linear method"
  )

  age %<>% hildebrand_input("age", c("youth", "adult"))
  monitor %<>% hildebrand_input("monitor", c("actigraph", "geneactiv"))
  location %<>% hildebrand_input("location", c("hip", "wrist"))

  if (feature_calc) {

    if (verbose) cat(
      "\n...Calculating 1-s features for the HILDEBRAND LINEAR method"
    )

    d %<>% generic_features(time_var, ...)

  }

  results <-

    .hildebrand %>%
    dplyr::filter(
      tolower(.age) %in% age,
      tolower(.monitor) %in% monitor,
      tolower(.location) %in% location
    ) %>%
    split(., 1:nrow(.)) %>%

    lapply(

      function(
        x, .data, enmo_name, time_var,
        min_vo2_mlkgmin, max_vo2_mlkgmin, warn_high_low,
        met_mlkgmin, RER
      ) {

        ## VO2 (ml/kg/min)
        ifelse(
          .data[[enmo_name]] <= x$cp,
          min_vo2_mlkgmin,
          .data[[enmo_name]] * x$slope + x$intercept
        ) %>%

        ## More variables
        vo2_expand(
          .data,
          paste("hildebrand_linear", x$.age, x$.monitor, x$.location, sep = "_"),
          time_var,
          min_vo2_mlkgmin,
          max_vo2_mlkgmin,
          warn_high_low,
          met_mlkgmin,
          RER
        )

      },

      .data = d, enmo_name = enmo_name, time_var = time_var,
      min_vo2_mlkgmin = min_vo2_mlkgmin,
      max_vo2_mlkgmin = max_vo2_mlkgmin,
      warn_high_low = warn_high_low,
      met_mlkgmin = met_mlkgmin,
      RER = RER

    ) %>%

    c(.name_repair = "minimal") %>%
    do.call(dplyr::bind_cols, .) %>%
    df_unique(.)

  if (!shrink_output) {

    stopifnot(abs(nrow(d) - nrow(results)) <= 1)

    results %<>%
      dplyr::bind_cols(d, ., .name_repair = "minimal") %>%
      df_unique(.)

  }

  return_vals(
    results, time_var,
    output_epoch, verbose
  )


}


# Non-Linear --------------------------------------------------------------

hildebrand_nonlinear <- function(
  d, time_var = "Timestamp", output_epoch = "default",
  min_vo2_mlkgmin = 3, max_vo2_mlkgmin = 70, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  feature_calc = TRUE, shrink_output = TRUE,
  verbose = FALSE, enmo_name = "ENMO", ...
) {

  if (verbose) cat(
    "\n...Getting predictions for the",
    " Hildebrand non-linear method"
  )


  if (feature_calc) {

    if (verbose) cat(
      "\n...Calculating 1-s features for the HILDEBRAND NON-LINEAR method"
    )

    d %<>% generic_features(time_var, ...)

  }

  results <-
    d[[enmo_name]] %>%
    {. ^ .534} %>%
    {0.901 * .} %>%
    vo2_expand(
      d, "hildebrand_nonlinear", time_var,
      min_vo2_mlkgmin, max_vo2_mlkgmin,
      warn_high_low, met_mlkgmin, RER
    )

  if (!shrink_output) {

    stopifnot(abs(nrow(d) - nrow(results)) <= 1)

    results %<>%
      dplyr::bind_cols(d, ., .name_repair = "minimal") %>%
      df_unique(.)

  }

  return_vals(
    results, time_var,
    output_epoch, verbose
  )

}


# Helper ------------------------------------------------------------------

hildebrand_input <- function(value, arg, choices) {

  if (is.null(value) | length(value) == 0) stop(
    "Must pass value(s) for ", arg, call. = FALSE
  )

  value %<>%
    tolower(.) %>%
    unique(.)

  if (!any(value %in% choices)) stop(
    "Must pass a valid value for ", arg, ". Options are: ",
    paste(choices, collapse = ", "), call. = FALSE
  )

  if (!all(value %in% choices)) {
    bad <- setdiff(value, choices)
    value %<>% intersect(choices)
    warning(
      "Removing the following invalid value(s) passed for ",
      arg, ": ", paste(bad, collapse = ", "),
      "\nRetaining these: ", paste(value, collapse = ", "),
      "\nOptions are: ", paste(choices, collapse = ", "),
      call. = FALSE
    )
  }

  value

}
