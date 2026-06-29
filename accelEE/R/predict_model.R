predict_model <- function(
  d, out_name, model, label,
  min_mets = 1, max_mets = 20,
  warn_high_low = TRUE
) {

  stopifnot(
    inherits(d, "data.frame")
  )

  d %>%
  dplyr::mutate(
    !!as.name(out_name) := check_values(
      stats::predict(model, newdata = d),
      min_mets, max_mets, label,
      "MET", "MET(s)", warn_high_low
    )
  )

}


check_values <- function(
  x, minimum, maximum, label,
  variable = c("MET", "VO2"),
  units = c("MET(s)", "ml/kg/min"),
  warn_high_low = TRUE
) {

  ## Setup

    variable <- match.arg(variable)
    units <- match.arg(units)

    if (is.matrix(x)) {
      stopifnot(ncol(x) == 1)
      x %<>% as.vector(.)
    }


  ## Check for missing values

    if (anyNA(x)) warning(
      "Detected ", sum(is.na(x)), " missing value(s) for the ",
      label, " method", call. = FALSE
    )


  ## Check for low values

    check_small <- (x < minimum) %in% TRUE

    if (any(check_small)) {

      if (warn_high_low) warning(
        "Rounding up ", paste(sum(check_small), variable), " value(s) below",
        " the minimum of ", paste(minimum, units), " for the ", label,
        " method", call. = FALSE
      )

      x %<>% pmax(minimum)

    }


  ## Check for high values

    check_big <- (x > maximum) %in% TRUE

    if (any(check_big)) {

      if (warn_high_low) warning(
        "Rounding down ", paste(sum(check_big), variable), " value(s) above",
        " the maximum of ", paste(maximum, units), " for the ", label,
        " method", call. = FALSE
      )

      x %<>% pmin(maximum)

    }


  ## Finish up

    x

}
