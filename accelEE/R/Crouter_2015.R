crouter15 <- function(
  d, time_var = "Timestamp", output_epoch = "default",
  min_mets = 1, max_mets = 20, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  shrink_output = TRUE, verbose = FALSE,
  model = c("VA", "VM"), movement_var = "Axis1",
  ...
) {

  model <- match.arg(model, c("VA", "VM"), TRUE)

  stopifnot(length(movement_var) == length(model))

  mapply(
    crouter15_individual,
    model = model,
    movement_var = movement_var,
    MoreArgs = list(
      d = d, time_var = time_var, output_epoch = output_epoch,
      min_mets = min_mets, max_mets = max_mets, warn_high_low = warn_high_low,
      met_mlkgmin = met_mlkgmin, RER = RER, shrink_output = shrink_output,
      verbose = verbose, ...
    ),
    SIMPLIFY = FALSE
  ) %>%
  {Reduce(merge, .)}


}

crouter15_individual <- function(
  d, time_var = "Timestamp", output_epoch = "default",
  min_mets = 1, max_mets = 20, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  shrink_output = TRUE, verbose = FALSE,
  model = c("VA", "VM"), movement_var = "Axis1",
  ...
) {

  model_name <- paste("Crouter 2015", model)

  tag <- paste0("Crouter15_", model)

  if (verbose) cat(
    "\n...Getting predictions for the", model_name, "method"
  )

  use_default <- is_default(output_epoch)

  if (use_default) {
    output_epoch <-
      lookup_epoch("Crouter 2015", "unique") %>%
      lubridate::period(.)
  }

  ## Retrieve information

  info <-
    .crouter15 %>%
    dplyr::filter(.model == model) %>%
    as.list(.)

  ## Run operations

  results <-
    d %>%
    epoch_check(
      time_var, 5, verbose,
      paste("Crouter 2015", model, "model")
    ) %>%
    dplyr::mutate(
      METs := ifelse(
        !!as.name(movement_var) <= info$cp,
        1,
        info$intercept + !!as.name(movement_var) * info$slope
      ) %>%
        check_values(min_mets, max_mets, model)
    ) %>%
    met_expand(
      "METs", tag, met_mlkgmin,
      min_mets, max_mets, RER, warn_high_low
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
