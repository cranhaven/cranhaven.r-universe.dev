# Get features ------------------------------------------------------------

#' Calculate features for Staudenmayer models
#'
#' @inheritParams generic_features
#'
#' @return Data frame containing features in the specified format
#' @export
#'
#' @references Staudenmayer et al. (2015), \doi{10.1152/japplphysiol.00026.2015}
#'
#' @examples
#' if (isTRUE(requireNamespace("read.gt3x"))) {
#'
#'   f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
#'   d <- read.gt3x::read.gt3x(f, asDataFrame = TRUE, imputeZeroes = TRUE)
#'
#'   staudenmayer_features(d[1:12000, ], "time", "X", "Y", "Z")
#'
#' }
staudenmayer_features <- function(
  d, time_var = "Timestamp", x_var = "Accelerometer_X",
  y_var = "Accelerometer_Y", z_var = "Accelerometer_Z",
  win_width_sec = 15, verbose = FALSE, ...
) {

  if (verbose) cat("\n...Calculating Staudenmayer features")

  samp_freq <- get_samp_freq(d, time_var)

  expected <- samp_freq * win_width_sec

  d %T>%
  {if ("vm" %in% names(.)) warning(
    "Overwriting/re-calculating `vm`", call. = FALSE
  )} %T>%
  {if ("v.ang" %in% names(.)) warning(
    "Overwriting/re-calculating `v.ang`", call. = FALSE
  )} %>%
  dplyr::mutate(
    vm := sqrt(
      (!!as.name(x_var))^2 +
      (!!as.name(y_var))^2 +
      (!!as.name(z_var))^2
    ),
    v.ang =
      {ifelse(vm == 0, 0, (!!as.name(x_var))/vm)} %>%
      {90*(asin(.)/(pi/2))}
  ) %>%
  dplyr::group_by(grp = lubridate::floor_date(
    !!as.name(time_var), lubridate::period(win_width_sec)
  )) %>%
  dplyr::summarise(
    !!as.name(time_var) := lubridate::floor_date(
      dplyr::first(!!as.name(time_var))
    ),
    dplyr::across(
      dplyr::all_of(c("vm", "v.ang")),
      list(mean = mean, sd = staud_sd),
      .names = "{.fn}.{.col}"
    ),
    powers = powers(vm, samp_freq),
    n = dplyr::n(),
    .groups = "drop"
  ) %T>%
  {if (sum(.$n != expected) > 1) stop(
    "More than one short window detected in the input",
    " for Staudenmayer feature calculation", call. = FALSE
  )} %>%
  tidyr::unpack(powers) %>%
  dplyr::filter(n == expected) %>%
  dplyr::select(-c(n, grp)) %>%
  stats::setNames(., gsub("\\.v.ang", ".ang", names(.)))

}


# Get predictions (main function) -----------------------------------------

staudenmayer <- function(
  d, time_var = "Timestamp", output_epoch = "default",
  min_mets = 1, max_mets = 20, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  feature_calc = TRUE, shrink_output = TRUE,
  verbose = FALSE, ..., select = c("METs_lm", "METs_rf")
) {


  ## Startup printing and checks

    if (!isTRUE(requireNamespace("EE.Data", quietly = TRUE))) stop(
      "You must install package `EE.Data` to use the",
      " Staudenmayer method(s)", call. = FALSE
    )

    stopifnot(select %in% c("METs_lm", "METs_rf"))


  ## Main operations

    if (feature_calc) {

      d %<>% staudenmayer_features(., time_var, verbose = verbose, ...)

    }

    if (verbose) cat(
      "\n...Getting Staudenmayer predictions (",
      paste(
        dplyr::recode(
          unique(select), "METs_lm" = "linear",
          "METs_rf" = "random forest"
        ),
        collapse = " and "
      ),
      dplyr::recode(
        length(select), "model", "models",
        .default = "model(s)"
      ),
      ")",
      sep = ""
    )

    results <-
      d %>%
      predict_staudenmayer(
        "METs_lm", select,
        EE.Data::staudenmayer_lm, "Staudenmayer Linear",
        min_mets, max_mets, warn_high_low
      ) %>%
      predict_staudenmayer(
        "METs_rf", select,
        EE.Data::staudenmayer_rf, "Staudenmayer Random Forest",
        min_mets, max_mets, warn_high_low
      ) %>%
      met_expand(
        "METs", "staudenmayer", met_mlkgmin,
        min_mets, max_mets, RER, warn_high_low
      )


  ## Last steps

    if (shrink_output) results %<>% dplyr::select(
      dplyr::all_of(time_var),
      dplyr::matches("staudenmayer")
    )

    return_vals(
      results, time_var,
      output_epoch, verbose
    )


}


# Get predictions (supporting functions) ----------------------------------

staud_sd <- function(x) {
  if (dplyr::n_distinct(x) == 1) return(0)
  stats::sd(x)
}


predict_staudenmayer <- function(
  d, out_name, select, model, label, min_mets,
  max_mets, warn_high_low = TRUE
) {

  if (!out_name %in% select) return(d)
  stopifnot(exists("sd.vm", d))

  predict_model(
    d, out_name, model, label,
    min_mets, max_mets, warn_high_low
  ) %>%
  dplyr::mutate(
    !!as.name(out_name) := ifelse(
      sd.vm < 0.01, min_mets, !!as.name(out_name)
    )
  )

}


powers <- function(vm, samp_freq) {

  mods <-
    stats::fft(vm) %>%
    Mod(.) %>%
    .[-1]

  n <-
    length(mods) %>%
    {. / 2} %>%
    floor(.)

  freq <- samp_freq*(1:n)/(2*n)

  mods %<>%
    .[1:n] %>%
    {ifelse(is.na(.), 0, .)}

  sd_vm <- stats::sd(vm)

  dplyr::tibble(
    p625 = pow.625(vm, freq, n, mods, sd_vm),
    dfreq = dom.freq(vm, mods, freq),
    ratio.df = frac.pow.dom.freq(vm, mods, sd_vm)
  )

}


pow.625 <- function(vm, freq, n, mods, sd_vm) {

  if (sd_vm == 0) return(0)

  {freq>0.6 & freq<2.5} %>%
  {(1:n)[.]} %>%
  mods[.] %>%
  sum(.) %>%
  {. / sum(mods)}

}


dom.freq <- function(vm, mods, freq) {

	if(length(vm)==1) return(NA)

  which.max(mods) %>%
  {freq[.]} %>%
  as.vector(.)

}


frac.pow.dom.freq <- function(vm, mods, sd_vm) {

  if (sd_vm == 0) return(0)

  max(mods)/sum(mods)

}
