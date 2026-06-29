# Get features ------------------------------------------------------------

#' Calculate features for Montoye's neural networks
#'
#' @inheritParams generic_features
#' @inheritParams accelEE-function
#'
#' @return A dataframe of features for entry into the neural networks
#' @export
#'
#' @references Montoye et al. (2017) \doi{10.1080/1091367X.2017.1337638}
#'
#' @examples
#' if (isTRUE(requireNamespace("read.gt3x"))) {
#'
#'   f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
#'   d <- read.gt3x::read.gt3x(f, asDataFrame = TRUE, imputeZeroes = TRUE)
#'
#'   montoye_features(d[1:12000, ], "time", "X", "Y","Z", "right")
#'
#' }
montoye_features <- function(
  d, time_var = "Timestamp",  x_var = "Accelerometer_X",
  y_var = "Accelerometer_Y", z_var = "Accelerometer_Z",
  side = c("left", "right"), win_width_sec = 30, verbose = FALSE, ...
) {

  stopifnot(all(side %in% c("left", "right")))

  if (verbose) cat(
    "\n...Getting Montoye features (",
    paste(side, collapse = " and "), " side)", sep = ""
  )

  expected <- get_samp_freq(d, time_var) * win_width_sec

  d %<>%
    dplyr::rename(
      X = !!as.name(x_var),
      Y = !!as.name(y_var),
      Z = !!as.name(z_var)
    ) %>%
    dplyr::group_by(
      !!as.name(time_var) := lubridate::floor_date(
      !!as.name(time_var), lubridate::period(win_width_sec)
    )) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = c(X, Y, Z),
        .fns =
          ~ .x %>%
          stats::quantile(probs = c(0.10, 0.25, 0.50, 0.75, 0.90)) %>%
          c(cov = auto_cov(.x)) %>%
          matrix(nrow = 1) %>%
          as.data.frame(.) %>%
          stats::setNames(c(
            "pTen", "pTwentyFive", "pFifty",
            "pSeventyFive", "pNinety", "cov"
          )),
        .names = "AL_LW_{.col}"
      ),
      n = dplyr::n()
    ) %T>%
    {if (sum(.$n != expected) > 1) stop(
      "Unexpected issue with feature calculation for",
      " the Montoye 2017 method", call. = FALSE
    )} %>%
    dplyr::filter(n == expected) %>%
    dplyr::select(-n) %>%
    tidyr::unpack(dplyr::all_of(paste0("AL_LW_", c("X", "Y", "Z"))), names_sep = "_")

  if ("right" %in% side) d %<>%
    stats::setNames(., gsub("^AL_LW_", "AL_RW_", names(.))) %>%
    dplyr::select(!dplyr::all_of(time_var)) %>%
    dplyr::bind_cols(d, .)

  if (!"left" %in% side) d %<>% dplyr::select(
    !dplyr::matches("^AL_LW_")
  )

  d

}


# Get predictions (main function) -----------------------------------------

montoye <- function(
  d, time_var = "Timestamp", output_epoch = "default",
  min_mets = 1, max_mets = 20, warn_high_low = TRUE,
  met_mlkgmin = 3.5, RER = 0.85,
  feature_calc = TRUE, shrink_output = TRUE,
  verbose = FALSE, side = c("left", "right"),
  ...
) {


  ## Startup printing and checks

    if (!isTRUE(requireNamespace("EE.Data", quietly = TRUE))) stop(
      "You must install package `EE.Data` to use the",
      " Montoye method(s)", call. = FALSE
    )

    stopifnot(all(side %in% c("left", "right")))


  ## Main operations

    if (feature_calc) {

      d %<>% montoye_features(time_var, side = side, verbose = verbose, ...)

    }

    if (verbose) cat(
      "\n...Getting Montoye predictions (",
      paste(side, collapse = " and "), "side)",
      sep = ""
    )

    results <-
      d %>%
      predict_montoye(
        "METs_left_wrist", "left", side, EE.Data::montoye_lw,
        "Montoye Left Wrist", min_mets, max_mets, warn_high_low
      ) %>%
      predict_montoye(
        "METs_right_wrist", "right", side, EE.Data::montoye_rw,
        "Montoye Right Wrist", min_mets, max_mets, warn_high_low
      ) %>%
      met_expand(
        "METs", "montoye", met_mlkgmin,
        min_mets, max_mets, RER, warn_high_low
      )


  ## Last steps

    if (shrink_output) results %<>% dplyr::select(
      !dplyr::matches("^AL_[RL]W_")
    )

    return_vals(
      results, time_var,
      output_epoch, verbose
    )


}


# Get predictions (supporting function) -----------------------------------

predict_montoye <- function(
  d, out_name, side, select, model,
  label, min_mets, max_mets, warn_high_low = TRUE
) {

  if (!side %in% select) return(d)

  expected_names <- attr(model$terms, "term.labels")

  if (!all(expected_names %in% names(d))) {

    stop(
      "Required features are not present for Montoye ",
      side, " wrist method", call. = FALSE
    )

  }

  predict_model(d, out_name, model, label, min_mets, max_mets, warn_high_low)

}

