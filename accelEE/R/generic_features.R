#' Calculate generic features for model application
#'
#' @inheritParams accelEE-function
#' @param x_var character. Name of the X-axis variable
#' @param y_var character. Name of the Y-axis variable
#' @param z_var character. Name of the Z-axis variable
#' @param win_width_sec desired window width for features
#' @param ... currently unused
#'
#' @return A data frame of features in the specified epoch length
#' @export
#'
#' @examples
#' if (isTRUE(requireNamespace("read.gt3x"))) {
#'
#'   f <- system.file("extdata/TAS1H30182785_2019-09-17.gt3x", package = "read.gt3x")
#'   d <- stats::setNames(
#'     read.gt3x::read.gt3x(f, asDataFrame = TRUE, imputeZeroes = TRUE),
#'     c("Timestamp", "Accelerometer_X", "Accelerometer_Y", "Accelerometer_Z")
#'   )[1:30000, ]
#'
#'   utils::head(generic_features(d))
#'
#' }
generic_features <- function(
  d, time_var = "Timestamp", x_var = "Accelerometer_X",
  y_var = "Accelerometer_Y", z_var = "Accelerometer_Z",
  win_width_sec = 1, verbose = FALSE, ...
) {

  if (verbose) cat("\n...Calculating generic features")

  d %T>%
  {if ("vm" %in% names(.)) warning(
    "Overwriting/re-calculating `vm`", call. = FALSE
  )} %T>%
  {if ("ENMO" %in% names(.)) warning(
    "Overwriting/re-calculating `ENMO`", call. = FALSE
  )} %>%
  dplyr::mutate(
    vm := sqrt(
      (!!as.name(x_var))^2 +
        (!!as.name(y_var))^2 +
        (!!as.name(z_var))^2
    ),
    ENMO = pmax(vm - 1, 0)*1000
  ) %!>%
  collapse_EE(
    time_var, lubridate::period(win_width_sec),
    verbose = verbose & win_width_sec != 1
  )

}
