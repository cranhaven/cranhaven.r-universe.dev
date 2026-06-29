# Main function -----------------------------------------------------------

#' Run the Hibbing 2023 file preparation routine
#'
#' @aliases agd_hibbing23 gt3x_hibing23
#'
#' @usage
#'
#' ## Wrapper function:
#' ee_file_hibbing23(filename, ...)
#'
#'
#' ## Sub-routine functions
#' agd_hibbing23(filename, verbose = FALSE, ...)
#'
#' @inheritParams ee_file
#' @param verbose logical. Print updates to console?
#'
#' @return A data frame whose contents are prepared according to the Hibbing
#'   2023 scheme
#'
#' @note This routine requires the \code{PhysicalActivity},
#'   \code{PhysActBedRest}, and \code{read.gt3x} packages. Also note that the
#'   original Hibbing paper used a package called \code{AGread} rather than
#'   \code{read.gt3x}, and there are some differences. \code{AGread} is
#'   available on GitHub, but not CRAN--hence, the requirement for
#'   \code{read.gt3x} here.
#'
#' @keywords internal
#' @name hibbing23-file
ee_file_hibbing23 <- function(filename, ...) {

  ext <- get_extension(filename)

  switch(
    ext,
    "agd" = agd_hibbing23(filename, ...),
    "gt3x" = gt3x_hibbing23(filename, ...),
    stop(
      "The `Hibbing 2023` scheme does not include a",
      " preparation routine for *.", ext, " file extensions"
    )
  )

}


# Subroutines -------------------------------------------------------------

agd_wrap <- function(filename) {

  stopifnot(
    test_package("PhysicalActivity", "Hibbing 2023 scheme")
  )

  PhysicalActivity::readActigraph(filename) %>%
  dplyr::rename_with(
    ~gsub("^TimeStamp$", "Timestamp", .x) %>%
     gsub("^axis", "Axis", .) %>%
     gsub("^vm$", "Vector.Magnitude", .) %>%
     gsub("^steps$", "Steps", .) %>%
     gsub("^incline", "Inclinometer.", .) %>%
     gsub("^lux$", "Lux", .)
  ) %T>%
  {stopifnot("Timestamp" %in% names(.))} %>%
  dplyr::mutate(
    !!as.name("Date") :=
      strftime(Timestamp, "%m/%d/%Y", tz = lubridate::tz(Timestamp)) %>%
      gsub("^0([0-9])", "\\1", .) %>%
      gsub("/0([0-9])/", "/\\1/", .),
    !!as.name("Time") := strftime(Timestamp, "%H:%M:%S", tz = lubridate::tz(Timestamp))
  ) %>%
  dplyr::relocate(dplyr::any_of(c("Timestamp", "Date", "Time"))) %>%
  structure(metadata = NULL)

}

agd_hibbing23 <- function(filename, verbose = FALSE, ...) {

  stopifnot(
    get_extension(filename) == "agd",
    test_package("PhysicalActivity", "Hibbing 2023 scheme"),
    test_package("PhysActBedRest", "Hibbing 2023 scheme")
  )

  agd_wrap(filename) %>%
  within({TS = Timestamp}) %>%
  epoch_check(
    verbose = verbose,
    routine_label = "*.agd routine (Hibbing 2023 scheme)"
  ) %>%

  PhysicalActivity::wearingMarking(
    perMinuteCts = 1, TS = "Timestamp",
    cts = "Axis1", newcolname = "Choi_is_NonWear"
  ) %>%
  PhysActBedRest::markbedrest(
    "TS", "Axis1", "adult", "wrist", tempdir()
  ) %>%

  within({

    TS = NULL
    days = NULL

    Tracy_is_Sleep = bedrest == "br"
    bedrest = NULL

    Choi_is_NonWear = Choi_is_NonWear == "nw"

    valid_status = ifelse(
      Choi_is_NonWear,
      "Non-Wear",
      ifelse(Tracy_is_Sleep, "Sleep", "Awake-Wear")
    )

    is_WakeWear = valid_status == "Awake-Wear"
    is_Sleep = valid_status == "Sleep"
    is_NonWear = valid_status == "Non-Wear"

    weekday = factor(
      weekday,
      c(
        "Monday", "Tuesday", "Wednesday",
        "Thursday", "Friday", "Saturday", "Sunday"
      )
    )

    is_weekend = weekday %in% c("Saturday", "Sunday")

  }) %>%

  PAutilities::df_reorder(
    c("weekday", "is_weekend"), "Timestamp"
  ) %>%
  PAutilities::df_reorder(
    "Vector.Magnitude", "Axis3"
  ) %>%
  PAutilities::df_reorder(
    c("Tracy_is_Sleep", "valid_status"), "Choi_is_NonWear"
  ) %>%
  .[ ,!grepl("^Inclinometer", names(.))] %T>%
  {file.remove(file.path(tempdir(), "subj_slp_sum.csv"))}

}

gt3x_wrap <- function(filename) {

  stopifnot(
    test_package("read.gt3x", "Hibbing 2023 scheme")
  )

  d <-
    read.gt3x::read.gt3x(
      path = filename,
      verbose = FALSE,
      asDataFrame = TRUE,
      imputeZeroes = TRUE
    ) %T>%
    {stopifnot(identical(names(.), c("time", "X", "Y", "Z")))} %>%
    stats::setNames(c("Timestamp", paste0("Accelerometer_", c("X", "Y", "Z")))) %>%
    dplyr::mutate(Timestamp = lubridate::force_tz(Timestamp, "UTC"))

  remove <-
    attributes(d) %>%
    names(.) %>%
    setdiff(c("names", "row.names", "class"))

  for (x in remove) {
    attr(d, x) <- NULL
  }

  attr(d, "class") <- c("RAW", "data.frame")

  d

}

gt3x_hibbing23 <- function(filename, verbose = FALSE, ...) {


  ## Setup and reading

    stopifnot(
      get_extension(filename) == "gt3x",
      test_package("read.gt3x", "Hibbing 2023 scheme")
    )

    d <- gt3x_wrap(filename)


  ## Run methods

    methods_1s <-
      generic_features(d, verbose = verbose) %!>%
      accelEE(
        c("Hildebrand Linear", "Hildebrand Non-Linear", "Hibbing 2018"),
        feature_calc = FALSE, output_epoch = "60 sec", ee_vars = "kcal",
        warn_high_low = FALSE, verbose = verbose,
        algorithm = 1, site = c("Left Wrist", "Right Wrist"),
        age = "adult", monitor = "ActiGraph", location = "wrist"
      ) %>%
      dplyr::rename_with(
        ~ gsub("^hibbing18", "hibbing", .x),
        dplyr::matches("^hibbing18")
      ) %>%
      dplyr::rename_with(
        ~ tolower(.x),
        dplyr::matches("kcal_kgmin")
      ) %>%
      dplyr::rename_with(~ gsub("kcal_kgmin_", "", .x, TRUE)) %>%
      dplyr::rename_with(~ gsub("_Wrist_Algorithm1$", "", .x, TRUE)) %>%
      dplyr::rename_with(~ gsub("_adult_ActiGraph_wrist$", "", .x, TRUE)) %>%
      dplyr::relocate(vm:ENMO, .after = Accelerometer_Z) %>%
      dplyr::rename("vm_raw_g" = "vm", "ENMO_mg" = "ENMO") %>%
      dplyr::relocate(!dplyr::matches("^hibbing"))

    montoye <-
      montoye_features(d, verbose = verbose) %!>%
      accelEE(
        "Montoye 2017", feature_calc = FALSE, output_epoch = "60 sec",
        warn_high_low = FALSE, ee_vars = "kcal", verbose = verbose
      ) %>%
      dplyr::rename_with(~ gsub("kcal_kgmin_", "", .x, TRUE)) %>%
      dplyr::rename_with(~ gsub("_wrist$", "", .x, TRUE))

    staudenmayer <-
      staudenmayer_features(d, verbose = verbose) %!>%
      accelEE(
        c("Staudenmayer Linear", "Staudenmayer Random Forest"),
        feature_calc = FALSE, output_epoch = "60 sec", warn_high_low = FALSE,
        ee_vars = "kcal", verbose = verbose
      ) %>%
      dplyr::rename_with(~ gsub("kcal_kgmin_", "", .x, TRUE))


  ## Combine methods

    list(methods_1s, montoye, staudenmayer) %>%
    sapply(nrow) %>%
    range(.) %>%
    diff(.) %>%
    {if (. > 1) stop("Unexpected row numbers", call. = FALSE)}

    list(methods_1s, montoye, staudenmayer) %>%
    Reduce(merge, .) %>%
    tidyr::pack(
      montoye_features = AL_LW_X_pTen:AL_RW_Z_cov,
      staudenmayer_features = mean.vm:ratio.df
    )


}


# Helper function(s) ------------------------------------------------------

epoch_check <- function(
  d, time_var = "Timestamp",
  target = 60, verbose = FALSE,
  routine_label
) {

  e <- epoch_length(d, time_var)

  if (e == target) return(d)

  if (e > target) stop(
    "Cannot execute the ", routine_label, " on files",
    " with epoch length > ", target, " seconds", call. = FALSE
  )

  if (verbose) cat("\n...Reintegrating to ", target, "-s epochs", sep = "")

  PAutilities::reintegrate(
    df = d,
    target_sec = target,
    time_var = time_var
  )

}
