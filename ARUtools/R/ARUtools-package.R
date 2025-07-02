#' ARUtools: Management and Processing of Autonomous Recording Unit (ARU) Data
#'
#' @description
#' Parse Autonomous Recording Unit (ARU) data and for sub-sampling recordings.
#' Extract Metadata from your recordings, select a subset of recordings for
#' interpretation, and prepare files for processing on the
#' WildTrax <https://wildtrax.ca/> platform. Read and process metadata
#' from recordings collected using the Song Meter and BAR-LT types of ARUs.
#'
#' @docType package
#' @name ARUtools
#' @import rlang
#'
"_PACKAGE"

rlang::on_load(rlang::local_use_cli())




#' Set options for matching column headers in GPS text logs
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Set column patterns (case insensitive)
  options(
    ARUtools =
      list(
        pat_gps_date = "date|(DD/MM/YY)",
        pat_gps_time = "time|(HH/MM(/SS)?)|(HH:MM(:SS)?)", # Optional seconds
        pat_gps_coords = c("lon", "lat")
      )
  )
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(
      # Vars used in Non-Standard Evaluations, declare here to
      # avoid notes. Here are Defaults for various functions
      c(
        "site_id", "psel_std", "t2sr", # sample_recordings()
        "path", "subdir_out", "filename_out", "clip_length", "start_time", # clip_wave(),
        "totalwindless", # wind_detection_summarize_json()
        "taskLength", "transcriber", "hrs_assigned", # wt_assign_tasks(),
        "is_testing" # sox_spectro() for usethis failure
      )
    )
  }
}
