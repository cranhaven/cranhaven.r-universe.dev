#' Helper function for GPS tests
#'
#' Creates an example GPS file for testing
#'
#' @noRd
test_gps <- function(lon = "Longitude (decimal degrees)",
                     lat = "Latitude (decimal degrees)",
                     time = "HH:MM:SS",
                     date = "DD/MM/YY", skips = 2, path = "gps.csv") {
  p <- testthat::test_path(path)

  meta <- rep("GPS log file", skips - 1)
  for (i in p) {
    readr::write_lines(
      c(
        meta,
        paste(lat, lon, time, date, sep = ", "),
        "45,  -76,  07:30:00, 25/05/2021",
        "55,  -84.3, 16:08:00, 03/06/2021"
      ),
      i
    )
  }

  p
}

#' Helper function for meta data tests
#'
#' Creates a directory structure with example recording files
#'
#' @noRd
temp_files <- function() {
  purrr::walk(
    fs::path_temp(ARUtools::example_files),
    ~ {
      fs::dir_create(dirname(.x))
      writeLines("test", .x)
    }
  )

  fs::path_temp(ARUtools::example_files)
}

#' Helper function to create test wave files
#'
#' Creates a directory structure and example wave files in temp folders.
#'
#' @param n Numeric. How many test files to create (up to six). D
#'
#' @return vector of paths to temporary wave files
#'
#' @export
#'
#' @examples
#' temp_wavs(n=3)
#'
temp_wavs <- function(n = 6) {
  f_in <- fs::path(
    tempdir(),
    "waves",
    rep(c("site1", "site2", "site3"), 2),
    c(
      "file1.wav", "file2.wav", "file3.wav",
      "file4.wav", "file5.wav", "file6.wav"
    )
  )
  if (n < 6) f_in <- f_in[seq_len(n)]
  fs::path_dir(f_in) |> fs::dir_create()
  w <- tuneR::sine(440, duration = 100000)
  purrr::map(f_in, \(x) tuneR::writeWave(w, x))
  f_in
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
