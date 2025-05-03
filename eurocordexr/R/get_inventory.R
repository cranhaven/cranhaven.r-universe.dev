#' Get inventory from path containing EURO-CORDEX .nc files
#'
#' Returns a data.table with information by splitting the netcdf files into
#' their components (GCM, RCM, variable, experiment, ...) and aggregates over
#' years.
#'
#' @param path Path that will be searched recursively for .nc files.
#' @param add_files Boolean (default \code{TRUE}), if \code{TRUE}, will add a
#'   column containing lists of associated files with their full paths (useful
#'   e.g. for further processing).
#'
#' @return A data.table with the inventory information.
#'
#' @seealso \code{\link{check_inventory}} for performing some checks.
#'
#' @export
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#'
#' @examples
#' # some empty example files
#' fn_zip <- system.file("extdata", "inv-test-files.zip", package = "eurocordexr")
#' tmpdir <- tempdir()
#' unzip(fn_zip, exdir = tmpdir)
#'
#' dat_inv <- get_inventory(fs::path(tmpdir, "testdata", "mixed-vars"))
#' print(dat_inv)
#'
#'
get_inventory <- function(path,
                          add_files = TRUE){

  # NSE in R CMD check
  list_files <- timefreq <- downscale_realisation <- experiment <- NULL
  institute_rcm <- gcm <- domain <- variable <- ensemble <- fn <- NULL
  V9 <- file_fullpath <- NULL
  period <- date_start <- date_end <- NULL


  all_files_fullpath <- fs::dir_ls(path,
                                   regexp = "[.]nc$",
                                   recurse = TRUE)

  all_files_base <- fs::path_file(all_files_fullpath)

  # create info data
  data.table(fn = fs::path_ext_remove(all_files_base)) %>%
    .[, tstrsplit(fn, "_")] -> dat_info

  # add period column (V9), if it does not exist, as e.g. for OROG
  if(! "V9" %in% names(dat_info)){
    dat_info[, V9 := NA_character_]
  }

  setnames(dat_info,
           old = paste0("V", 1:9),
           new = c("variable", "domain", "gcm", "experiment","ensemble",
                   "institute_rcm", "downscale_realisation", "timefreq", "period"))

  # check for errors in period encoding
  dat_info[period == ensemble, period := NA]

  # add file
  dat_info[, file_fullpath := all_files_fullpath]

  # prep dates
  dat_info[, c("date_start", "date_end") := tstrsplit(period, "-")]
  dat_info[, date_start := lubridate::ymd(date_start)]
  dat_info[, date_end := lubridate::ymd(date_end)]

  # helper fun to check for complete contiguous period
  f_date_complete <- function(date_start, date_end){

    if(all(is.na(date_start))) return(NA)

    # 360 calendar adjustment
    lgl_check <- lubridate::month(date_end) == 12 & lubridate::day(date_end) == 30
    lubridate::`day<-`(date_end[lgl_check], 31)

    mapply(seq, date_start, date_end, by = "day") %>%
      unlist %>%
      sort %>%
      unique %>%
      diff %>%
      magrittr::equals(1) %>%
      all

  }

  f_sim_years <- function(date_start, date_end){

    if(all(is.na(date_start))) return(NA_integer_)

    mapply(seq, lubridate::year(date_start), lubridate::year(date_end), SIMPLIFY = FALSE) %>%
      unlist %>%
      unique %>%
      length

  }


  # get unique models
  dat_info_summary <- dat_info[,
                               list(nn_files = .N,
                                    date_start = min(date_start),
                                    date_end = max(date_end),
                                    total_simulation_years = f_sim_years(date_start, date_end),
                                    period_contiguous = f_date_complete(date_start, date_end),
                                    list_files = list(file_fullpath)),
                               by = list(variable, domain, gcm, institute_rcm, experiment,
                                            ensemble, downscale_realisation, timefreq)]

  setorder(dat_info_summary,
           variable, domain, gcm, institute_rcm, experiment,
           ensemble, downscale_realisation, timefreq)

  # remove files if not requested
  if(!add_files) dat_info_summary[, list_files := NULL]

  # make a class to define specific print options
  setattr(dat_info_summary, "class", c("eurocordexr_inv", class(dat_info_summary)))

  return(dat_info_summary)
}
