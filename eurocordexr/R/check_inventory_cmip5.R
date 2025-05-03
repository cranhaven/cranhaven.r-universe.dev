#' Perform some checks on the inventory of CMIP5 files
#'
#' Some simple checks for multiple time frequencies, ensembles, and completeness
#' of simulation periods. These checks are meant as guides only, since one might
#' not wish multiple elements of the above for climate model ensemble
#' assessments.
#'
#' The checks are \itemize{
#' \item for multiple time frequency (day, month, ...)
#' \item for multiple ensembles (r1i1p1, r2i1p1, ...)
#' \item for complete periods of simulations: here complete means at least
#' 1860-2005 for historical and 2006-2099 for rcp*
#' \item that each rcp* has a corresponding historical run (optional, off by
#' default; otherwise problematic with merged hist and rcp runs)
#' }
#'
#'
#' @param data_inventory A data.table as resulting from
#'   \code{\link{get_inventory}}.
#' @param check_hist Boolean, if \code{TRUE}, tests that each rcp* has a corresponding historical run.
#'
#' @return An object of class "eurocordexr_inv_check_cmip5" (an overloaded list) with results
#' from the checks. Has a special print method, which shows a verbose summary of the results.
#'
#'
#' @export
#'
#' @import data.table
#' @importFrom magrittr %>%
#'
#' @examples
#' # some empty example files
#' fn_zip <- system.file("extdata", "inv-test-files-cmip5.zip", package = "eurocordexr")
#' tmpdir <- tempdir()
#' unzip(fn_zip, exdir = tmpdir)
#'
#' dat_inv <- get_inventory_cmip5(fs::path(tmpdir, "testdata-cmip5"))
#' check_inventory_cmip5(dat_inv)
#'
check_inventory_cmip5 <- function(data_inventory,
                                  check_hist = FALSE){

  if(!inherits(data_inventory, "cmip5")) stop("Not a CMIP5 inventory, please use check_inventory() instead.")

  # NSE in R CMD check
  list_files <- ensembles <- N <-  NULL
  variable <- gcm <- experiment <- ensemble <- NULL
  timefreq <- date_start <- date_end <- period_contiguous <-  NULL
  all_years_equal <- all_date_start_equal <- total_simulation_years <- NULL


  dat_inv <- copy(data_inventory)

  # remove list_files column for nicer printing
  if("list_files" %in% names(dat_inv)) dat_inv[, list_files := NULL]


  l_out <- list()



  # check for multiple timefreq
  timefreqs <- sort(unique(dat_inv$timefreq))
  timefreqs <- timefreqs[timefreqs != "fx"]

  l_out$timefreqs <- timefreqs

  # check for multiple ensembles
  dat_mult_ens <- dat_inv[,
                          list(N = .N,
                               ensembles = paste(ensemble, collapse = ", ")),
                          list(variable, gcm, experiment, timefreq)]
  dat_mult_ens <- dat_mult_ens[N > 1]

  l_out$multiple_ensembles <- dat_mult_ens


  # check for complete periods: minimum 1860-2005 for hist and 2006-2099 for rcp*
  dat_period_complete <- rbind(
    dat_inv[experiment == "historical" & !is.na(date_start) &
              !(year(date_start) <= 1860 & year(date_end) >= 2005 & total_simulation_years >= 146 & period_contiguous == TRUE)],
    dat_inv[startsWith(experiment, "rcp") & !is.na(date_start) &
              !(year(date_start) <= 2006 & year(date_end) >= 2099 & total_simulation_years >= 94 & period_contiguous == TRUE)]
  )

  l_out$incomplete_periods <- dat_period_complete


  # check that each rcp has a historical
  if(check_hist){
    dat_hist <- dat_inv[timefreq != "fx" & experiment == "historical",
                        list(variable, gcm, ensemble, timefreq)]
    l_out$missing_historical <- dat_inv[
      timefreq != "fx" & experiment != "historical"
    ][!dat_hist, on = names(dat_hist)]
  }

  class(l_out) <- c("eurocordexr_inv_check_cmip5", class(l_out))
  l_out
}


#' @export
print.eurocordexr_inv_check_cmip5 <- function(x, ...){

  # start
  cat("Checks performed:", "\n")
  cat("------------------------------------------------------\n")
  cat("------------------------------------------------------\n")


  # check for multiple timefreq
  if(length(x$timefreqs) > 1) {
    cat("Multiple time frequencies detected:", x$timefreqs, "\n")
  } else {
    cat("No multiple time frequencies.", "\n")
  }
  cat("------------------------------------------------------\n")
  cat("------------------------------------------------------\n")



  # check for multiple ensembles
  n_mult_ens <- nrow(x$multiple_ensembles)
  if(n_mult_ens > 0){
    cat("Multiple ensembles in", n_mult_ens, "cases:", "\n")
    print(x$multiple_ensembles)
  } else {
    cat("No multiple ensembles.", "\n")
  }
  cat("------------------------------------------------------\n")
  cat("------------------------------------------------------\n")


  if(nrow(x$incomplete_periods) == 0){
    cat("All historical and rcp simulations have complete periods.\n")
  } else {
    cat("Following model runs do not have complete periods:", "\n")
    print(x$incomplete_periods)
  }
  cat("------------------------------------------------------\n")
  cat("------------------------------------------------------\n")


  # check that each rcp has a historical
  if(!is.null(x$missing_historical)){
    if(nrow(x$missing_historical) == 0){
      cat("All rcp simulations have a corresponding historical run.\n")
    } else {
      cat("Following scenario model runs do not have a corresponding historical run:", "\n")
      print(x$missing_historical)
    }
    cat("------------------------------------------------------\n")
    cat("------------------------------------------------------\n")
  }


  # end
  cat("Finished checks.", "\n")
  cat("------------------------------------------------------\n")
  cat("------------------------------------------------------\n")


}
