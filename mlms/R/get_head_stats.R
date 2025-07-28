#' Calculate Hydraulic Head Statistics
#'
#' @description Calculate summary statistics for multi-level hydraulic head data,
#'   categorized by site and port, and summarized over time.
#'
#' @param vars 'character' vector.
#'   One or more variable names for which to compute summary statistics.
#'   Choices include:
#'   "total_head_va" is the hydraulic head in feet above the North American Vertical Datum of 1988,
#'   "temp_va" is the fluid temperature in degree Celsius,
#'   "baro_va" is the atmospheric pressure in pounds per square inch absolute (psi), and
#'   "press_va" is the absolute fluid pressure in psi.
#'   By default, all variables are included.
#' @param by_port 'logical' flag.
#'   Whether to compute statistics according to a sites monitoring port.
#'   Defaults to grouping by site only.
#'
#' @return A data frame with the following variables:
#'   \describe{
#'     \item{`var_ds`}{Variable description.}
#'     \item{`var_nm`}{Variable name as specified in the `vars` argument.}
#'     \item{`site_nm`}{Local site name for a MLMS well.}
#'     \item{`port_nu`}{Identifier for the valved measurement port,
#'       included only if the `by_port` argument is set to true (not the default).}
#'     \item{`n`}{Sample size, which is the number of records in a given sample that contain finite values.}
#'     \item{`nna`}{Number of missing values that were stripped before the statistic was computed.}
#'     \item{`start_dt`}{Start date for the period of record.}
#'     \item{`end_dt`}{End date for the period of record.}
#'     \item{`duration`}{Duration of the record period, measured in years.}
#'     \item{`mean`}{Arithmetic mean of the variable values.}
#'     \item{`sd`}{Standard deviation of the variable values.}
#'     \item{`min`}{Minimum of the variable values.}
#'     \item{`qu_1st`}{First quartile of the variable values.}
#'     \item{`median`}{Median of the variable values.}
#'     \item{`qu_3rd`}{Third quartile of the variable values.}
#'     \item{`max`}{Maximum of the variable values.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`get_head_outliers`] function may be used to identify fluid pressure outliers.
#'
#' @export
#'
#' @examples
#' d <- get_head_stats()
#' str(d)
#'
#' d <- get_head_stats(vars = "press_va", by_port = TRUE)
#' str(d)

get_head_stats <- function(vars = NULL, by_port = FALSE) {

  # get pressure metadata
  meta <- get_press_metadata()

  # check arguments
  checkmate::assert_subset(vars, choices = names(meta))
  checkmate::assert_flag(by_port)

  # set variables
  if (is.null(vars)) {
    vars <- names(meta)
  }

  # get summary statistics
  l <- lapply(vars,
    FUN = function(var_nm) {
      digits <- meta[[var_nm]]$digits
      d <- aggregate_data(var_nm, digits, by_port)
      var_ds <- meta[[var_nm]]$var_ds
      data.frame("var_ds" = var_ds, "var_nm" = var_nm, d)
    }
  )

  # bind data
  d <- do.call(rbind, l)

  # set classes
  d$var_nm <- as.factor(d$var_nm)
  d$var_ds <- as.factor(d$var_ds)
  d$site_nm <- as.factor(d$site_nm)
  d$n <- as.integer(d$n)
  d$nna <- as.integer(d$nna)

  # remove row names
  rownames(d) <- NULL

  d
}


# Function to aggregate data
aggregate_data <- function(var_nm, digits = 2, by_port = TRUE) {

  # load datasets
  file <- "data/heads.rda"
  if (checkmate::test_file_exists(file, access = "r")) {
    load(file)
  } else {
    utils::data("heads", package = "mlms")
  }

  # check arguments
  checkmate::assert_choice(var_nm, choices = colnames(heads))
  checkmate::assert_count(digits)
  checkmate::assert_flag(by_port)

  # get unique site names
  site_names <- heads$site_nm |> unique() |> sort()

  # get descriptive statistics
  l <- lapply(site_names,
    FUN = function(site_nm) {
      is_site <- heads$site_nm == site_nm

      # aggregate by site and port
      if (by_port) {
        port_numbers <- heads$port_nu[is_site]
        unique_ports <- unique(port_numbers) |> sort(decreasing = TRUE)
        l <- lapply(unique_ports,
          FUN = function(port_nu) {
            is_port <- port_numbers == port_nu
            var_dt <- heads[["press_dt"]][is_site][is_port]
            var_va <- heads[[var_nm]][is_site][is_port]
            get_statistics(var_dt, var_va, digits = digits)
          }
        )
        d <- do.call(rbind, args = l)
        d <- data.frame("port_nu" = unique_ports, d)

      # aggregate by site only
      } else {
        var_va <- heads[[var_nm]][is_site]
        var_dt <- heads[["press_dt"]][is_site]
        d <- get_statistics(var_dt, var_va, digits = digits)
      }

      data.frame("site_nm" = site_nm, d)
    }
  )

  # bind data
  do.call(rbind, args = l)
}


# Function to retrieve descriptive statistics
get_statistics <- function(var_dt, var_va, digits = 2) {

  # check arguments
  checkmate::assert_posixct(var_dt)
  checkmate::assert_numeric(var_va, len = length(var_dt))
  checkmate::assert_count(digits)

  # initialize list
  l <- list()

  # remove missing values
  is_na <- is.na(var_va)
  var_va <- var_va[!is_na]
  var_dt <- var_dt[!is_na]

  # add number of observations
  l$n <- length(var_va)

  # add number of NA values
  l$nna <- sum(is_na)

  # add add period of record
  l$start_dt <- min(var_dt) |> as.Date()
  l$end_dt <- max(var_dt) |> as.Date()
  duration <- as.double(l$end_dt - l$start_dt) / 365
  l$duration <- round_usgs(duration, digits = 1)

  # add mean values
  l$mean <- mean(var_va) |> round_usgs(digits = digits)

  # add standard deviation
  l$sd <- stats::sd(var_va) |> round_usgs(digits = digits)

  # add quantiles
  qu <- stats::quantile(var_va) |> round_usgs(digits = digits) |> as.list()
  names(qu) <- c("min", "qu_1st", "median", "qu_3rd", "max")
  l <- c(l, qu)

  # convert to data frame
  data.frame(l)
}
