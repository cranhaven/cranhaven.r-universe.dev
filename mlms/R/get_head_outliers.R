#' Detect Hydraulic Head Outliers
#'
#' @description Detect outliers in the fluid pressure data by comparing parameter values
#'   against statistical metrics derived from the [`heads`] dataset.
#'   This function will employ both the standard score (z-score) and Interquartile Range (IQR)
#'   methods for outlier identification.
#'
#' @param data 'data.frame' table.
#'   Depth-discrete measurements of fluid pressure and temperature, hydraulic head values,
#'   and land-surface atmospheric pressure measurements.
#'   Defaults to the [`heads`] dataset.
#' @param method 'character' string.
#'   Outlier detection method.
#'   Specify "z-score" (default) for the standard score method, best suited for normally distributed data,
#'   or "IQR" to use the IQR method, which is ideal for skewed distributions.
#' @param threshold 'numeric' number.
#'   Z-score value used to determine whether a parameter value is considered
#'   an outlier or significantly different from the historic mean value.
#' @param multiplier 'numeric' number.
#'   Multiplier used to determine the threshold for outliers in the IQR method.
#' @param min_n 'integer' number.
#'   Minimum sample size needed to detect outliers.
#' @param quiet 'logical' flag.
#'   Whether to suppress printing of outlier information.
#' @inheritParams get_head_stats
#'
#' @return A data frame with the following variables:
#'   \describe{
#'     \item{`var_ds`}{Variable description.}
#'     \item{`var_nm`}{Variable name as specified in the `vars` argument.}
#'     \item{`site_nm`}{Local site name for a MLMS well.}
#'     \item{`port_nu`}{Identifier for the valved measurement port,
#'       included only if the `by_port` argument is set to true (not the default).}
#'     \item{`press_dt`}{Time at which measurements were measured outside the multiport casing.}
#'     \item{`var_va`}{Parameter value.}
#'     \item{`z_score`}{Z-score, a statistical measure that indicates how many standard deviations
#'       a parameter value is from the mean.}
#'     \item{`qu_1st`}{First quartile (25th percentile) of historical parameter values.}
#'     \item{`qu_3rd`}{Third quartile (75th percentile) of historical parameter values.}
#'     \item{`n`}{Sample size.}
#'   }
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso [`get_head_stats`] function is used to calculate the fluid pressure statistics.
#'
#' @export
#'
#' @examples
#' d <- get_head_outliers()
#' str(d)
#'
#' d <- get_head_outliers(method = "IQR")
#' str(d)

get_head_outliers <- function(data = mlms::heads,
                              vars = NULL,
                              method = c("z-score", "IQR"),
                              threshold = 3,
                              multiplier = 1.5,
                              min_n = 10L,
                              quiet = TRUE) {

  # get pressure metadata
  meta <- get_press_metadata()

  # check arguments
  checkmate::assert_data_frame(data)
  checkmate::assert_subset(vars, choices = names(meta))
  method <- match.arg(method)
  checkmate::assert_number(threshold, lower = 0, finite = TRUE)
  checkmate::assert_number(multiplier, lower = 0, finite = TRUE)
  checkmate::assert_count(min_n, positive = TRUE)
  checkmate::assert_flag(quiet)

  # set variables
  if (is.null(vars)) {
    vars <- names(meta)
  }

  # get outliers
  l <- lapply(vars,
    FUN = function(var_nm) {
      d <- get_stat_measures(data, var_nm)
      if (is.null(d)) return(NULL)
      var_ds <- meta[[var_nm]]$var_ds
      data.frame("var_ds" = var_ds, "var_nm" = var_nm, d)
    }
  )

  # remove null components
  l <- Filter(Negate(is.null), l)
  if (length(l) == 0) return(invisible(NULL))

  # bind data
  d <- do.call(rbind, l)

  # remove records with insufficient statistical power
  is <- d$n >= min_n
  d <- d[is, ]

  # get outliers
  if (method == "z-score") {
    is <- abs(d$z_score) > threshold
  } else {
    iqr <- d$qu_3rd - d$qu_1st
    li <- d$qu_1st - multiplier * iqr
    ui <- d$qu_3rd + multiplier * iqr
    is <- d$var_va < li | d$var_va > ui
  }
  if (sum(is) == 0) return(invisible(NULL))
  d <- d[is, ]

  # remove row names
  rownames(d) <- NULL

  # set classes
  d$var_ds <- as.factor(d$var_ds)
  d$var_nm <- as.factor(d$var_nm)
  d$site_nm <- as.factor(d$site_nm)

  # print outliers
  if (!quiet) {
    txt <- utils::capture.output(d) |> paste(collapse = "\n")
    message("Values identified as outliers:\n", txt)
  }

  invisible(d)
}


# Function to identify a variable's outliers
get_stat_measures <- function(data, var_nm) {

  # check arguments
  checkmate::assert_data_frame(data)
  checkmate::assert_string(var_nm)

  # get statistics
  stats <- get_head_stats(vars = var_nm, by_port = TRUE)

  # add statistics to data
  idxs <- match(
    paste(data$site_nm, data$port_nu),
    paste(stats$site_nm, stats$port_nu)
  )
  is <- is.finite(idxs)
  if (sum(is) == 0) return(invisible(NULL))
  d <- cbind(data[is, ], stats[idxs[is], ])

  # add variable value
  d$var_va <- d[[var_nm]]

  # remove records with missing parameter values or insufficient statistical power
  is <- is.finite(d$var_va)
  if (sum(is) == 0) return(invisible(NULL))
  d <- d[is, ]

  # calculate z-scores
  d$z_score <- (d$var_va - d$mean) / d$sd

  # subset columns
  cols <- c(
    "site_nm",
    "port_nu",
    "press_dt",
    "var_va",
    "z_score",
    "qu_1st",
    "qu_3rd",
    "n"
  )
  d <- d[, cols]

  d
}
