#' @name bayeswise
#' @aliases bayeswise
#' @title Calculate Cellwise Flags for Anomaly Detection Using Robust Bayesian Methods
#' @description
#' The function uses a Bayesian approach to determine if a data entry is an outlier or not.
#' The function takes a long-format \code{data.frame} object as input and returns it with two appended vectors.
#' The first vector contains the posterior probabilities for a cell to be anomalous, and the second vector provides
#' a set of logical values indicating whether the data entry is an outlier (\code{TRUE}) or not (\code{FALSE}).
#' @usage bayeswise(a, prior = NULL, epochs = 1000L)
#' @param a A long-format \code{data.frame} object with survey data. For details see information on the data format.
#' @param prior A numerical value or vector of cell-level prior probabilities of observing an outlier. It is \code{NULL} by default. If false, the function searches for a column named \code{"prior"} within the dataset. If such column is not provided in the dataset, a \code{0.5} non-informative value is used for all cells.
#' @param epochs Number of epochs used to train a nontrivial robust linear model via the lion algorithm. By default, the algorithm will run 1000 iterations.
#' @details
#' The argument \code{a} is provided as an object of class \code{data.frame}.
#' This object is considered as a long-format \code{data.frame}, and it must have at least five columns with the following names:
#' \describe{
#'   \item{\code{"strata"}}{a \code{character} or \code{factor} column containing the information on the stratification.}
#'   \item{\code{"unit_id"}}{a \code{character} or \code{factor} column containing the ID of the statistical unit in the survey sample(x, size, replace = FALSE, prob = NULL).}
#'   \item{\code{"master_varname"}}{a \code{character} column containing the name of the observed variable.}
#'   \item{\code{"current_value_num"}}{a \code{numeric} the observed value, i.e., a data entry}
#'   \item{\code{"pred_value"}}{a \code{numeric} a value observed on a previous survey for the same variable if available. If not available, the value can be set to \code{NA} or \code{NaN}. When working with longitudinal data, the value can be set to a time-series forecast or a filtered value.}
#'   \item{\code{"prior"}}{a \code{numeric} a value of prior probabilities of observing an outlier for the cell. If this column is omitted in the dataset provided, the function will use the values provided through the argument \code{prior}.}}
#' The \code{data.frame} object in input can have more columns, but the extra columns would be ignored in the analyses.
#' However, these extra columns would be preserved in the system memory and returned along with the results from the cellwise outlier-detection analysis.
#' The use of the R-packages \code{dplyr}, \code{purrr}, and \code{tidyr} is highly recommended to simplify the conversion of datasets between long and wide formats.
#' @return A data frame with the same columns as the input data frame, plus the following additional columns:
#'   \describe{
#'     \item{prior}{The prior probability used for the cell (either input or derived).}
#'     \item{zScore}{The z-score of the cell.}
#'     \item{hScore}{The h-score of the cell.}
#'     \item{rScore}{The r-score of the cell.}
#'     \item{tScore}{The t-score of the cell.}
#'     \item{score}{The final outlier score of the cell, representing the posterior probability of being anomalous.}
#'     \item{outlier}{A boolean indicating whether the cell is an outlier.}
#'     \item{anomaly_flag}{A character string indicating the type of anomaly detected, if any.}
#'   }
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples
#' # Load the package
#' library(HRTnomaly)
#' set.seed(2025L)
#' # Load the 'toy' data
#' data(toy)
#' # Detect cellwise outliers using Bayesian Analysis
#' res <- bayeswise(toy[sample.int(100), ], 0.5, 10L)
#' @keywords outliers distribution probability
#' @export
bayeswise <- function(a, prior = NULL, epochs = 1000L) {
  if (!is.numeric(epochs) || !is.finite(epochs))
    stop("The argument `epochs` must be a finite integer number")
  # Check if the dataset `a` contains values for the prior of each cell
  if (is.null(prior)) {
    if ("prior" %in% colnames(a)) {
      prior <- 1 - a$prior # Cell-level prior probability for regular cases!!!
    } else {
      # Non informative prior if `a` has no prior probabilities for each cell
      prior <- 0.5 # This is cell-level prior probability for regular cases!!!
    }
  } else {
    prior <- 1 - prior # Cell-level prior probability for regular cases!!!
  }
  a$prior <- prior

  storage.mode(epochs) <- "integer"
  wh <- c("strata", "unit_id", "master_varname")
  dtac <- a[, c(wh, "current_value_num")] %>%
    pivot_wider(names_from = any_of("master_varname"),
                values_from = matches("current_value_num"))
  ord <- order(dtac$strata)
  dtac <- dtac[ord, ]

  dtap <- a[, c(wh, "pred_value")] %>%
    pivot_wider(names_from = any_of("master_varname"),
                values_from = matches("pred_value"))
  ord <- order(dtap$strata)
  dtap <- dtap[ord, ]

  dtas <- a[, c(wh, "prior")] %>%
    pivot_wider(names_from = any_of("master_varname"),
                values_from = matches("prior"))
  ord <- order(dtas$strata)
  dtas <- dtas[ord, ]

  xc <- as.matrix(dtac[, -1L:-2L]) # Current
  xp <- as.matrix(dtap[, -1L:-2L]) # Past
  s <- as.matrix(dtas[, -1L:-2L]) # Prior Prob.
  g <- matrix(0, nrow(xc), ncol(xc))
  z <- matrix(0, nrow(xc), ncol(xc))
  h <- matrix(0, nrow(xc), ncol(xc))
  r <- matrix(0, nrow(xc), ncol(xc))
  t <- matrix(0, nrow(xc), ncol(xc))
  storage.mode(g) <- "integer"
  storage.mode(s) <- "double"
  scores <- .C("bayeswise", s = s, G = g, z = z, h = h, r = r, t = t, xc, xp,
               dim(xc), epochs, NAOK = TRUE, PACKAGE = "HRTnomaly")
  scores$s <- as.data.frame(scores$s)
  scores$G <- as.data.frame(scores$G)
  scores$z <- as.data.frame(scores$z)
  scores$h <- as.data.frame(scores$h)
  scores$r <- as.data.frame(scores$r)
  scores$t <- as.data.frame(scores$t)
  scores$s$unit_id <- dtac$unit_id
  scores$G$unit_id <- dtac$unit_id
  scores$z$unit_id <- dtac$unit_id
  scores$h$unit_id <- dtac$unit_id
  scores$r$unit_id <- dtac$unit_id
  scores$t$unit_id <- dtac$unit_id
  names(scores$s) <- c(colnames(xc), "unit_id")
  names(scores$G) <- c(colnames(xc), "unit_id")
  names(scores$z) <- c(colnames(xc), "unit_id")
  names(scores$h) <- c(colnames(xc), "unit_id")
  names(scores$r) <- c(colnames(xc), "unit_id")
  names(scores$t) <- c(colnames(xc), "unit_id")
  scores$s <- scores$s %>%
    pivot_longer(seq_len(ncol(xc)), values_drop_na = TRUE)
  scores$G <- scores$G %>%
    pivot_longer(seq_len(ncol(xc)), values_drop_na = TRUE)
  scores$z <- scores$z %>%
    pivot_longer(seq_len(ncol(xc)), values_drop_na = TRUE)
  scores$h <- scores$h %>%
    pivot_longer(seq_len(ncol(xc)), values_drop_na = TRUE)
  scores$r <- scores$r %>%
    pivot_longer(seq_len(ncol(xc)), values_drop_na = TRUE)
  scores$t <- scores$t %>%
    pivot_longer(seq_len(ncol(xc)), values_drop_na = TRUE)
  names(scores$s)[-1L] <- c("master_varname", "score")
  names(scores$G)[-1L] <- c("master_varname", "outlier")
  names(scores$z)[-1L] <- c("master_varname", "zScore")
  names(scores$h)[-1L] <- c("master_varname", "hScore")
  names(scores$r)[-1L] <- c("master_varname", "rScore")
  names(scores$t)[-1L] <- c("master_varname", "tScore")
  a <- a %>% inner_join(scores$z, by = c("master_varname", "unit_id"))
  a <- a %>% inner_join(scores$h, by = c("master_varname", "unit_id"))
  a <- a %>% inner_join(scores$r, by = c("master_varname", "unit_id"))
  a <- a %>% inner_join(scores$t, by = c("master_varname", "unit_id"))
  a <- a %>% inner_join(scores$s, by = c("master_varname", "unit_id"))
  a <- a %>% inner_join(scores$G, by = c("master_varname", "unit_id"))
  a$outlier <- as.logical(a$outlier)
  return(a)
}
