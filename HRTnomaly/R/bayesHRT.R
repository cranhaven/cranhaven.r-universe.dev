#' @name bayesHRT
#' @aliases bayesHRT
#' @title Calculate Cellwise Flags for Anomaly Detection Using Bayesian Testing
#' @description
#' The function uses the predictive posterior distribution based on empirical likelihoods to determine if a data entry is an outlier or not.
#' The function takes a long-format \code{data.frame} object as input and returns it with two appended vectors.
#' The first vector contains the posterior probabilities as numbers between zero and one, and the second vector provides
#' a set of logical values indicating whether the data entry is an outlier (\code{TRUE}) or not (\code{FALSE}).
#' @usage bayesHRT(a, prior = NULL)
#' @param a A long-format \code{data.frame} object with survey data. For details see information on the data format.
#' @param prior A numerical value or vector of cell-level prior probabilities of observing an outlier. It is \code{NULL} by default. If false, the function searches for a column named \code{"prior"} within the dataset. If such column is not provided in the dataset, a \code{0.5} non-informative value is used for all cells.
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
#'     \item{post_prob}{The posterior probability of the value being an outlier.}
#'     \item{outlier}{A boolean indicating whether the value is an outlier.}
#'     \item{anomaly_flag}{A character string indicating the type of anomaly detected, if any.}
#'   }
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples
#' # Load the package
#' library(HRTnomaly)
#' set.seed(2025L)
#' # Load the 'toy' data
#' data(toy)
#' # Detect cellwise outliers
#' res <- bayesHRT(toy[sample.int(100), ], prior = 0.5)
#' @keywords outliers distribution probability
#' @export
bayesHRT <- function(a, prior = NULL) {
  # Check if the dataset `a` contains values for the prior of each cell
  if (is.null(prior)) {
    if ("prior" %in% colnames(a)) {
        prior <- 1 - a$prior # This is cell-level prior probability for regular cases!!!
    } else {
      # Use non informative prior if the dataset `a` does not contain prior probabilities for each cell
      prior <- 0.5 # This is cell-level prior probability for regular cases!!!
    }
  } else {
    prior <- 1 - prior # This is cell-level prior probability for regular cases!!!
  }
  ## Historical residual and zero check
  hRes <- .C("history_res", double(nrow(a)), double(nrow(a)),
             as.double(a$current_value_num),
             as.double(a$pred_value), nrow(a),
             NAOK = TRUE, DUP = TRUE, PACKAGE = "HRTnomaly")[1L:2L]
  zScore <- hRes[[2L]] * prior # Used as priors for each cell
  hRes <- hRes[[1L]]
  dtah <- cbind.data.frame(a[, c("strata", "unit_id", "master_varname")], hRes, zScore)
  dtaz <- dtah[, -4L] %>% pivot_wider(names_from = any_of("master_varname"), values_from = matches("zScore"))
  dtah <- dtah[, -5L] %>% pivot_wider(names_from = any_of("master_varname"), values_from = matches("hRes"))
  dtah[, -1L:-2L][is.na(dtah[, -1L:-2L])] <- 0

  ## Tail-check
  dtac <- a[, c("strata", "unit_id", "master_varname", "current_value_num")] %>%
          pivot_wider(names_from = any_of("master_varname"), values_from = matches("current_value_num"))
  dtac[dtac <= 0] <- NA
  dtal <- as.matrix(log(dtac[, -1L:-2L]))

  gr <- factor(dtac$strata)

  tRes <- .C("tail_res", as.double(dtal), dim(dtal),
             gr, nlevels(gr), res = double(prod(dim(dtal))),
             NAOK = TRUE, PACKAGE = "HRTnomaly")$res
  tRes <- array(tRes, dim = dim(dtal))
  tRes[is.na(tRes)] <- 0

  ## Relational-check
  rRes <- 0
  dtae <- tRes
  rRes <- .C("relat_res", dtae = as.double(dtae),
             dim(dtal), PACKAGE = "HRTnomaly")$dtae
  rRes <- array(rRes, dim = dim(dtal))

  ## Putting things together using the highest posterior probability class
  pr_mat <- as.matrix(dtaz[, -1L:-2L])
  hRes <- as.matrix(dtah[, -1L:-2L])
  finals <- .C("post_results", as.double(pr_mat), integer(prod(dim(dtal))),
               dim(dtal), as.double(hRes), as.double(rRes), as.double(tRes),
               NAOK = TRUE, PACKAGE = "HRTnomaly")[1L:2L]
  pr_mat <- array(finals[[1L]], dim = dim(dtal)) # Outlier posterior probability!!!
  finals <- array(as.logical(finals[[2L]]), dim = dim(dtal)) # Outlier flag (if TRUE then outlier)

  ## Putting things together using a Fuzzy-Logic-Inspired procedure
  dtac[, -1L:-2L] <- array(pr_mat, dim = dim(dtal))
  # dtar <- dtac %>% pivot_longer(2 + seq_len(ncol(dtal)), values_drop_na = TRUE)
  dtar <- dtac %>% pivot_longer(cols = 3:dim(dtac)[2],
                                names_to = "master_varname",
                                values_to = "post_prob")
  a <- left_join(a, dtar)
  dtac[, -1L:-2L] <- finals
  dtar <- dtac %>% pivot_longer(cols = 3:dim(dtac)[2],
                                names_to = "master_varname",
                                values_to = "outlier")
  a <- left_join(a, dtar)
  return(a)
}
