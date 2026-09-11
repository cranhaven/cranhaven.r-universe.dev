#' @name fuzzyHRT
#' @aliases fuzzyHRT
#' @title Calculate Cellwise Flags for Anomaly Detection
#' @description
#' The function uses fuzzy logic to determine if a data entry is an outlier or not.
#' The function takes a long-format \code{data.frame} object as input and returns it with two appended vectors.
#' The first vector contains the anomaly scores as numbers between zero and one, and the second vector provides
#' a set of logical values indicating whether the data entry is an outlier (\code{TRUE}) or not (\code{FALSE}).
#' @usage fuzzyHRT(a, contamination = 0.08)
#' @param a A long-format \code{data.frame} object with survey data. For details see information on the data format.
#' @param contamination A number between zero and one used as a threshold when identifying outliers from the fuzzy scores.
#' By default, the algorithm will identify 8\% of the records as anomalies.
#' @details
#' The argument \code{a} is proivded as an object of class \code{data.frame}.
#' This object is considered as a long-format \code{data.frame}, and it must have at least five columns with the following names:
#' \describe{
#'   \item{\code{"strata"}}{a \code{character} or \code{factor} column containing the information on the stratification.}
#'   \item{\code{"unit_id"}}{a \code{character} or \code{factor} column containing the ID of the statistical unit in the survey sample(x, size, replace = FALSE, prob = NULL).}
#'   \item{\code{"master_varname"}}{a \code{character} column containing the name of the observed variable.}
#'   \item{\code{"current_value_num"}}{a \code{numeric} the observed value, i.e., a data entrie}
#'   \item{\code{"pred_value"}}{a \code{numeric} a value observed on a previous survey for the same variable if available. If not available, the value can be set to \code{NA} or \code{NaN}. When working with longitudinal data, the value can be set to a time-series forecast or a filtered value.}}
#' The \code{data.frame} object in input can have more columns, but the extra columns would be ignored in the analyses.
#' However, these extra columns would be preserved in the system memory and returned along with the results from the cellwise outlier-detection analysis.
#' The use of the R-packages \code{dplyr}, \code{purrr}, and \code{tidyr} is highly recommended to simplify the conversion of datasets between long and wide formats.
#' @return The long-format \code{data.frame} is provided as input data and contains extra columns i.e., anomaly flags and outlier indicators columns.
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples
#' \dontrun{
#' # Load the package
#' library(HRTnomaly)
#' set.seed(2025L)
#' # Load the 'toy' data
#' data(toy)
#' # Detect cellwise outliers
#' res <- fuzzyHRT(toy[sample.int(100), ])
#' }
#' @keywords outliers
#' @keywords distribution
#' @keywords probability
NULL
fuzzyHRT <- function(a, contamination = 0.08) {

  ## Historical and zero check
  hScore <- .C("history_check", double(nrow(a)), double(nrow(a)),
               as.double(a$current_value_num),
               as.double(a$pred_value), nrow(a),
               NAOK = TRUE, DUP = TRUE, PACKAGE = "HRTnomaly")[1L:2L]
  zScore <- hScore[[2L]]
  hScore <- hScore[[1L]]

  ## Tail-check
  dtac <- a[, c("strata", "unit_id", "master_varname", "current_value_num")] %>%
    pivot_wider(names_from = any_of("master_varname"),
                values_from = matches("current_value_num"))
  dtac[dtac <= 0] <- NA
  dtal <- as.matrix(log(dtac[, -1L:-2L]))

  gr <- factor(dtac$strata)
  # Smat <- double(prod(dim(dtal)))
  tScore <- .C("tail_check", as.double(dtal), dim(dtal),
               gr, nlevels(gr), res = double(prod(dim(dtal))),
               NAOK = TRUE, PACKAGE = "HRTnomaly")$res

  ## Relational-check
  rScore <- 1
  dtae <- .C("normalize", as.double(dtal), dim(dtal),
             gr, nlevels(gr), res = double(prod(dim(dtal))),
             NAOK = TRUE, PACKAGE = "HRTnomaly")$res
  dtae[is.na(dtae)] <- 0
  rScore <- .C("relat_check", dtae = as.double(dtae),
               dim(dtal), PACKAGE = "HRTnomaly")$dtae
  rScore <- array(rScore, dim = dim(dtal))

  ## Putting things together using a Fuzzy-Logic-Inspired procedure
  dtac[, -1L:-2L] <- array(rScore * tScore, dim = dim(dtal))
  dtar <- dtac %>% pivot_longer(2 + seq_len(ncol(dtal)), values_drop_na = TRUE)

  dtar <- dtac %>% pivot_longer(cols = 3:dim(dtac)[2],
                                names_to = "master_varname",
                                values_to = "rScore")
  dtar <- left_join(a, dtar)

  a$score <- zScore * hScore * dtar$rScore
  th <- quantile(a$score[a$score != 0], contamination, na.rm = TRUE)
  a$outlier <- a$score < th & a$score != 0

  return(a)
}
