#' @name bootHRT
#' @aliases bootHRT
#' @title Calculate Cellwise Flags for Anomaly Detection Using Bayesian Bootstrap
#' @description
#' The function uses Bayesian bootstrap to determine if a data entry is an outlier or not.
#' The function takes a long-format \code{data.frame} object as input and returns it with appended vectors.
#' The output includes flags for different quantiles and means of the contamination threshold distribution.
#' @usage bootHRT(a, contamination = 0.08, boot_max_it = 1000L)
#' @param a A long-format \code{data.frame} object with survey data. For details see information on the data format.
#' @param contamination A number between zero and one used as a threshold when identifying outliers from the fuzzy scores.
#' By default, the algorithm will identify approximately 8\% of the data entries as anomalies.
#' @param boot_max_it An integer number determining the iterations performed by Bayesian bootstrap algorithm. It is set to \code{1000} by default.
#' @details
#' The argument \code{a} is provided as an object of class \code{data.frame}.
#' This object is considered as a long-format \code{data.frame}, and it must have at least five columns with the following names:
#' \describe{
#'   \item{\code{"strata"}}{a \code{character} or \code{factor} column containing the information on the stratification.}
#'   \item{\code{"unit_id"}}{a \code{character} or \code{factor} column containing the ID of the statistical unit in the survey sample(x, size, replace = FALSE, prob = NULL).}
#'   \item{\code{"master_varname"}}{a \code{character} column containing the name of the observed variable.}
#'   \item{\code{"current_value_num"}}{a \code{numeric} the observed value, i.e., a data entry}
#'   \item{\code{"pred_value"}}{a \code{numeric} a value observed on a previous survey for the same variable if available. If not available, the value can be set to \code{NA} or \code{NaN}. When working with longitudinal data, the value can be set to a time-series forecast or a filtered value.}}
#' The \code{data.frame} object in input can have more columns, but the extra columns would be ignored in the analyses.
#' However, these extra columns would be preserved in the system memory and returned along with the results from the cellwise outlier-detection analysis.
#' The use of the R-packages \code{dplyr}, \code{purrr}, and \code{tidyr} is highly recommended to simplify the conversion of datasets between long and wide formats.
#' @return A data frame with the same columns as the input data frame, plus the following additional columns:
#'   \describe{
#'     \item{score}{The raw anomaly score for each cell.}
#'     \item{outlier_1qt}{A boolean indicating if the cell is an outlier based on the first quantile threshold.}
#'     \item{outlier_2qt}{A boolean indicating if the cell is an outlier based on the second quantile (median) threshold.}
#'     \item{outlier_3qt}{A boolean indicating if the cell is an outlier based on the third quantile threshold.}
#'     \item{outlier_1mn}{A boolean indicating if the cell is an outlier based on the mean threshold minus one standard deviation.}
#'     \item{outlier_2mn}{A boolean indicating if the cell is an outlier based on the mean threshold.}
#'     \item{outlier_3mn}{A boolean indicating if the cell is an outlier based on the mean threshold plus one standard deviation.}
#'     \item{anomaly_flag}{A character string indicating the type of anomaly detected, if any (e.g., "h", "t", "r").}
#'   }
#'   The returned object also includes an attribute \code{"thresholds"} which is a numeric vector of length \code{boot_max_it} containing samples from the posterior distribution of the contamination threshold.
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples
#' # Load the package
#' library(HRTnomaly)
#' set.seed(2025L)
#' # Load the 'toy' data
#' data(toy)
#' # Detect cellwise outliers
#' res <- bootHRT(toy, boot_max_it = 10)
#' @keywords outliers distribution probability
#' @export
bootHRT <- function(a, contamination = 0.08, boot_max_it = 1000L) {
  ## Historical and zero check
  hScore <- .C("history_check", double(nrow(a)), double(nrow(a)),
               as.double(a$current_value_num),
               as.double(a$pred_value), nrow(a),
               NAOK = TRUE, DUP = TRUE, PACKAGE = "HRTnomaly")[1L:2L]
  zScore <- hScore[[2L]]
  hScore <- hScore[[1L]]

  ## Tail-check
  dtac <- a[, c("strata", "unit_id", "master_varname", "current_value_num")] %>%
          pivot_wider(names_from = any_of("master_varname"), values_from = matches("current_value_num"))
  dtac[dtac <= 0] <- NA
  dtal <- as.matrix(log(dtac[, -1L:-2L]))
  
  gr <- factor(dtac$strata)
  
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
  #dtar <- dtac %>% pivot_longer(2 + seq_len(ncol(dtal)), values_drop_na = TRUE)
  
  dtar <- dtac %>% pivot_longer(cols = 3:dim(dtac)[2], names_to = "master_varname", values_to = "rScore") #dtac %>% pivot_longer(5 + seq_len(ncol(dtal)), values_drop_na=TRUE)
  dtar <- left_join(a, dtar)

  ## Empirical Bayesian threshold distribution
  a$score <- zScore * hScore * dtar$rScore
  finScores <- na.omit(a$score[a$score!=0])
	storage.mode(contamination) <- "double"
	storage.mode(finScores) <- "double"
	storage.mode(boot_max_it) <- "integer"
	th_v <-.C("bayes_boot", th = double(boot_max_it), 
            boot_max_it, finScores, length(finScores), 
            contamination, PACKAGE = "HRTnomaly")$th
  mth <- mean(th_v)
  th_s <- c(quantile(th_v, c(0.25, 0.5, 0.75)), mth, mth + c(-1, 1) * sd(th_v))
  outly <- sapply(th_s, function(th) a$score < th & a$score != 0)
  colnames(outly) <- paste0("outlier_", c(1:3, 1:3), rep(c("qt", "mn"), each = 3))
  a <- cbind.data.frame(a, outly)
  attr(a, "thresholds") <- th_v

  return(a)
}
