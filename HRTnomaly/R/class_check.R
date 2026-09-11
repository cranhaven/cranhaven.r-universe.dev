#' @name class_check
#' @aliases class_check
#' @title Evaluate the Accuracy of Outlier Classification Results
#' @description
#' The function computes the confusion matrix between the logical output of an outlier detection algorithm and a reference (ground-truth) logical vector.
#' The function also calculates the overall accuracy of the results from the confusion matrix, including recall, precision, and F1-scores for the two classes (regular, versus outlier).
#' @usage class_check(pred, truth)
#' @param pred A logical vector with the classification output from an anomaly detection algorithm.
#' @param truth A logical vector with the observed classification as a reference (or ground truth).
#' @details
#' The function computes the confusion matrix using the function \code{table}. True positive and false negative are successively evaluated to compute overall accuracy, recall, precision, and F1-scores.
#' @return An S3 class named \code{checkwise} with the confusion matrix, and other accuracy metrices appended as attribues.
#' @return \code{attr(, "overall")} A numeric value between zero and one with the overall accuracy.
#' @return \code{attr(, "recall")} A numeric vector of values between zero and one with the recall index for regular and outlier cells.
#' @return \code{attr(, "precision")} A numeric vector of values between zero and one with the precision index for regular and outlier cells.
#' @return \code{attr(, "f1-score")} A numeric vector of values between zero and one with the F1-scores for regular and outlier cells.
#' @author Luca Sartore \email{drwolf85@gmail.com}
#' @examples
#' \dontrun{
#' # Load the package
#' library(HRTnomaly)
#' set.seed(2025L)
#' # Load the 'toy' data
#' data(toy)
#' # Detect cellwise outliers using Cellwise Analysis
#' res <- cellwise(toy[sample.int(100), ], 0.05, 2L)
#' class_check(res$outlier, res$anomaly_flag != "")
#' }
#' @keywords outliers
#' @keywords distribution
#' @keywords probability
#' @export
class_check <- function(pred, truth) {

  tb <- table(pred, truth)
  attr(tb, "overall") <- sum(diag(tb)) / sum(tb)
  attr(tb, "recall") <- diag(apply(tb, 1, function(xx) xx / sum(xx)))
  attr(tb, "precision") <- diag(apply(tb, 2, function(xx) xx / sum(xx)))
  attr(tb, "f1-score") <- 1 /
    (0.5 / attr(tb, "recall") + 0.5 / attr(tb, "precision"))
  tb <- unclass(tb)
  class(tb) <- "checkwise"
  return(tb)
}
