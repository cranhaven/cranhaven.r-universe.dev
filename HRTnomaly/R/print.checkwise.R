#' @name print.checkwise
#' @aliases print.checkwise
#' @title A Method to Print the Accuracy of Outlier Classification Results
#' @description
#' The function prints the confusion matrix and accuracy results previously computed with the function \code{class_check}.
#' @usage \method{print}{checkwise}(x, confusion = FALSE, ...)
#' @param x An S3 object of the class \code{checkwise}, typically computed with the function \code{class_check}.
#' @param confusion A logical value, which is \code{FALSE} by default. If \code{TRUE}, the confusion matrix is printed after showing all accuracy metrics.
#' @param ... Additional arguments to pass to the function \code{cat}.
#' @details
#' The function computes the confusion matrix using the function \code{table}. True positive and false negative are successively evaluated to compute overall accuracy, recall, precision, and F1-scores.
#' @return An S3 class named \code{checkwise} with the confusion matrix, and other accuracy metrics appended as attribues.
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
#' # Detect cellwise outliers using Bayesian Analysis
#' res <- cellwise(toy[sample.int(100), ], 0.5, 10L)
#' class_check(res$outlier, res$anomaly_flag != "")
#' }
#' @keywords outliers
#' @keywords distribution
#' @keywords probability
#' @export
print.checkwise <- function(x, confusion = FALSE, ...) {

  cat("  Overall accuracy:\n", ...)
  cat(attr(x, "overall"), "\n", sep = "", ...)
  cat("  Recall:\n", ...)
  print(attr(x, "recall"))
  cat("  Precision:\n", ...)
  print(attr(x, "precision"))
  cat("  F1-score:\n", ...)
  print(attr(x, "f1-score"))
  if (confusion) {
    cat("  Confusion:\n", ...)
    print(as.table(x))
  }
}
