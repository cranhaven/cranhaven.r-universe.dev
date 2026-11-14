## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(nevada)

## -----------------------------------------------------------------------------
x <- nvd(model = "gnp", n = 3, model_params = list(p = 1/3))
repr_nvd(x, representation = "laplacian")

## -----------------------------------------------------------------------------
x <- nvd(model = "gnp", n = 3, model_params = list(p = 1/3))
dist_nvd(x, representation = "laplacian", distance = "hamming")

## ---- eval=FALSE--------------------------------------------------------------
#  #' Test Statistic for the Two-Sample Problem
#  #'
#  #' This function computes the test statistic...
#  #'
#  #' @param data A list storing the concatenation of the two samples from which
#  #'   the user wants to make inference. Alternatively, a distance matrix stored
#  #'   in an object of class \code{\link[stats]{dist}} of pairwise distances
#  #'   between data points.
#  #' @param indices1 An integer vector that contains the indices of the data
#  #'   points belong to the first sample in the current permuted version of the
#  #'   data.
#  #'
#  #' @return A numeric value evaluating the desired test statistic.
#  #' @export
#  #'
#  #' @examples
#  #' # TO BE DONE BY THE DEVELOPER OF THE PACKAGE
#  stat_{{{name}}} <- function(data, indices1) {
#    n <- if (inherits(data, "dist"))
#      attr(data, "Size")
#    else if (inherits(data, "list"))
#      length(data)
#    else
#      stop("The `data` input should be of class either list or dist.")
#  
#    indices2 <- seq_len(n)[-indices1]
#  
#    x <- data[indices1]
#    y <- data[indices2]
#  
#    # Here comes the code that computes the desired test
#    # statistic from input samples stored in lists x and y
#  
#  }

## ---- eval=FALSE--------------------------------------------------------------
#  stat_student <- function(data, indices1) {
#    n <- if (inherits(data, "dist"))
#      attr(data, "Size")
#    else if (inherits(data, "list"))
#      length(data)
#    else
#      stop("The `data` input should be of class either list or dist.")
#  
#    indices2 <- seq_len(n)[-indices1]
#  
#    x <- data[indices1]
#    y <- data[indices2]
#  
#    # Here comes the code that computes the desired test
#    # statistic from input samples stored in lists x and y
#    x <- unlist(x)
#    y <- unlist(y)
#  
#    stats::t.test(x, y, var.equal = TRUE)$statistic
#  }

## -----------------------------------------------------------------------------
x <- nvd(model = "gnp", n = 10, model_params = list(p = 1/3))
y <- nvd(model = "k_regular" , n = 10, model_params = list(k = 8L))
test2_global(
  x = x, 
  y = y, 
  representation = "laplacian", 
  distance = "frobenius", 
  stats = c("flipr:student_ip", "flipr:fisher_ip"), 
  seed = 1234
)$pvalue

