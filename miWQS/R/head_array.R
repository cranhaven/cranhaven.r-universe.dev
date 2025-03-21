# #' Head and Summary of an Array
# #' #Copy from makeJournal Tables
# #'
# #' @name head.array
# #' @family statistics
# #' @keywords array
# #'
# #' @description Two functions that allow to see part of (head) or summarize an array, across the 3rd
# #'  dimension.
# #' @details '\code{head.array} the head of the first two dimensions in every level of the array.
# #' '\code{summary.array} the summary of dimensions across level of the array.
# #'
# #'
# #' @param x a 3-dimensional array
# #' @param object a 3-dimensional array
# #' @param ... additional parameters passed to \code{\link[utils]{head}}.
# #'
# #' @examples
# #' #An array. (In reality, the array would have many more rows than this example. )
# #' v <- c(rnorm(20, 0, 3), rnorm(20, 1, 3),  rgamma(20, 0, 3), rgamma(20, 1, 3) )
# #' result <- array( v , dim = c(20, 2, 2))
# #'
# #' head(result)
# #' summary(result)
# #'
# #' @import utils
# #' #head function only
# #' @export
head.array <- function(x, ...) { apply(x, 2:3, head, ...) }

# #'@rdname head.array
# #'@export
summary.array <- function(object, ...) { apply(object, 2:3, summary, ...) }

# Functions Used to Simulate Data in Simulation Study
# Step 2: Functions to Conduct Univariate Bayesian MI
