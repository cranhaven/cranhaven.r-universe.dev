#' Printing function for objects of class gofCOP
#' 
#' \code{\link{print.gofCOP}} prints the values of an object of class gofCOP.
#' 
#' @method print gofCOP
#' 
#' @param x An object of \code{class} gofCOP.
#' @param ...  Further arguments to be passed over. Currently non.
#' @return None
#' @export
print.gofCOP <- function(x, ...) {
  for (j in seq_along(x)) {
    cat(rep("-", getOption("width")), sep = "")
    cat("\n")
    cat(strwrap(x[[j]]$method), sep = "\n")
    cat("\n")
    out <- character()
    if (!is.null(x[[j]]$theta)) {
      for (i in seq_along(x[[j]]$theta)) {
        out <- c(out, paste0("theta.", i, " = ", x[[j]]$theta[i]))
      }
    }
    if (!is.null(x[[j]]$df)) {
      out <- c(out, paste("df =", x[[j]]$df))
    }
    cat("Parameters:")
    cat("\n")
    cat(paste(out, collapse = "\n"))
    cat("\n")
    cat("\n")

    if (!is.null(x[[j]]$df)) {
      if (length(x[[j]]$df) > 1 & !any(x[[j]]$df == 60) | 
          length(x[[j]]$df) == 3) {
cat(
"For the CvM and KS test the df have to be an integer and for this reason were 
transformed."
)
        cat("\n")
        cat("\n")
      }
      if (any(x[[j]]$df == 60)) {
cat(
"For the PIOSTn test the estimated df were too high and for computational 
reasons were fixed to 60."
)
        cat("\n")
        cat("\n")
      }
    }

    if (!is.null(x[[j]]$res.tests)) {
      cat("Tests results:")
      cat("\n")
      print(x[[j]]$res.tests, max = 50)
      if (dim(x[[j]]$res.tests)[1] != 1 & j == length(x)) {
        cat("\n")
cat(
"Please use the functions gofGetHybrid() and gofOutputHybrid() for display of 
subsets of Hybrid tests. To access the results, please obtain them from the 
structure of the gofCOP object."
)
      }
      if (any(is.na(x[[j]]$res.tests[, 1]))) {
        cat("\n")
cat(bold(
"At least one p-value was returned as 'NA'. Typically this is due to the copula 
parameter being estimated close to the boundaries. Consider adjusting the 
possible parameter space via 'lower' and 'upper'."
))
      }
    }
    cat("\n")
    cat("\n")
    if (is.element("White", rownames(x[[j]]$res.tests)) & 
        x[[j]]$copula == "t") {
cat(
"The test gofWhite may be unstable for t-copula. Please handle the results 
carefully."
)
      cat("\n")
      cat("\n")
    }
  }
  invisible(x)
}
