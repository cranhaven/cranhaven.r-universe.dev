#' Applicable gof tests for testing problem
#' 
#' \code{\link{gofTest4Copula}} returns for a given copula and a dimension the
#' applicable implemented tests.
#' 
#' Before performing a gof test on a dataset, it pays out to have a close look
#' at the Scatterplot to receive an idea about the possible type of copula.
#' Afterwards follows the decision about the test. The tests in this package
#' can be used for different types of copulae functions and dimensions. This
#' function is dedicated to help finding the applicable gof tests for the
#' dataset.
#' 
#' @param copula The copula to test for. Possible are the copulae 
#' \code{"normal"}, \code{"t"}, \code{"clayton"}, \code{"gumbel"}, 
#' \code{"frank"}, \code{"joe"}, \code{"amh"}, \code{"galambos"}, 
#' \code{"huslerReiss"}, \code{"tawn"}, \code{"tev"}, \code{"fgm"} and 
#' \code{"plackett"}. For the default \code{NULL}, all tests in the package are
#' returned.
#' @param d The dimension to search for.
#' @return A character vector which consists of the names of the tests.
#' @examples
#' 
#' gofTest4Copula("clayton", d = 2)
#' 
#' gofTest4Copula("gumbel", d = 5)
#' 
#' @export gofTest4Copula
gofTest4Copula <- function(copula = NULL, d = 2) {
  if (length(copula) > 1) {
    stop("The argument 'copula' has to be of length 1.")
  }
  if (is.null(copula)) {
    copula <- "clayton"
  }
  if (!is.numeric(d)) {
    stop("d must be a numeric.")
  }
  if (d <= 1) {
    stop("The dimension d must be 2 or even higher.")
  }
  cops <- lapply(ls(pos = "package:gofCopula"), function(x) {
    try(eval(formals(x)$copula), silent = TRUE)
  })
  cops_pos <- sapply(cops, function(x) {
    any(is.element(x, copula))
  })
  if (!any(cops_pos)) {
    cat("This copula is for no test implemented.", fill = TRUE)
  } else {
    res <- which(unlist(mapply(.gofCheckDim, 
                               test = ls(pos = "package:gofCopula")[cops_pos], 
                               copula = copula)) >= d)
    if (length(res) == 0) {
return(paste0(
"No test is available for dimensionality ", d, " and copula ", copula,"."
))
    } else {
      return(names(res))
    }
  }
}

# internal function to check the available dimensionality for a specific test. 
.gofCheckDim <- function(test, copula) {
  if (is.element(copula, c("normal", "t", "clayton", "gumbel", "frank", 
                           "joe", "amh"))) {
    if (is.element(test, c("gofRosenblattSnB", "gofRosenblattSnC", 
                           "gofRosenblattChisq", "gofRosenblattGamma", "gofCvM", 
                           "gofKendallCvM", "gofKendallKS", "gofCustomTest", 
                           "gofArchmSnB", "gofArchmSnC", "gofArchmChisq", 
                           "gofArchmGamma", "gofKS")) & copula != "amh") {
      return(Inf)
    } else if (is.element(test, c("gofKernel", "gofWhite")) || copula == "amh") {
      return(2)
    } else if (is.element(test, c("gofPIOSRn", "gofPIOSTn")) & copula == "t") {
      return(2)
    } else if (is.element(test, c("gofPIOSRn", "gofPIOSTn")) & copula != "amh") {
      return(3)
    } else {
      return(NULL)
    }
  } else if (is.element(copula, c("galambos", "huslerReiss", "tawn", "tev", 
                                  "fgm", "plackett"))) {
    if (is.element(test, c("gofRosenblattSnB", "gofRosenblattSnC", 
                           "gofRosenblattChisq", "gofRosenblattGamma", "gofCvM", 
                           "gofKendallCvM", "gofKendallKS", "gofCustomTest", 
                           "gofKS", "gofKernel", "gofWhite", 
                           "gofPIOSRn", "gofPIOSTn"))) {
      return(2)
    } else {
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# depreciated function from earlier package versions. 


#' The function \code{gofWhich} was renamed to \code{gofTest4Copula}. Please
#' use \code{gofTest4Copula}.
#' 
#' Please see and use the function \code{\link{gofTest4Copula}}.
#' 
#' @param copula The copula to test for. Possible are the copulae
#' \code{"normal"}, \code{"t"}, \code{"clayton"}, \code{"gumbel"}, 
#' \code{"frank"}, \code{"joe"}, \code{"amh"}, \code{"galambos"}, 
#' \code{"huslerReiss"}, \code{"tawn"}, \code{"tev"}, \code{"fgm"} and 
#' \code{"plackett"}. For the default \code{NULL}, all tests in the package are
#' returned.
#' @param d The dimension to search for.
#' @return A character vector which consists of the names of the tests.
#' 
#' @export gofWhich
gofWhich <- function(copula = NULL, d = 2) {
warning(
"The function 'gofWhich' was renamed to 'gofTest4Copula'. Please use 
'gofTest4Copula'."
)
  res.f <- gofTest4Copula(copula = copula, d = d)
  return(res.f)
}
