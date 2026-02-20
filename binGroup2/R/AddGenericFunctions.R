# A few generic functions to make some of our method functions work
###############################################################################

# ExpTests()
###############################################################################
#' @title Access the expected number of tests from an object
#'
#' @description \code{ExpTests} is a generic function that extracts the expected
#'   number of tests from an object that contains information
#'   about a testing configuration.
#'
#' @param object An object for which a summary of the expected number of tests is desired.
#' @param ... Additional arguments to be passed to \code{ExpTests}.

#' @return The value return depends on the class of its object. See the documentation
#'   for the corresponding method functions.
#'
#' @author Christopher R. Bilder
#'
#' @seealso
#' \code{\link{ExpTests.opChar}} and \code{\link{ExpTests.OTC}}
#'
#' @examples
#' # Find the optimal testing configuration for
#' #   non-informative two-stage hierarchical testing.
#' res1 <- OTC1(algorithm = "D2", p = 0.01, Se = 0.99, Sp = 0.99,
#'              group.sz = 2:100, obj.fn = c("ET", "MAR", "GR1"),
#'              weights = matrix(data = c(1,1), nrow = 1, ncol = 2))
#' ExpTests(res1)

ExpTests <- function(object, ...) {
  UseMethod("ExpTests")
}



# Config()
###############################################################################
#' @title Access the testing configurations returned from an object
#'
#' @description \code{Config} is a generic function that extracts testing configurations from an object

#' @param object An object from which the testing configurations are to be extracted.
#' @param ... Additional arguments to be passed to \code{Config}.

#' @author Christopher R. Bilder
#'
#' @seealso
#' \code{\link{Config.opChar}} and \code{\link{Config.OTC}}
#'
#' @examples
#' # Find the optimal testing configuration for
#' #   non-informative two-stage hierarchical testing.
#' res1 <- OTC1(algorithm = "D2", p = 0.01, Se = 0.99, Sp = 0.99,
#'              group.sz = 2:100, obj.fn = c("ET", "MAR", "GR1"),
#'              weights = matrix(data = c(1,1), nrow = 1, ncol = 2))
#' Config(res1)

Config <- function(object, ...) {
  UseMethod("Config")
}


# pmf()
###############################################################################
#' @title Access the testing probability mass function returned from an object
#'
#' @description \code{pmf} is a generic function that extracts the probability
#'   mass function from an object (if available) that contains information
#'   about a testing configuration.
#' @author Christopher R. Bilder
#' @param object An object from which the probability mass function is to be extracted.
#' @param ... Additional arguments to be passed to \code{pmf}.
#'
#' @seealso
#' \code{\link{pmf.halving}} and \code{\link{pmf.Sterrett}}
#'
#' @examples
#' res <- halving(p = rep(0.01, 10), Sp = 1, Se = 1,
#'                stages = 2, order.p = TRUE)
#' pmf.halving(res)

pmf <- function(object, ...) {
  UseMethod("pmf")
}

