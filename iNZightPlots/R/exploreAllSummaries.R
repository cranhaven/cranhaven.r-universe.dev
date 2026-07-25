#' Allows easy access to a summary for every variable in the data set.
#'
#' @title Explore all Univariate Summaries
#' @param data a data set
#' @param ... additional arguments passed to getPlotSummary()
#' @return allSummaries object, a concatenation of summaries from all variables
#' @author Tom Elliott
#' @export
#' @examples
#' exploreAllSummaries(iris)
exploreAllSummaries <- function(data, ...) {
    ## Runs getPlotSummary() on all variables.

    sums <- lapply(colnames(data),
        function(cname) {
            paste(getPlotSummary(cname, varnames = list(x = cname), data = data, ...),
                collapse = "\n"
            )
        }
    )

    tt <- paste(sums, collapse = "\n\n")

    class(tt) <- "allSummaries"
    tt
}

#' @describeIn exploreAllSummaries print method for allSummaries object
#' @param x an \code{allSummaries} object
print.allSummaries <- function(x, ...)
    cat(x)
