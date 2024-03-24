


##' How many stars would we need for this p value?
##'
##' Regression table makers need to know how many stars
##' to attach to parameter estimates. This takes
##' p values and a vector which indicates how many stars
##' are deserved.  It returns a required number of asterixes.
##' Was named "stars" in previous version, but renamed due to
##' conflict with R base function \code{stars}
##'
##' Recently, we have requests for different symbols. Some people want
##' a "+" symbol if the p value is smaller than 0.10 but greater than
##' 0.05, while some want tiny smiley faces if p is smaller than
##' 0.001. We accomodate that by allowing a user specified vector of
##' symbols, which defaults to c("*", "**", "***")
##' @param pval P value
##' @param alpha alpha vector, defaults as c(0.05, 0.01, 0.001).
##' @param symbols The default is c("*", "**", "***"), corresponding
##'     to mean that p values smaller than 0.05 receive one star,
##'     values smaller than 0.01 get two stars, and so forth.  Must be
##'     same number of elements as alpha. These need not be asterixes,
##'     could be any character strings that users desire. See example.
##' @return a character vector of symbols (eg asterixes), same length as pval
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @export
##' @examples
##' starsig(0.06)
##' starsig(0.021)
##' starsig(0.001)
##' alpha.ex <- c(0.10, 0.05, 0.01, 0.001)
##' symb.ex <- c("+", "*", "**", ":)!")
##' starsig(0.07, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.04, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.009, alpha = alpha.ex, symbols = symb.ex)
##' starsig(0.0009, alpha = alpha.ex, symbols = symb.ex)
##'
starsig <-
    function(pval, alpha = c(0.05, 0.01, 0.001),
             symbols = c("*", "**", "***"))
{
    if (length(alpha) != length(symbols)) {
        messg <- "alpha vector must have same number of elements as symbols vector"
        stop(messg)
    }
    if(is.vector(pval) && !is.numeric(pval)) pval <- as.numeric(pval)
    nstars <- sapply(pval, function(x) sum(abs(x) < alpha))
    sapply(nstars, function(x) if(!is.na(x) && x > 0) symbols[x] else " ")
}
NULL
