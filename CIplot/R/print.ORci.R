#' Print Methods for Odds Ratios and their Confidence Intervals
#' of \code{ORci} object
#'
#' Print odds ratios and their confidence intervals of \code{ORci} object.
#' 
#' @method print ORci
#' @export
#' 
#' @param x \code{ORci} object.see alse \code{\link{ORci}}.
#' @param \dots other options for print such as \code{digits}.
#'
#' @seealso \code{glm}, \code{\link{ORci}}.
#' 
#' @examples
#' require(MASS)
#' data(birthwt)
#' x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
#'          family = binomial)
#' OR1 <- ORci(x)
#' print(OR1, digits = 3)
#'
print.ORci <- function(x, ...)
{
    attr(x, "conf.level") <- NULL
    attr(x, "class") <- NULL
    print(x, ...)
}

