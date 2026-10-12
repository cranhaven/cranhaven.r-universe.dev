#' Calculate odds ratios and their confidence intervals
#' from \code{glm} object
#'
#' @import MASS
#'
#' @export
#'
#' @param x \code{glm} object (logistic regression only!).
#' @param conf.level the confidence interval. Default is 0.95.
#' @return an object \code{ORci} and \code{matirix} classes with four columns.
#'   \describe{
#'    \item{OR}{odds ratio}
#'    \item{lwr}{lower conficence intarval}
#'    \item{upr}{upper conficence intarval}
#'    \item{p.value}{P value by logistic regression}
#'   }
#'
#' @examples
#' require(graphics)
#' require(MASS)
#' data(birthwt)
#' x <- glm(low ~  age + lwt + smoke + ptl + ht + ui, data = birthwt,
#'          family = binomial)
#' OR1 <- ORci(x)
#' CIplot(OR1, las = 1)

ORci <-
    function(x, conf.level = 0.95)
{
    est <- coefficients(x)
    ci <- confint(x, level = conf.level)
    p <- summary(x)$coefficients[-1, 4]

    OR <- cbind(exp(cbind(est, ci)[-1,]), p)
    colnames(OR) <- c("OR", "lwr", "upr", "p.value")

    attr(OR, "conf.level") <- conf.level
    attr(OR, "class") <- c("ORci", "matrix")
    return(OR)
}

