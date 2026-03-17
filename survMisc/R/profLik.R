#' @name profLik
#' @title Profile likelihood for coefficients in a \code{coxph} model
#' @description Profile likelihood for coefficients in a \code{coxph} model
#' 
#' @param x A \code{coxph} model.
#' @param CI \bold{C}onfidence \bold{I}nterval.
#' @param interval Number of points over which to evaluate coefficient.
#' @param mult \bold{Mult}iplier. Coefficent will be multiplied by lower and upper
#' value and evaluated across this range.
#' @param devNew Open a new device for each plot. See 
#'  \cr
#' ?grDevices::dev.new
#' @param ... Additional parameters passed to \code{graphics::plot.default}.
#' 
#' @details
#' Plots of range of values for coefficient in model with log-likelihoods
#' for the model with the coefficient fixed at these values.
#'  \cr \cr
#' For each coefficient a range of possible values is chosen, given by
#'  \eqn{\hat{B}*mult_{lower} - \hat{B}*mult_{upper}}{
#'       Bhat * mult[lower] - Bhat * mult[upper]}.
#' A series of models are fit (given by \code{interval}).
#' The coefficient is included in the model as a
#' \emph{fixed} term and the partial log-likelihood for the model is calculated.
#'  \cr \cr
#' A curve is plotted which gives the partial log-likelihood for each of these candidate values.
#' An appropriate confidence interval (CI) is given
#' by subtracting 1/2 the value of the appropriate quantile
#' of a chi-squared distribution with \eqn{1} degree of freedom.
#' \cr \cr
#' Two circles are also plotted giving the 95% CI for the Wald statistic.
#' 
#' @return One plot for each coefficient in the model. 
#' 
#' @references Example is from:
#' \bold{T&G}. 
#' Section 3.4.1, pg 57.
#'
#' @export
#' @examples
#' data("pbc", package="survival")
#' c1 <- coxph(formula = Surv(time, status == 2) ~ age + edema + log(bili) +
#'                       log(albumin) + log(protime), data = pbc)
#' profLik(c1, col="red")
#'
profLik <- function(x,
                    CI=0.95,
                    interval=50,
                    mult=c(0.1, 2),
                    devNew=TRUE,
                    ...) {
    if (!inherits(x, "coxph")) stop ("Only applies to objects of class coxph")
    coef1 <- stats::coef(x)
    ## use collapse in case formula spans >1 line
    f1 <- paste0(deparse(x$formula), collapse="")
    f1 <- gsub("  ", "", f1)
    ## plot title
    main1 <- paste0("Partial likelihood profiles and ",
                    100 * CI,
                    "% CI cutoff for model:\n",
                    f1,
                    " \n Circles show ",
                    100 * CI,
                    "% CI limits for Wald interval")
###----------------------------------------
###  plots
###----------------------------------------
    ## get names of the coefficients from model.frame
    ## note excluding Surv
    n1 <- names(stats::model.frame(x))[!grepl( "Surv", names(stats::model.frame(x)) )]
    ## allocate memory for log partical likelihood
    llik <- double(length=interval)
    for (i in seq(length(coef1))) {
        ## lower + upper limits
        low1 <- mult[1] * coef1[i]
        up1 <- mult[2] * coef1[i]
        ## range for coefficient
        beta1 <- seq(from=low1, to=up1, length.out=interval)
        for (j in seq(interval)) {
            ## right hand side of formula without coefficient
            rhs1 <- paste0(n1[-i], collapse="+")
            ## offset = includes coefficient as fixed covariate
            off1 <- beta1[j]
            off2 <- paste0("+offset(", off1, "*", n1[i], ")")
            ## new RHS for formula
            rhs2 <- paste0(rhs1, off2)
            f2 <- stats::as.formula(paste0(".~", rhs2))
            ## refit model and find model loglik with this value (beta) of coefficient
            c2 <- stats::update(x, formula=f2)
            llik[j] <- c2$loglik[2]
        }
        graphics::par(oma=c(0, 0, 4, 0))
        if (i > 1 & devNew == TRUE) grDevices::dev.new()
        graphics::plot.default(beta1, llik,
                               type="l",
                               xlab="Values for coefficient",
                               ylab="Model partial likelihood",
                               main=n1[i],
                               ...)
        ## range for confidence interval is chi-square on with 1 df
        rCI <- stats::qchisq(CI, 1)
        ## confidence interval (calcuate lower only)
        ci1 <- x$loglik[2] - rCI / 2
        graphics::abline(h=ci1, lty=2)
        sd1 <- sqrt(x$var[i, i])
        ## range for confidence interval of Wald is normal
        ## if CI is 95% then need convert to 97.5%
        CI2 <- (1 - CI) / 2
        rCI <- stats::qnorm(1 - CI2)
        graphics::points(coef1[i] + c(-rCI, rCI) * sd1,
                         c(ci1, ci1),
                         pch=1,
                         cex=3,
                         ...)
        graphics::mtext(main1, line=0.3, outer=TRUE)
        }
}
