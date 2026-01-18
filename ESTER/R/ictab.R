#' Computes Akaike weights or pseudo-BMA weights for a set of models
#'
#' Returns a table with weights of a set of models, based on various
#' information criteria. Currently, \code{ictab} supports the computation of
#' Akaike weights from the \code{aic} or the \code{bic} computed on \code{lm}
#' or \code{merMod} models, as well as the computation of pseudo-BMA weights,
#' computed from the WAIC or LOOIC of \code{brmsfit} models.
#'
#' @param mods Should be a named list of models, of class \code{lm}, \code{merMod} or
#' \code{brmsfit}.
#' @param ic Indicates which information criterion to use. Current supported
#' information criteria include \code{aic} and \code{bic} for \code{lm} and
#' \code{merMod} models, as well as \code{WAIC} and \code{LOO} for
#' \code{brmsfit} models.
#' @param ... Additional parameters to be passed to \code{brms::WAIC} or
#' \code{brms::LOO} functions.
#'
#' @return An object of class \code{data.frame}, which contains the value of the
#' information criterion (either AIC, BIC, WAIC or LOOIC), the number of parameters
#' (k for AIC and BIC or p for WAIC or LOOIC), the delta_IC (for AIC and BIC) or the
#' elpd for models compared with WAIC or LOOIC, and the weight of each
#' model (Akaike weights for AIC or BIC and pseudo-BMA weights for WAIC or LOOIC).
#'
#' @importFrom stats as.formula logLik
#' @importFrom rlang dots_list
#' @importFrom brms WAIC LOO
#'
#' @examples
#' library(ESTER)
#' data(mtcars)
#' mod1 <- lm(mpg ~ cyl, mtcars)
#' mod2 <- lm(mpg ~ cyl + vs, mtcars)
#' mod3 <- lm(mpg ~ cyl + vs + I(vs^2), mtcars)
#' mod4 <- lm(mpg ~ cyl * vs, mtcars)
#' mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4)
#' ictab(mods, aic)
#' ictab(mods, bic)
#'
#' \dontrun{
#' library(brms)
#' mod1 <- brm(mpg ~ cyl, mtcars)
#' mod2 <- brm(mpg ~ cyl + vs, mtcars)
#' mods <- list(m1 = mod1, m2 = mod2)
#' ictab(mods, LOO, reloo = TRUE, k_threshold = 0.6, cores = 2)
#' }
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @seealso \code{\link{aic}}, \code{\link{bic}}
#'
#' @references Burnham, K. P., \& Anderson, D. R. (2002). Model Selection
#' and Multimodel Inference: A Practical Information-Theoretical Approach.
#' 2d ed. New York: Springer-Verlag.
#'
#' @references Burnham, K. P., \& Anderson, D. R. (2004). Multimodel
#' inference: Understanding AIC and BIC in model selection. Sociological
#' Methods and Research, 33(2), 261-304.
#'
#' @references Yao, Y. P., Vehtari, A., Simpson, D., \& Gelman, A. (2017).
#' Using stacking to average Bayesian predictive distributions.
#'
#' @export

ictab <- function(mods, ic, ... ) {

    modnames <- names(lapply(mods, function(x) deparse(substitute(x) ) ) )

    check.resp <-
        lapply(mods, FUN = function(x) as.formula(formula(x) )[[2]] )

    if (length(unique(check.resp) ) > 1)

        stop("\n All models should be fitted on the same data \n")

    res <- data.frame(modnames = modnames)

    if (identical(ic, aic) | identical(ic, bic) ) {

        res$ic <- unlist(lapply(mods, function(x) ic(x)[1]) )
        res$k <- unlist(lapply(mods, function(x) attr(logLik(x), "df") ) )
        res$delta_ic <- res$ic - min(res$ic)
        res$mod_lik <- exp(-0.5 * res$delta_ic)
        res$ic_wt <- res$mod_lik / sum(res$mod_lik)

    } else if (identical(ic, WAIC) | identical(ic, LOO) ) {

        res_list <- lapply(mods, function(x) ic(x, ...) )

        res$ic <- unlist(lapply(res_list, function(x) x[[3]]) )
        res$p_ic <- unlist(lapply(res_list, function(x) x[[2]]) )
        res$elpd <- unlist(lapply(res_list, function(x) x[[1]]) )
        res$un_wt <- exp(res$elpd - max(res$elpd) )
        res$ic_wt <- res$un_wt / sum(res$un_wt)

    }

    res <- res[rev(order(res$ic_wt) ), ]
    res[, 2:6] <- sapply(res[, 2:6], round, 4)

    return(res[, -5])

}
