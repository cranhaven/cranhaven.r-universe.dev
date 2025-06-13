#' Computes the Wald test statistic.
#'
#' Computes the Wald test statistic.
#'  Not to be called directly by the user. 
#'
#' @param f a two-sided formula of the fitted survival model, with the longitudinal outcome 
#'      being the first covariate on the right side of the formula.
#' @param data a data.frame containing covariates in \code{f}.
#'
#' @return The Wald test statistic.
#'
#' @examples 
#'  data(data_timevar);
#'  f <- Surv(time_L, time_Y, delta)~y+X3+X4+X5;
#'  get_wald(f, data_timevar);
#'
#' @importFrom survival coxph Surv
#'
#' @seealso \code{\link{get_node_val}}
#' @export


get_wald <- function(f, data){
    ret <- tryCatch({
        coxml <- suppressWarnings(coxph(f,data))
        (coxml$coefficients[1])^2/ coxml$var[1,1]
    }, error = function(e){Inf})
}


