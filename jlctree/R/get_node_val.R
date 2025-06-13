#' Computes the test statistic at the current node.
#'
#' Computes the test statistic at the current node.
#'  Not to be called directly by the user. 
#'
#' @param f1 a two-sided formula of the fitted survival model, without the longitudinal outcome in 
#'      the right side of the formula. Only needed when \code{lrt}=TRUE.
#' @param f2 a two-sided formula of the fitted survival model, same as \code{f1} but with the longitudinal outcome 
#'      being the first covariate on the right side of the formula. 
#' @param data a data.frame containing covariates in \code{f2}.
#' @param lrt if TRUE, use likelihood ratio test, otherwise use 
#'      Wald test. Default is TRUE.
#' @param ... further arguments to pass to or from other methods.
#'
#' @return The test statistic at the current node.
#'
#' @examples 
#'  data(data_timevar);
#'  f1 <- Surv(time_L, time_Y, delta)~X3+X4+X5;
#'  f2 <- Surv(time_L, time_Y, delta)~y+X3+X4+X5;
#'  get_node_val(f1, f2, data_timevar, lrt=TRUE);
#'
#' @seealso \code{\link{get_lrt},\link{get_wald}}
#' @export


get_node_val<- function(f1, f2, data, lrt=TRUE, ... ){

    if (lrt){
        ret <- get_lrt(f1, f2, data, ...)
    } else {
        ret <- get_wald(f2, data)
    }
    return (ret)
}



