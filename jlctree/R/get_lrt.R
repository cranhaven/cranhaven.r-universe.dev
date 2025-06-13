#' Computes the likelihood ratio test statistic.
#'
#' Computes the likelihood ratio test statistic. 
#'  Not to be called directly by the user. 
#'  
#' @param f1 a two-sided formula of the fitted survival model, without the longitudinal outcome in 
#'      the right side of the formula.
#' @param f2 a two-sided formula of the fitted survival model, same as \code{f1} but with the longitudinal outcome 
#'      being the first covariate on the right side of the formula. 
#' @param data a data.frame containing the covariates in both \code{f1} and \code{f2}.
#' @param stable a parameter, see also \code{jlctree.control}.
#' @param cov.max a parameter, see also \code{jlctree.control}.
#'
#' @return The likelihood ratio test statistic. 
#'
#' @examples 
#'  data(data_timevar);
#'  f1 <- Surv(time_L, time_Y, delta)~X3+X4+X5;
#'  f2 <- Surv(time_L, time_Y, delta)~y+X3+X4+X5;
#'  get_lrt(f1, f2, data_timevar);
#'
#' @importFrom survival coxph Surv
#'
#' @seealso \code{\link{get_node_val}}
#' @export


get_lrt <- function(f1, f2, data, stable=TRUE, cov.max=1e5){

    coxml1 <- tryCatch({
        coxml1 <- suppressWarnings(coxph(f1, data))
        coxml1
    }, error = function(e){
        f1 <- Surv(start, end, event) ~ 1
        coxml1 <- suppressWarnings(coxph(f1, data))
        coxml1$loglik <- c(coxml1$loglik[1],coxml1$loglik[1])
        coxml1
    })

    coxml2 <- tryCatch({
        coxml2 <- suppressWarnings(coxph(f2, data))
        coxml2
    }, error = function(e){
        f2 <- Surv(start, end, event) ~ 1
        coxml2 <- suppressWarnings(coxph(f2, data))
        coxml2$loglik <- c(coxml2$loglik[1],coxml2$loglik[1])
        coxml2
    })

    loglik_diff <- 2*(coxml2$loglik[2] - coxml1$loglik[2] )

    if(stable){
        # If the estimate is unstable, set the diff to be Inf.
        # Thus, the split leading to this data will not be considered.
        if (max(c(diag(coxml1$var), diag(coxml2$var))) > cov.max){
            loglik_diff <- Inf
        }
    }

    # If some fit doesnt went wrong, set 
    if(is.na(loglik_diff)){loglik_diff <- Inf}
    ret <- max(0,loglik_diff)
    return(ret)
}


