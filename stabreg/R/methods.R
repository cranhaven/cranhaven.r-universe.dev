

predict.stabreg <- function(object, ...){
    stopifnot(class(object) == "stabreg")

    X <- model.matrix(object$formula, model.frame(object$formula, ...))

    if(object$is_glm)
        coefidc <- grep("^loc", names(object$coefs))
    else
        coefidc <- 1:ncol(X)
    as.vector(X %*% object$coefs[coefidc])
}


print.stabreg <- function(x, ...){

    par <- x$coefs
    parnames <- names(x$coefs)

    se_vals  <- if(!is.null(x$se)) x$se else rep(NA_real_, length(par))

    lc <- rc <- approx_lc <- approx_rc <- rep(NA_real_, length(par))
    if(!is.null(x$cb)){
        if(x$is_glm){
            lc <- x$cb$left
            rc <- x$cb$right
        } else {
            lc <- c(x$cb$left, rep(NA_real_, 3))
            rc <- c(x$cb$right, rep(NA_real_, 3))
        }
    }

    if(!is.null(x$approx_cb)){
        approx_lc <- x$approx_cb$left
        approx_rc <- x$approx_cb$right
    }


    df <- data.frame("Estimate" = par,
                     "Std.Error" = se_vals,
                     "t.value" = unname(par) / se_vals,
                     "aprx.left.conf" = approx_lc,
                     "aprx.right.conf" = approx_rc,
                     "left.conf" = lc,
                     "right.conf" = rc,
                     row.names = parnames)

    print(x$formula)
    print(df)

    if(!is.null(x$cb))
        cat("conf level: ", x$conf_level, "\n")
    cat("\nAIC = ", x$aic, "\n")
}


AIC.stabreg <- function(object, ..., k = 2){
    k*length(object$coefs) - 2*object$log_lik

}
