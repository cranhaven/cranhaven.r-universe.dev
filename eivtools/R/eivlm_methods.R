## some S3 methods for eivlm objects

model.matrix.eivlm <- function(object, ...){
    class(object) <- "lm"
    model.matrix.lm(object, ...)
}

print.eivlm <- function (x, digits = max(3L, getOption("digits") - 3L), ...){
    ## taken verbatim from stats:::print.lm
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    if (length(coef(x))) {
        cat("Coefficients:\n")
        print.default(format(coef(x), digits = digits), print.gap = 2L, quote = FALSE)
    }
    else cat("No coefficients\n")
    cat("\n")
    invisible(x)
}

print.summary.eivlm <-  function (x, digits = max(3L, getOption("digits") - 3L), symbolic.cor = x$symbolic.cor, signif.stars = getOption("show.signif.stars"), ...) {
    ## adapted from stats:::print.summary.lm
    
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
    if( !is.null(x$reliability) ){
        cat("Reliability:\n")
        print(x$reliability, digits = digits)
    } else {
        cat("Error Covariance Matrix\n")
        print(x$Sigma_error, digits = digits)
    }
    cat("\n")
    
    resid <- x$residuals
    df <- x$df
    rdf <- df[2L]
    cat(if (!is.null(x$weights) && diff(range(x$weights))) 
            "Weighted ", "Residuals:\n", sep = "")
    if (rdf > 5L) {
        nam <- c("Min", "1Q", "Median", "3Q", "Max")
        rq <- if (length(dim(resid)) == 2L) {
                  structure(apply(t(resid), 1L, quantile), dimnames = list(nam, dimnames(resid)[[2L]]))
              } else {
                  zz <- zapsmall(quantile(resid), digits + 1L)
                  structure(zz, names = nam)
              }
        print(rq, digits = digits, ...)
    } else if (rdf > 0L) {
        print(resid, digits = digits, ...)
    } else {
        cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
        cat("\n")
    }
    
    if (length(x$aliased) == 0L) {
        cat("\nNo Coefficients\n")
    } else {
        if (nsingular <- df[3L] - df[1L]) 
            cat("\nCoefficients: (", nsingular, " not defined because of singularities)\n", sep = "")
        else cat("\nCoefficients:\n")
        coefs <- x$coefficients
        if (!is.null(aliased <- x$aliased) && any(aliased)) {
            cn <- names(aliased)
            coefs <- matrix(NA, length(aliased), 4, dimnames = list(cn, colnames(coefs)))
            coefs[!aliased, ] <- x$coefficients
        }
        printCoefmat(coefs, digits = digits, signif.stars = signif.stars, 
                     na.print = "NA", ...)
    }
    cat("Number of observations used:", x$N,"\n")
    if( !is.null(x$cluster_varname) ){
        cat(paste("Std. Errors adjusted for ",x$cluster_num," clusters in ",x$cluster_varname,"\n",sep=""))
    }
    cat("Latent residual standard deviation:", format(signif(sqrt(x$latent_resvar), digits)),"\n")
    cat("Latent R-squared: ", format(signif(x$latent_R2, digits)),
        ", (df-adjusted: ",format(signif(x$latent_R2_dfadj, digits)),")\n",sep="")
    
    cat("\nEIV-Adjusted vs Unadjusted Coefficients:\n")
    tmp <- cbind(x$coefficients[,1,drop=FALSE], x$unadj_coefficients[,1])
    colnames(tmp) <- c("Adjusted","Unadjusted")
    print(tmp, digits = digits)
    
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1L) {
            cat("\nCorrelation of Coefficients:\n")
            if (is.logical(symbolic.cor) && symbolic.cor) {
                print(symnum(correl, abbr.colnames = NULL))
            } else {
                correl <- format(round(correl, 2), nsmall = 2, digits = digits)
                correl[!lower.tri(correl)] <- ""
                print(correl[-1, -p, drop = FALSE], quote = FALSE)
            }
        }
    }
    cat("\n")
    invisible(x)
}

summary.eivlm <- function (object, correlation = FALSE, symbolic.cor = FALSE, ...){
    ## adapted from stats:::summary.lm
    z   <- object
    p   <- z$rank
    rdf <- z$df.residual
    
    #####################################
    ## computations for degenerate model
    #####################################
    if (p == 0) {
        r <- z$residuals
        n <- length(r)
        w <- z$weights
        if (is.null(w)) {
            rss <- sum(r^2)
        } else {
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
        }
        ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
        class(ans) <- "summary.eivlm"
        ans$aliased <- is.na(coef(object))
        ans$residuals <- r
        ans$df <- c(0L, n, length(ans$aliased))
        ans$coefficients <- matrix(NA, 0L, 4L)
        dimnames(ans$coefficients) <- list(NULL, c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
        ans$latent_resvar <- rss/rdf
        ans$r.squared <- 0
        return(ans)
    }
    
    if (is.null(z$terms)) 
        stop("invalid 'eivlm' object:  no 'terms' component")
    if (!inherits(object, "eivlm")) 
        warning("calling summary.eivlm(<fake-eivlm-object>) ...")
    
    ##########################################################
    ## computations for the unadjusted model
    ##########################################################
    if(is.null(Qr <- z$unadj_qr)){
        stop("eivlm object does not have a proper 'qr' component for unadjusted model.\n Rank zero or should not have used eivreg(.., qr=FALSE).")
    }
    n <- NROW(Qr$qr)
    if (is.na(z$df.residual) || n - p != z$df.residual) 
        warning("residual degrees of freedom in object suggest this is not an \"eivlm\" fit")
    r <- z$unadj_residuals
    f <- z$unadj_fitted.values
    w <- z$weights
    if (is.null(w)) {
        unadj_mss <- if (attr(z$terms, "intercept"))
                         sum((f - mean(f))^2)
                     else sum(f^2)
        unadj_rss <- sum(r^2)
    } else {
        unadj_mss <- if (attr(z$terms, "intercept")) {
                         m <- sum(w * f/sum(w))
                         sum(w * (f - m)^2)
                     }
                     else sum(w * f^2)
        unadj_rss <- sum(w * r^2)
        r <- sqrt(w) * r
    }
    unadj_resvar <- unadj_rss/rdf
    if (is.finite(unadj_resvar) && unadj_resvar < (mean(f)^2 + var(f)) * 1e-30)
        warning("essentially perfect fit for unadjusted model: summary may be unreliable")
    p1 <- 1L:p
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    se <- sqrt(diag(R) * unadj_resvar)
    est <- z$unadj_coefficients[Qr$pivot[p1]]
    tval <- est/se
    ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
    ans$unadj_residuals <- r
    ans$unadj_coefficients <- cbind(est, se, tval, 2 * pt(abs(tval), rdf, lower.tail = FALSE))
    dimnames(ans$unadj_coefficients) <- list(names(z$unadj_coefficients)[Qr$pivot[p1]],
                                             c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans$aliased <- is.na(coef(object))
    ans$unadj_sigma <- sqrt(unadj_resvar)
    ans$df <- c(p, rdf, NCOL(Qr$qr))
    if (p != attr(z$terms, "intercept")) {
        df.int <- if (attr(z$terms, "intercept")) 
                      1L
                  else 0L
        ans$unadj_r.squared <- unadj_mss/(unadj_mss + unadj_rss)
        ans$unadj_adj.r.squared <- 1 - (1 - ans$unadj_r.squared) * ((n - df.int)/rdf)
        ans$unadj_fstatistic <- c(value = (unadj_mss/(p - df.int))/unadj_resvar, numdf = p - df.int, dendf = rdf)
    } else {
        ans$unadj_r.squared <- ans$unadj_adj.r.squared <- 0
    }
    ans$unadj_cov.unscaled <- R
    dimnames(ans$unadj_cov.unscaled) <- dimnames(ans$unadj_coefficients)[c(1,1)]
    if (correlation) {
        ans$unadj_correlation <- (R * unadj_resvar)/outer(se, se)
        dimnames(ans$unadj_correlation) <- dimnames(ans$unadj_cov.unscaled)
        ans$unadj_symbolic.cor <- symbolic.cor
    }
  
    ##########################################################
    ## computations for the adjusted model
    ##########################################################
    est                 <- z$coefficients
    passthrough         <- c("residuals","fitted.values","N","latent_resvar","vcov","relnames")
    ans[passthrough]    <- z[passthrough]
    if( !is.null(z$reliability) ){
        ans$reliability <- z$reliability
    } else {
        ans$Sigma_error <- z$Sigma_error
    }
    ans$latent_R2_dfadj <- (z$varYXZ[1,1] - z$latent_resvar) / z$varYXZ[1,1]
    ans$latent_R2       <- (z$varYXZ[1,1] - ((z$latent_resvar) * rdf / (z$N))) / z$varYXZ[1,1]

    if( !is.null(z$cluster_varname) ){
        passthrough      <- c("cluster_varname","cluster_values","cluster_num")
        ans[passthrough] <- z[passthrough]
    }
    
    if( !is.null(ans$vcov) ){
        se <- sqrt(diag(ans$vcov))
        tval <- est/se
        ans$coefficients <- cbind(est, se, tval, 2 * pt(abs(tval), rdf, lower.tail = FALSE))
        dimnames(ans$coefficients) <- list(rownames(ans$vcov), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

        if (correlation) {
            ans$correlation <- ans$vcov / outer(se, se)
            dimnames(ans$correlation) <- dimnames(ans$vcov)
        }
    } else {
        ans$coefficients <- cbind(est, NA, NA, NA)
        dimnames(ans$coefficients) <- list(names(est), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

        if (correlation) {
            ans$correlation <- matrix(0.0, nrow = length(est), ncol = length(est))
            is.na(ans$correlation) <- TRUE
            dimnames(ans$correlation) <- list(names(est), names(est))
        }
    }
    ans$symbolic.cor <- symbolic.cor    

    ##################################
    ## wrap up
    ##################################
    if (!is.null(z$na.action)) 
        ans$na.action <- z$na.action
    class(ans) <- "summary.eivlm"
    ans
}

vcov.eivlm <- function(object, ...){
  object$vcov
}
