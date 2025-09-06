#' @export
print.cKendall <- function(x, ...) {
    cat("\n Test for quasi-independence with conditional Kendall's tau\n")
    cat("\n Call: ")
    print(x$Call)
    if (x$a != 0)
        cat(paste("\nTransformation is applied with parameter a =", round(x$a, 4)))
    cat(paste("\n", "Kendall's tau =", round(x$PE, 4), ", SE =", round(x$SE, 4),
              ", Z =", round(x$STAT, 4), ", p-value = ", round(x$p.value, 4), "\n\n"))
}

#' @export
coef.trReg <- function(object, ...) {
    tmp <- object$PE[,1]
    names(tmp) <- object$vNames
    return(tmp)
}

#' @export
print.pmcc <- function(x, ...) {
    cat("\n Test for quasi-independence with conditional correlation coefficient\n")
    cat("\n Call: ")
    print(x$Call)
    if (x$a != 0)
        cat(paste("\nTransformation is applied with parameter a =", round(x$a, 4)))
    cat(paste("\n", "Correlation coefficient =", round(x$PE, 4), ", SE =", round(x$SE, 4),
              ", Z =", round(x$STAT, 4), ", p-value = ", round(x$p.value, 4), "\n\n"))
}

#' @export
print.trSurvfit <- function(x, ...) {
    cat("\n Fitting structural transformation model \n")
    cat("\n Call: ")
    print(x$Call)
    cat(paste("\n", "Conditional Kendall's tau =",
              round(x$iniKendall, 4), ", p-value =", round(x$iniP, 4)))
    cat(paste("\n", "Restricted inverse probability weighted Kendall's tau =",
              round(x$iniKendall.ipw, 4), ", p-value =", round(x$iniP.ipw, 4)))
    cat(paste("\n Transformation parameter by minimizing absolute value of Kendall's tau:",
              round(x$byTau$par, 4)))
    cat(paste("\n Transformation parameter by maximizing p-value of the test:",
              round(x$byP$par, 4), "\n\n"))
}

#' @export
#' @importFrom stats model.matrix printCoefmat sd
print.trReg <- function(x, ...) {
    cat("\n Call:")
    print(x$Call)
    cat("\n Sample size =", nrow(x$.data))
    cat("\n Number of events = ", sum(x$.data$status))
    x$breaks[which.min(x$breaks)] <- -Inf
    x$breaks[which.max(x$breaks)] <- Inf
    if (length(x$a) > 1) {
        cat("\n\n The segments and the corresponding transformation parameters are:")
        for (i in 1:length(x$a)) {
            cat("\n   In segment",
                paste("(", round(x$breaks[i], 3), ", ", round(x$breaks[i + 1], 3), "]", sep = ""), 
                ", the transformation parameter is", x$a[i])
        }
        cat("\n")
    } else cat("\n\n Transformation parameter is", x$a, "\n")
    cat("\n Standard errors obtained from", x$B, "bootstrap samples.\n")
    tab <- cbind(coef = round(x$PE[,1], 3),
                 "se(coef)" = round(x$SE, 3),
                 z = round(x$PE[,1] / x$SE, 3),
                 "Pr(>|z|)" = round(2 * pnorm(-abs(x$PE[,1] / x$SE)), 3))
    rownames(tab) <- x$varNames
    printCoefmat(as.data.frame(tab), P.values = TRUE, has.Pvalue = TRUE)
    cat("\n")
    if (!is.null(x$PEta)) {
        cat("\n Coefficient estimates for transformed truncation times used in the adjusted model:\n")
        if (is.matrix(x$PEta))
            tab2 <- cbind(coef = round(x$PEta[, "coef"], 3),
                          "se(coef)" = round(x$PEta[, "se(coef)"], 3),
                          z = round(x$PEta[, "z"], 3),
                          "Pr(>|z|)" = round(x$PEta[, "Pr(>|z|)"], 3))
        ## tab2 <- cbind(coef = round(x$PEta[,1], 3),
        ##               "se(coef)" = round(x$PEta[,3], 3),
        ##               z = round(x$PEta[,4], 3),
        ##               "Pr(>|z|)" = round(x$PEta[,5], 3))
        else tab2 <- cbind(coef = round(x$PEta[1], 3),
                           "se(coef)" = round(x$PEta[3], 3),
                           z = round(x$PEta[4], 3),
                           "Pr(>|z|)" = round(x$PEta[5], 3))
        rownames(tab2) <- rownames(x$PEta)
        printCoefmat(as.data.frame(tab2), P.values = TRUE, has.Pvalue = TRUE)
        cat("\n")
    }
    
}

#' @export
summary.trReg <- function(object, ...) {
    print(object)
}

#' @export
print.trgof <- function(x, ...) {
    cat("\n Overall signficances based on left-truncated regression model: p-value =", round(x$pval, 4))
    if (x$input != "Surv") {
        cat("\n\n The segments and the corresponding transformation parameters are:")
        x$breaks[which.min(x$breaks)] <- -Inf
        x$breaks[which.max(x$breaks)] <- Inf
        for (i in 1:length(x$fitQs)) {
            cat("\n   For segment",
                paste("(", round(x$breaks[i], 3), ", ", round(x$breaks[i + 1], 3), "]", sep = ""), 
                ", the transformation parameter is", unique(x$fitQs[[i]]$a))
        }
    }
    cat("\n\n")
}
