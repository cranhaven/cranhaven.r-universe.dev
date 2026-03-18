overdispersionCheck <- function(x) {
    n <- length(x)
    iqr <- IQR(x)
    mdn <- median(x)
    lwr <- mdn - 1.5 * iqr
    upr <- mdn + 1.5 * iqr
    lengthSTlwr <- length(x[x < lwr])
    lengthGTupr <- length(x[x > upr])
    p <- (lengthSTlwr + lengthGTupr) * 100/n
    cat(lengthSTlwr, "of", n, "obervations smaller than median - 1.5 * IQR\n")
    cat(lengthGTupr, "of", n, "obervations greater than median + 1.5 * IQR\n")
    cat(round(p, 2), "percent of the observations are further from the median than\n")
    cat("in a normal distribution\n")
    if (p > 11) 
        cat("The input vector therefore shows overdispersion.\n\n")
    if (p < 11) 
        cat("The input vector therefore shows no overdispersion.\n\n")
}
