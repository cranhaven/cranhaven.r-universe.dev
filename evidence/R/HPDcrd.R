HPDcrd <- function(x, alpha = 0.05) {
    n <- length(x)
    nn <- round(alpha * n)
    x <- sort(x)
    xx <- x[(n - nn + 1):n] - x[1:nn]
    m <- min(xx)
    nnn <- which(xx == m)[1]
    return(c(x[nnn], x[n - nn + nnn]))
}
