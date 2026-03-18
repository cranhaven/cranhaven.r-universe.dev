nineplot <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    mn <- mean(x)
    s <- sd(x)
    opar <- par(mfrow = c(3, 3))
    for (i in 1:4) {
        y <- rnorm(n, mn, s)
        qqnorm(y, xlab = "", ylab = "", main = "repl.")
        qqline(y)
    }
    qqnorm(x, col = "red", main = "real")
    qqline(x, col = "red")
    for (i in 6:9) {
        y <- rnorm(n, mn, s)
        qqnorm(y, xlab = "", ylab = "", main = "repl.")
        qqline(y)
    }
    par(opar)
}
