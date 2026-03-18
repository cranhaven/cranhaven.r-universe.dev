binPriorsPlot <- function() {
    x <- seq(0, 1, 0.01)
    k <- c(0.5, 1, 2)
    par(mfrow = c(3, 3))
    for (i in 1:3) {
        for (j in 1:3) {
            plot(x, dbeta(x, k[i], k[j]), type = "l", 
                ylab = "g(x)", axes = F, xlab = "p")
            axis(2, labels = F)
            axis(1, at = c(0, 0.5, 1))
            title(paste("Beta(", k[i], ", ", k[j], 
                ")", sep = ""), cex = 0.7)
        }
    }
    par(mfrow = c(1, 1))
}
