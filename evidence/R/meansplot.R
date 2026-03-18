meansplot <- function(y, grp) {
    stripchart(y ~ grp, pch = 1, vert = TRUE)
    means <- tapply(y, grp, mean)
    abline(h = mean(y), lty = 3)
    for (i in 1:length(means)) {
        segments(i - 0.2, means[i], i + 0.2, means[i], 
            col = "darkgray", lwd = 5)
    }
}
