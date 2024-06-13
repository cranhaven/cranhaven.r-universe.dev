plot.OBsProb <-
function (x, code = TRUE, prt = FALSE, cex.axis = par("cex.axis"), 
    ...) 
{
    spikes <- function(prob, lwd = 3, ...) {
        y <- prob
        n <- nrow(y)
        x <- seq(n)
        lab <- rownames(prob)
        barplot(y[, 1], ylim = c(0, 1), xlab = "factors", ylab = "posterior probability", axes = TRUE, ...)
        axis(2, cex.axis = cex.axis)
        invisible(NULL)
    }
    if (!any(class(x) == "OBsProb")) 
        return("\nArgument `x' should be class OBsProb. Output of corresponding function.")
    prob <- as.matrix(x$prob)
    if (code) 
        rownames(prob) <- names(x$prob)
    spikes(prob, ...)
    if (prt) 
        summary.OBsProb(x)
    invisible(NULL)
}
