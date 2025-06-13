print.AcrossTic <- function (x, ...) 
{
cat ("AcrossTic object\n")
if (x$X.supplied) {
    cat (paste0 ("Data is ", x$nrow.X, " x ", x$ncol.X, ", r = ", x$r, "\n"))
} else {
    cat (paste0 ("Dist of size ", x$nrow.X, ", r = ", x$r, "\n"))

}
if (any (names (x) == "cross.count"))
    cat (paste0 ("Solution ", signif (x$total.dist, 4), 
                 ", cross-count ", signif (x$cross.count, 4), "\n"))
else
    cat (paste0 ("Solution ", signif (x$total.dist, 4), "\n"))
# "computed in elapsed time", x$time.required["elapsed"], "\n")

    

}
