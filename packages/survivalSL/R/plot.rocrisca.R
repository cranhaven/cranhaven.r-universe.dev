
plot.rocrisca <- function(x, ..., information=TRUE){

plot.default(NULL, xlim = c(0,1), ylim = c(0,1), ...)

lines(1-x$table$sp, x$table$se, ...)

if(information==TRUE){abline(c(0,1), lty=2)}
}

