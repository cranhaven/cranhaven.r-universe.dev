#' conditional_Spearman class plot method
#' @method plot conditional_Spearman
#' @param x conditional_Spearman object
#' @param ... arguments passed to plot.default
#' @keywords plot
#' @export
#' @importFrom graphics axis box lines plot points polygon segments

plot.conditional_Spearman <- function(x, ...){
  if(x$conditional.method=="kernel"){
    plot(x$est$est[, x$conditional.by], x$est$est[,"est"], ylim=c(-1, 1), xlab=x$conditional.by, ylab="conditional Spearman", ...)
  }else if(x$conditional.method=="stratification"){
    
    plot(1:dim(x$est$est)[1], x$est$est[,"est"], ylim=c(-1, 1), xlab=x$conditional.by, ylab="conditional Spearman", type="n" , axes=F)
    axis(side=1, at=1:dim(x$est$est)[1], labels=x$est$est[, x$conditional.by])
    axis(side=2, at=(-2:2)/2, labels=format((-2:2)/2, nsmall=2))
    box()
    points(1:dim(x$est$est)[1], x$est$est[,"est"], ylim=c(-1, 1), xlab=x$conditional.by, ylab="conditional Spearman", ...)
    points(1:dim(x$est$est)[1], x$est$est[,"lower.CI"], col=2, pch="-")
    points(1:dim(x$est$est)[1], x$est$est[,"upper.CI"], col=2, pch="-")
    segments(1:dim(x$est$est)[1], x$est$est[,"upper.CI"],1:dim(x$est$est)[1], x$est$est[,"lower.CI"],col=2)    
  }else if(x$conditional.method=="lm"){
    
    ord.est <- x$est$est[order(x$est$est[, x$conditional.by]), c(x$conditional.by, "est", "lower.CI", "upper.CI")]
    plot(ord.est[, x$conditional.by], ord.est[, "est"], ylim=c(-1, 1),xlab=x$conditional.by, ylab="conditional Spearman", type="n", ... )
    lines(ord.est[, x$conditional.by], ord.est[, "lower.CI"],col=2, lty=2)
    lines(ord.est[, x$conditional.by], ord.est[, "upper.CI"],col=2, lty=2)
    polygon(c(rev(ord.est[, x$conditional.by]), ord.est[, x$conditional.by]), 
            c(rev(ord.est[ ,"upper.CI"]), ord.est[ ,"lower.CI"]), col = 'grey80', border = NA)
    lines(ord.est[, x$conditional.by], ord.est[, "est"],col=1, lty=1)
    
  }else stop("Please specify conditional.method as lm, kernel, or stratification")
  
  
  
}
