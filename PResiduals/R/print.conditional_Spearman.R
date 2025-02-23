#' conditional_Spearman class print method
#' @method print conditional_Spearman
#' @param x conditional_Spearman object
#' @param ... arguments passed to print.default
#' @keywords print 
#' @export
#' @importFrom utils head
print.conditional_Spearman <- function(x, ...) {
  cat("Partial Spearman's correlation conditional by:", x$conditional.by, '\n')
  cat("Conditional method:", x$conditional.method, '\n')
  if(x$conditional.method=="lm") {
    cat("Conditional Formula:", as.character(x$est$conditional.formula), '\n')
    y <- head(x$est$est)
  
  }else if(x$conditional.method=="kernel"){
    cat("kernel function:", x$est$kernel.function, "\n")
    cat("kernel bandwidth:", round(x$est$kernel.bandwidth, 4), '\n')
    y <- head(x$est$est)
  }else if(x$conditional.method=="stratification"){
    cat("Number of levels of", x$conditional.by, ":",dim(x$est$est)[1], '\n' )
    y <- x$est$est
  }
  
  invisible(print(y,...))
  if(dim(x$est$est)[1]>6) cat('...', '\n')
  cat('Fisher Transform:',x$fisher,'\n')
  cat('Confidence Interval: ', format(x$conf.int*100,digits=3),'%\n', sep='')
  cat('Number of Observations:',x$data.points,'\n')  
  invisible(y)
}

