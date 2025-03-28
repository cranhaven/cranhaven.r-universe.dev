#'CytOpt plot
#'
#'plot S3 method for CytOpt object
#'
#'@param x an object of class \code{CytOpt} to plot.
#'@param ... further arguments passed to or from other methods. Not implemented.
#'
#'@return a \code{\link[ggplot2]{ggplot}} object, potentially composed through 
#'\code{\link[patchwork]{patchwork}}
#'
#'@method plot CytOpt
#'@import patchwork
#'@export
#'
#'@examples
#'if(interactive()){
#'
#'res <- CytOpT(X_s = HIPC_Stanford_1228_1A, X_t = HIPC_Stanford_1369_1A, 
#'              Lab_source = HIPC_Stanford_1228_1A_labels,
#'              eps = 0.0001, lbd = 0.0001, n_iter = 10000, n_stoc=10,
#'              step_grad = 10, step = 5, power = 0.99, 
#'              method='minmax')
#'plot(res)
#'
#'}

plot.CytOpt <- function(x, ...){
  
  if(colnames(x$proportions)[1] == "Gold_standard" & !is.null(x$monitoring)){
    KL_plot(x$monitoring) /
      barplot_prop(x$proportions)
  }else{
    barplot_prop(x$proportions)
  }
}