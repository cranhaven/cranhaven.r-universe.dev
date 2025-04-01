#' Plot for a ps.xbg object
#'
#' `ps.xgb.plot` produces a figure showing the balance criteria by iteration for a 
#' ps.xgb object.
#' 
#' @param x An object of class [ps.xgb]
#' @param ... Arguments to be passed to other functions
#' 
#' @return Returns a [ggplot] object.
#' 
#' @examples 
#' # See vignette for examples.
#' 
#' @import ggplot2
#' @method plot ps.xgb
#' @export


plot.ps.xgb = function(x , ...){
   #  extract data
   d = x$es.max
   d = apply(d,1,mean)
   d = data.frame(d=d , x=(0:(length(d)-1))*x$iters.per.step)
   
   # create plot
   out = ggplot(d) + 
      geom_line(aes(x=x,y=d)) + 
      theme_bw() + 
      xlab('Iteration') + scale_x_continuous(expand=c(0.02,0) , limits=c(0,max(d$x))) +
      ylab('Maximum Standardized Difference' ) + scale_y_continuous(expand=c(0.02,0) , limits = c(0,max(d$d)))
   
   return(out)
}
