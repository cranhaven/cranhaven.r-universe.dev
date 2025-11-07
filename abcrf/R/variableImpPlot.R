variableImpPlot <- function(object, n.var=min(30, length(object$model.rf$variable.importance)), xlim=NULL, main=NULL)
{
  if (!inherits(object, "regAbcrf") && !inherits(object, "abcrf") )
    stop("object not of class abcrf or regAbcrf")
  imp <- object$model.rf$variable.importance
  ord <- rev(order(imp, decreasing = TRUE)[1:n.var])
  if(is.null(xlim)){
    xmin <- 0
    dotchart(imp[ord], xlab = 'Variable Importance', ylab = "", xlim = c(xmin, max(imp)), main=main)
  }
  else {
    xmin <- 0
    dotchart(imp[ord], xlab = 'Variable Importance', ylab = "", xlim = xlim, main=main)
  }
  invisible(imp)
}