plot.regAbcrf <- function(x, n.var=min(30, length(x$model.rf$variable.importance)), xlim=NULL, main=NULL, ...){
  if (!inherits(x, "regAbcrf")) 
    stop("First argument not of class regAbcrf")
  if (!is.numeric(n.var))
    stop("n.var needs to be a numeric object")
  if (!is.null(xlim) && !is.numeric(xlim))
    stop("xlim needs to be a numeric object or NULL")
  variableImpPlot(x, n.var=n.var, xlim=xlim, main=main)
}