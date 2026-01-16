

PCAsimilarity <- function(cov.x, cov.y, ret.dim = NULL, ...) {
  if (is.null(ret.dim))
    ret.dim = dim(cov.x)[1]
  
  eg.x <- eigen(cov.x)
  eg.y <- eigen(cov.y)
  eg.x.values <- eg.x$values[1:ret.dim]
  eg.y.values <- eg.y$values[1:ret.dim]
  eg.x.vectors <- eg.x$vectors[,1:ret.dim]
  eg.y.vectors <- eg.y$vectors[,1:ret.dim]
  
  total_var <- eg.x.values %*% eg.y.values
  
  return (c(PCAsimilarity = sum((eg.x.values %o% eg.y.values) * ((t(eg.x.vectors) %*% (eg.y.vectors))**2))/total_var))
}