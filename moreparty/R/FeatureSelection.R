# @import party
#
# @importFrom stats predict
#
#' @export

FeatureSelection <- function(Y, X, method="NRFE", ntree = 1000, measure = NULL, nperm = 30, alpha = 0.05, distrib = 'approx', parallel = FALSE, ...) {
  if(method=='NRFE') {
    res <- NRFE(Y, X, measure=measure, ntree=ntree, parallel=parallel, ...=...)  
  } else if(method=='RFE') {
    res <- RFE(Y, X, measure=measure, ntree=ntree, parallel=parallel, ...=...)  
  } else if(method=='ALT') {
    res <- ALT(Y, X, measure=measure, ntree=ntree, nperm=nperm, alpha=alpha, distrib=distrib, parallel=parallel, ...=...)  
  } else if(method=='HAPF') {
    res <- HAPF(Y, X, measure=measure, ntree=ntree, nperm=nperm, alpha=alpha, distrib=distrib, parallel=parallel, ...=...)  
  }
  return(res)
}
