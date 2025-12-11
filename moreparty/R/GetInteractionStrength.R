#' @importFrom utils combn 

#' @export

GetInteractionStrength <- function(object, xnames=NULL) {
  input = object@data@get("input")
  if(is.null(xnames)) xnames = colnames(input)
  combi <- utils::combn(xnames,2,simplify=F)
  foo <- function(x) vip_vint(object, feature_names=x, quantiles=TRUE)
  res <- lapply(combi, foo)
  vint <- do.call('rbind.data.frame',res)
  vint <- vint[order(-vint$Interaction),]
  return(vint)
}
