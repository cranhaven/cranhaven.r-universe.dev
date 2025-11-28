#' @export

summary.somsp <- function(object, ...){
  out <- unique(object$summary[, .(node, node_counts, node_lat, node_lon, node_sd_lat, node_sd_lon)])
  setorder(out, node)
  return(out)
}
