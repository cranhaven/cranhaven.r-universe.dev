
#' Interval Matrix associated to a Histogram Matrix
#' @author Jorge Arce Garro
#' @keywords internal
#' @param x A Histogram matrix
#'
#' @return An Interval Matrix
#'
#' @examples
get.sym.interval.limits<-function(x){
  dim.hist<-dim(x)
  out <- tibble::tibble(.rows = dim.hist[1])
  for (i in seq_len(dim.hist[2])) {
    values <- as.data.frame(matrix(rep(0,2*dim.hist[1]),nrow = dim.hist[1]))
    for(j in 1:dim.hist[1]){
      values[j, 1]<-min(x[[i]][[j]]$breaks)
      values[j, 2]<-max(x[[i]][[j]]$breaks)
    }
    new_interval <- new.sym.intreval(values[, 1], values[, 2])
    name <- colnames(x)[i]
    out <- tibble::add_column(out, `:=`({
      {
        name
      }
    }, new_interval))
  }
  row.names(out)<-row.names(x)
  class(out)<-c("symbolic_tbl",class(out))
  return(out)
}
