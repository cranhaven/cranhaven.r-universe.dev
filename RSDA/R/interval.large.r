
#' Calculate the large of each interval
#'
#' @param x An interval matrix
#'
#' @return A matrix with the large of each interval.
#' @export
#'
#' @examples
#' \dontrun{
#' data(oils)
#' interval.large(oils)
#' }
interval.large<-function(x){
  if (!all(sapply(x, function(x) any(class(x) %in% "symbolic_interval")))) {
    stop("All variables have to be intervals")
  }
  out <- purrr::map_df(x, function(x) ((max(x) - min(x))))
  out<-as.data.frame(out)
  row.names(out)<-row.names(x)
  return(out)
}
