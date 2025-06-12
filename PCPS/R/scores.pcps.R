#' @rdname pcps
#' @encoding UTF-8
#' @export
scores.pcps<-function(x, choices = c(1, 2), ...){
  sco <- summary(x, choices = choices)$scores
  return(sco)
}