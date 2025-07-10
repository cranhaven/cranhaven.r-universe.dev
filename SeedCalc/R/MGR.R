# mean germination rate

MGR <- function(time, nger) {
  result <- 1/MGT(time, nger)
  return(result)
}
