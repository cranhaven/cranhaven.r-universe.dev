growth <- function(lengths, wr = 90, wh = 10) {

  result <- (mean(lengths[[4]])*wr + mean(lengths[[3]])*wh)
  return(result)
}


