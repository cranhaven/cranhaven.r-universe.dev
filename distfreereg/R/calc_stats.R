calc_stats <-
function(epsp, stat){
  stopifnot(is.numeric(epsp))
  output <- numeric(length(stat))
  for(i in seq_along(stat)){
    output[[i]] <- get(stat[i])(epsp)
  }
  return(output)
}
