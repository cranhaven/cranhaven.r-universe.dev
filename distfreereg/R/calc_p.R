calc_p <-
function(observed_stats, mcsim_stats){
  stopifnot(is.list(observed_stats), is.list(mcsim_stats),
            identical(length(observed_stats), length(mcsim_stats)),
            isTRUE(all(names(observed_stats) == names(mcsim_stats))))
  m <- length(observed_stats)
  stopifnot(m == length(mcsim_stats))
  output <- vector(mode = "list", length = m)
  names(output) <- names(observed_stats)
  for(i in seq_len(m)){
    output[i] <- mean(observed_stats[[i]] <= mcsim_stats[[i]])
  }
  return(output)
}
