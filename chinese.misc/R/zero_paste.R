zero_paste <-
function(x) {
  maxx <- max(x)
  max_char <- nchar(as.character(maxx))
  newx <- as.character(x)
  for (i in 1:length(newx)) {
    ii <- newx[i]
    char_i <- nchar(ii)
    zeros <- paste0(rep("0", (max_char - char_i)), collapse = "")
    ii <- paste(zeros, ii, sep = "")
    newx[i] <- ii
  }
  return(newx)
}
