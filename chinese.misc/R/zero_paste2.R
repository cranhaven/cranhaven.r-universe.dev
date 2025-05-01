zero_paste2 <-
function(x, len) {
  x <- as.character(x)
  xn <- nchar(x)
  zeros <- paste0(rep("0", (len - xn)), collapse = "")
  x <- paste(zeros, x, sep = "")
  return(x)
}
