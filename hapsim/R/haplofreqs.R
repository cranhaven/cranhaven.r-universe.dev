"haplofreqs" <-
function(dat, firstl, lastl){
  n <- nrow(dat)
  hapls <- apply(dat[1:n,firstl:lastl], 1, function(x) paste(x, collapse="")) 
  freqs <- table(hapls)/n  
  return(freqs)
}
