f.aux2 <- function(w){
  w_upper <- w[upper.tri(w)]
  
  w.min <- min(w_upper)
  w.max <- max(w_upper)
  
  x <- (w_upper - w.min)/(w.max - w.min)
  
  wt <- matrix(ncol = ncol(w), nrow = nrow(w))
  wt[upper.tri(wt)] <- x
  wt[lower.tri(wt)] <- t(wt)[lower.tri(wt)]
  diag(wt) <- 0
  colnames(wt) <- colnames(w)
  rownames(wt) <- rownames(w)
  return(wt)
}
