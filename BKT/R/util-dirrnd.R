dirrnd <- function(alphavec) {
  rr <- rgamma(n = length(alphavec), shape = alphavec, scale = 1)
  a <- array(rr, dim(alphavec))
  if (length(dim(alphavec)) == 3) {
    ta <- aperm(a, c(1, 3, 2))
    sums <- rowSums(ta, dims = 2)
    expanded_sums <- array(rep(sums), dim = dim(ta))
    aa <- ta / expanded_sums
    aa <- aperm(aa, c(1, 3, 2))
  } else if (length(dim(alphavec)) == 2) {
    ta <- aperm(a, c(2, 1))
    sums <- rowSums(ta, dims = 1)
    expanded_sums <- array(rep(sums), dim = dim(ta))
    aa <- ta / expanded_sums
    aa <- aperm(aa, c(2, 1))
  } else {
    stop("dirrnd not working")
  }
  return(aa)
}
