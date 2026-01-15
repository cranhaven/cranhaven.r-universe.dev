# internal division for Buchberger algorithm
BBdivision <- function(qspray, divisors, LTdivisors) {
  if(qspray == qzero()) {
    return(qzero())
  }
  # we store the successive leading terms in LTs_f
  d <- max(arity(qspray), max(vapply(divisors, arity, integer(1L))))
  oqspray <- orderedQspray(qspray, d)
  opowers <- oqspray[["powers"]]
  ocoeffs <- oqspray[["coeffs"]]
  LTs_f <- lapply(seq_along(ocoeffs), function(i) {
    list("powers" = opowers[i, ], "coeff" = ocoeffs[i])
  })
  
  ndivisors <- length(divisors)
  nterms <- length(qspray@coeffs)

  cur <- qspray
  for(k in 1L:nterms) {
    LT_cur <- LTs_f[[k]]
    i <- 1L
    while(i <= ndivisors) {
      g    <- divisors[[i]]
      LT_g <- LTdivisors[[i]] 
      while(divides(LT_g, LT_cur)) {
        cur <- cur - quotient(LT_cur, LT_g) * g
        if(cur == qzero()) {
          return(qzero())
        }
        LT_cur <- leadingTerm(cur, d)
      }
      i <- i + 1L
    }
  }
  # return remainder
  cur
}
