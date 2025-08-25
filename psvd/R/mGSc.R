mGSc <- function(amat, m, n) {
    .Call(C_mGS,
          amatC = as.double(amat),
          mC = as.integer(m),
          nC = as.integer(n));
}
