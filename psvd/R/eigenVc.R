eigenVc <- function(xmat, wp, d, r, itmax, err) {
    .Call(C_eigenV,
          xmatC = as.double(xmat),
          wpC = as.double(wp),
          dC = as.integer(d),
          rC = as.integer(r),
          itmaxC = as.integer(itmax),
          errC = as.double(err));
}
