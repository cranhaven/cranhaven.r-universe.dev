# Taken from MASS
#
# Compute the null space of a matrix, i.e. another matrix such that its
#   cross-product with x is null

nullspace <- function(x){
  tmp <- qr(x)
  set <- if(tmp$rank == 0L) seq_len(ncol(x)) else  -seq_len(tmp$rank)
  qr.Q(tmp, complete = TRUE)[, set, drop = FALSE]
}