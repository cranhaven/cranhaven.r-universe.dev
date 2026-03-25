expm <- function(x, t=1)
{
  if (nrow(x) != ncol(x))
    stop("Matrix exponentiation is only defined for square matrices.")
  
  ret <- expokit_dgpadm_Qmat(Qmat=x, t=t, transpose_needed=TRUE)
  
  return( ret )
}


