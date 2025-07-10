#' Polar/Jordan form of matrices P and D
#' @description Converts the complex matrices P and D into matrices of eigenvectors and eigenvalues with real entries.
#' @param P, the eigenvectors from an eigenvalue decomposition.
#' @param D,  the eigenvalues from an eigenvalue decomposition.
#' @usage polar(P,D)
#' @return P the polar form (real-valued) matrix of eigenvectors. D the polar form (real-valued) matrix of eigenvalues.
#' @examples
#' z <- complex(real = rnorm(16), imag = rnorm(16))
#' M <- matrix(z,nrow=4)
#' decomp <- eigen(M)
#' polar(decomp$vectors,decomp$values)
#' @author Kyle Caudle
#' @author Randy Hoover
#' @author Jackson Cates
#' @author Everett Sandbo
#' @references Bhatia, R. (2013). Matrix analysis (Vol. 169). Springer Science & Business Media.


polar <- function(P,D) {
  # Creates the polar/Jordan form of the
  # P and D matrices after performing
  # eigenvalue decomposition where the
  # eigenvalue values are complex.

  # Input: P, the matrix of eigenvectors from an eigen
  # value decomposition.  D, the eigenvalues
  # from an eigenvalue deocmpostion.

  # Output: P the polar form (real-valued)
  # matrix of eigenvectors.
  # D the polar form (real-valued) matrix
  # of eigenvalues.

  n <- length(D)
  retV <- matrix(0,nrow=n,ncol=n)
  retD <- matrix(0,nrow=n,ncol=n)
  for (i in 1:n) {
    V = P
    L = D
    Vtemp = V
    LD <- diag(L)
    j <- 1
    while (j <=length(L)){
      if (is.complex(L[j]) && (abs(Im(L[j])) > 0.0001)){
        # Process V
        V[,j] <- Re(Vtemp[,j])
        V[,(j+1)] <- Im(Vtemp[,j])

        # Process LD
        LD[j,j] <- Re(L[j])
        LD[j,(j+1)] <- Im(L[j])
        LD[(j+1),j] <- -Im(L[j])
        LD[(j+1),(j+1)] <- Re(L[j])
        j <- j+1
      }
      j <- j+1
    }
    V = Re(V)
    LD = Re(LD)
    return(list(P = V, D = LD))
  }
}
