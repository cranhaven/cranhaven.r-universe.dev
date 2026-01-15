#' @useDynLib EigenR
#' @importFrom Rcpp evalCpp
#' @noRd
NULL

#' Determinant of a matrix
#' 
#' @description Determinant of a real or complex matrix.
#' 
#' @param M a square matrix or \code{\link{SparseMatrix}}, real or complex
#'
#' @return The determinant of \code{M}.
#' @export
#' 
#' @examples set.seed(666)
#' M <- matrix(rpois(25, 1), 5L, 5L)
#' Eigen_det(M)
#' # determinants of complex matrices are supported:
#' Eigen_det(M + 1i * M)
#' # as well as determinants of sparse matrices:
#' Eigen_det(asSparseMatrix(M))
#' Eigen_det(asSparseMatrix(M + 1i * M))
Eigen_det <- function(M){
  if(inherits(M, "SparseMatrix")){
    stopifnot(M[["nrows"]] == M[["ncols"]])
    if(is.complex(M[["Mij"]])){
      EigenR_det_sparse_cplx(
        M[["i"]], M[["j"]], M[["Mij"]], M[["nrows"]], M[["ncols"]]
      )
    }else{
      EigenR_det_sparse_real(
        M[["i"]], M[["j"]], M[["Mij"]], M[["nrows"]], M[["ncols"]]
      )
    }
  }else{
    stopifnot(isSquareMatrix(M))
    stopifnot(isRealOrComplex(M))
    if(is.complex(M)){
      EigenR_det_cplx(Re(M), Im(M))
    }else{
      EigenR_det_real(M)
    }
  }
}

#' Absolute value of the determinant
#' 
#' @description Absolute value of the determinant of a real matrix.
#' 
#' @param M a \emph{real} square matrix
#'
#' @return The absolute value of the determinant of \code{M}.
#' @export
#' 
#' @note `Eigen_absdet(M)` is not faster than `abs(Eigen_det(M))`.
#' 
#' @examples set.seed(666L)
#' M <- matrix(rpois(25L, 1), 5L, 5L)
#' Eigen_absdet(M)
Eigen_absdet <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isReal(M))
  EigenR_absdet(M)
}

#' Logarithm of the absolute value of the determinant
#' @description Logarithm of the absolute value of the determinant of a real 
#'   matrix.
#' 
#' @param M a \emph{real} square matrix
#'
#' @return The logarithm of the absolute value of the determinant of \code{M}.
#' @export
#' 
#' @note `Eigen_logabsdet(M)` is not faster than `log(abs(Eigen_det(M)))`.
#' 
#' @examples set.seed(666L)
#' M <- matrix(rpois(25L, 1), 5L, 5L)
#' Eigen_logabsdet(M)
Eigen_logabsdet <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isReal(M))
  EigenR_logabsdet(M)
}

#' Check injectivity
#' @description Checks whether a matrix represents an injective linear map 
#'   (i.e. has trivial kernel).
#' 
#' @param M a matrix, real or complex
#'
#' @return A Boolean value indicating whether \code{M} represents an injective 
#'   linear map.
#' @export
#' 
#' @examples set.seed(666L)
#' M <- matrix(rpois(35L, 1), 5L, 7L)
#' Eigen_isInjective(M)
Eigen_isInjective <- function(M){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    EigenR_isInjective_cplx(Re(M), Im(M))
  }else{
    EigenR_isInjective_real(M)
  }
}

#' Check surjectivity
#' @description Checks whether a matrix represents a surjective linear map.
#' 
#' @param M a matrix, real or complex
#'
#' @return A Boolean value indicating whether \code{M} represents a surjective 
#'   linear map.
#' @export
#' 
#' @examples set.seed(666L)
#' M <- matrix(rpois(35L, 1), 7L, 5L)
#' Eigen_isSurjective(M)
Eigen_isSurjective <- function(M){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    EigenR_isSurjective_cplx(Re(M), Im(M))
  }else{
    EigenR_isSurjective_real(M)
  }
}

#' Check invertibility
#' @description Checks whether a matrix is invertible.
#' 
#' @param M a matrix, real or complex
#'
#' @return A Boolean value indicating whether \code{M} is invertible.
#' @export
#' 
#' @examples set.seed(666L)
#' M <- matrix(rpois(25L, 1), 5L, 5L)
#' Eigen_isInvertible(M)
Eigen_isInvertible <- function(M){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    EigenR_isInvertible_cplx(Re(M), Im(M))
  }else{
    EigenR_isInvertible_real(M)
  }
}


#' Rank of a matrix
#' @description Rank of a real or complex matrix.
#'
#' @param M a matrix, real or complex
#'
#' @return The rank of \code{M}.
#' @export
Eigen_rank <- function(M){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    EigenR_rank_cplx(Re(M), Im(M))
  }else{
    EigenR_rank_real(M)
  }
}

#' Inverse of a matrix
#' 
#' @description Inverse of a real or complex matrix.
#'
#' @param M an invertible square matrix, real or complex
#'
#' @return The inverse matrix of \code{M}.
#' @export
Eigen_inverse <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_inverse_cplx(Re(M), Im(M))
    Minv <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Minv <- EigenR_inverse_real(M)
  }
  Minv
}

#' Pseudo-inverse of a matrix
#' 
#' @description Pseudo-inverse of a real or complex matrix 
#'   (Moore-Penrose generalized inverse).
#'
#' @param M a matrix, real or complex, not necessarily square
#'
#' @return The pseudo-inverse matrix of \code{M}.
#' @export
#' 
#' @examples library(EigenR)
#' M <- rbind(
#'   toeplitz(c(3, 2, 1)), 
#'   toeplitz(c(4, 5, 6))
#' )
#' Mplus <- Eigen_pinverse(M)
#' all.equal(M, M %*% Mplus %*% M)
#' all.equal(Mplus, Mplus %*% M %*% Mplus)
#' #' a complex matrix
#' A <- M + 1i * M[, c(3L, 2L, 1L)]
#' Aplus <- Eigen_pinverse(A)
#' AAplus <- A %*% Aplus
#' all.equal(AAplus, t(Conj(AAplus))) #' `A %*% Aplus` is Hermitian
#' AplusA <- Aplus %*% A
#' all.equal(AplusA, t(Conj(AplusA))) #' `Aplus %*% A` is Hermitian
Eigen_pinverse <- function(M){
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_pseudoInverse_cplx(Re(M), Im(M))
    Mpinv <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Mpinv <- EigenR_pseudoInverse_real(M)
  }
  Mpinv
}

#' Dimension of kernel
#' 
#' @description Dimension of the kernel of a matrix.
#' 
#' @param M a matrix, real or complex
#'
#' @return An integer, the dimension of the kernel of \code{M}.
#' @export
#'
#' @seealso \code{\link{Eigen_isInjective}}, \code{\link{Eigen_kernel}}.
#' 
#' @examples set.seed(666L)
#' M <- matrix(rpois(35L, 1), 5L, 7L)
#' Eigen_kernelDimension(M)
Eigen_kernelDimension <- function(M){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    EigenR_kernelDimension_cplx(Re(M), Im(M))
  }else{
    EigenR_kernelDimension_real(M)
  }
}

#' Kernel of a matrix
#' 
#' @description Kernel (null-space) of a real or complex matrix.
#'
#' @param M a matrix, real or complex
#' @param method one of \code{"COD"} or \code{"LU"}; the faster method depends 
#'   on the size of the matrix
#'
#' @return A basis of the kernel of \code{M}. With \code{method = "COD"}, the 
#'   basis is orthonormal, while it is not with \code{method = "LU"}.
#' @export
#' 
#' @seealso \code{\link{Eigen_kernelDimension}}.
#'
#' @examples set.seed(666)
#' M <- matrix(rgamma(30L, 12, 1), 10L, 3L)
#' M <- cbind(M, M[,1]+M[,2], M[,2]+2*M[,3])
#' # basis of the kernel of `M`:
#' Eigen_kernel(M, method = "LU")
#' # orthonormal basis of the kernel of `M`:
#' Eigen_kernel(M, method = "COD")
Eigen_kernel <- function(M, method = "COD"){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  method <- match.arg(tolower(method), c("cod", "lu"))
  if(is.complex(M)){
    if(method == "cod"){
      parts <- EigenR_kernel_COD_cplx(Re(M), Im(M))
    }else{
      parts <- EigenR_kernel_LU_cplx(Re(M), Im(M))
    }
    parts[["real"]] + 1i * parts[["imag"]]
  }else{
    if(method == "cod"){
      EigenR_kernel_COD_real(M)
    }else{
      EigenR_kernel_LU_real(M)
    }
  }
}

#' Range of a matrix
#' 
#' @description Range (column-space, image, span) of a real or complex matrix.
#'
#' @param M a matrix, real or complex
#' @param method one of \code{"LU"}, \code{"QR"}, or \code{"COD"}; the 
#'   \code{"LU"} method is faster
#'
#' @return A basis of the range of \code{M}. With \code{method = "LU"}, the 
#'   basis is not orthonormal, while it is with \code{method = "QR"} and 
#'   \code{method = "COD"}.
#' @export
Eigen_range <- function(M, method = "QR"){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  method <- match.arg(tolower(method), c("lu", "qr", "cod"))
  if(is.complex(M)){
    if(method == "qr"){
      parts <- EigenR_image_QR_cplx(Re(M), Im(M))
    }else if(method == "lu"){
      parts <- EigenR_image_LU_cplx(Re(M), Im(M))
    }else{
      parts <- EigenR_image_COD_cplx(Re(M), Im(M))
    }
    parts[["real"]] + 1i * parts[["imag"]]
  }else{
    if(method == "qr"){
      EigenR_image_QR_real(M)
    }else if(method == "lu"){
      EigenR_image_LU_real(M)
    }else{
      EigenR_image_COD_real(M)
    }
  }
}

#' QR decomposition of a matrix
#' 
#' @description QR decomposition of a real or complex matrix.
#'
#' @param M a matrix, real or complex
#'
#' @return A list with the \code{Q} matrix and the \code{R} matrix.
#' @export
#'
#' @examples M <- cbind(c(1,2,3), c(4,5,6))
#' x <- Eigen_QR(M)
#' x$Q %*% x$R
Eigen_QR <- function(M){
  stopifnot(is.matrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    QRparts <- EigenR_QR_cplx(Re(M), Im(M))
    lapply(QRparts, function(parts) parts[["real"]] + 1i * parts[["imag"]])
  }else{
    EigenR_QR_real(M)
  }
}

#' Cholesky decomposition of a matrix
#' 
#' @description Cholesky decomposition of a symmetric or Hermitian matrix.
#'
#' @param M a square symmetric/Hermitian positive-definite matrix or 
#'   \code{\link{SparseMatrix}}, real/complex
#'
#' @return The upper triangular factor of the Cholesky decomposition of 
#'   \code{M}.
#' @export
#' 
#' @details Symmetry is not checked; only the lower triangular part of 
#'   \code{M} is used.
#'
#' @examples M <- rbind(c(5,1), c(1,3))
#' U <- Eigen_chol(M)
#' t(U) %*% U # this is `M`
#' # a Hermitian example:
#' A <- rbind(c(1,1i), c(1i,2))
#' ( M <- A %*% t(Conj(A)) )
#' try(chol(M)) # fails
#' U <- Eigen_chol(M) 
#' t(Conj(U)) %*% U # this is `M`
#' # a sparse example
#' M <- asSparseMatrix(diag(1:5))
#' Eigen_chol(M)
Eigen_chol <- function(M){
  if(inherits(M, "SparseMatrix")){
    stopifnot(M[["nrows"]] == M[["ncols"]])
    if(is.complex(M[["Mij"]])){
      EigenR_chol_sparse_cplx(
        M[["i"]], M[["j"]], M[["Mij"]], M[["nrows"]], M[["ncols"]]
      )
    }else{
      EigenR_chol_sparse_real(
        M[["i"]], M[["j"]], M[["Mij"]], M[["nrows"]], M[["ncols"]]
      )
    }
  }else{
    stopifnot(isSquareMatrix(M))
    stopifnot(isRealOrComplex(M))
    if(is.complex(M)){
      EigenR_chol_cplx(Re(M), Im(M))
    }else{
      EigenR_chol_real(M)
    }
  }
}

#' 'UtDU' decomposition of a matrix
#' 
#' @description Cholesky-'UtDU' decomposition of a symmetric or Hermitian matrix.
#'
#' @param M a square symmetric/Hermitian positive or negative semidefinite 
#'   matrix, real/complex
#'
#' @return The Cholesky-'UtDU' decomposition of \code{M} in a list 
#'   (see example).
#' @export
#' 
#' @details Symmetry is not checked; only the lower triangular part of 
#'   \code{M} is used.
#'
#' @examples x <- matrix(c(1:5, (1:5)^2), 5, 2)
#' x <- cbind(x, x[, 1] + 3*x[, 2])
#' M <- crossprod(x)
#' UtDU <- Eigen_UtDU(M)
#' U <- UtDU$U
#' D <- UtDU$D
#' perm <- UtDU$perm
#' UP <- U[, perm]
#' t(UP) %*% diag(D) %*% UP # this is `M`
Eigen_UtDU <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    utdu <- EigenR_UtDU_cplx(Re(M), Im(M))
    utdu[["U"]] <- utdu[["U"]][["real"]] + 1i * utdu[["U"]][["imag"]]
    utdu[["D"]] <- utdu[["D"]][["real"]] + 1i * utdu[["D"]][["imag"]]
    utdu[["perm"]] <- 1L + utdu[["perm"]]
    utdu
  }else{
    utdu <- EigenR_UtDU_real(M)
    utdu[["perm"]] <- 1L + utdu[["perm"]]
    utdu
  }
}

#' Linear least-squares problems
#' 
#' @description Solves a linear least-squares problem.
#'
#' @param A a \code{n*p} matrix, real or complex
#' @param b a vector of length \code{n} or a matrix with \code{n} rows, 
#'   real or complex
#' @param method the method used to solve the problem, either \code{"svd"} 
#'   (based on the SVD decomposition) or \code{"cod"} (based on the 
#'   complete orthogonal decomposition)
#'
#' @return The solution \code{X} of the least-squares problem \code{AX ~= b} 
#'   (similar to \code{lm.fit(A, b)$coefficients}). This is a matrix if 
#'   \code{b} is a matrix, or a vector if \code{b} is a vector.
#' @export
#'
#' @examples set.seed(129)
#' n <- 7; p <- 2
#' A <- matrix(rnorm(n * p), n, p)
#' b <- rnorm(n)
#' lsfit <- Eigen_lsSolve(A, b)
#' b - A %*% lsfit # residuals
Eigen_lsSolve <- function(A, b, method = "cod"){
  method <- match.arg(tolower(method), c("svd", "cod"))
  stopifnot(is.matrix(A)) 
  stopifnot(is.atomic(b))
  b <- cbind(b)
  stopifnot(nrow(A) == nrow(b))
  stopifnot(is.numeric(A) || is.complex(A))
  stopifnot(is.numeric(b) || is.complex(b))
  if(method == "svd"){
    if(is.complex(A) || is.complex(b)){
      parts <- EigenR_lsSolve_cplx(Re(A), Im(A), Re(b), Im(b))
      parts[["real"]][,] + 1i * parts[["imag"]][,]
    }else{
      EigenR_lsSolve_real(A, b)[,]
    }
  }else{
    if(is.complex(A) || is.complex(b)){
      parts <- EigenR_lsSolve_cod_cplx(Re(A), Im(A), Re(b), Im(b))
      parts[["real"]][,] + 1i * parts[["imag"]][,]
    }else{
      EigenR_lsSolve_cod_real(A, b)[,]
    }  	
  }
}

#' Exponential of a matrix
#' 
#' @description Exponential of a real or complex square matrix.
#'
#' @param M a square matrix, real or complex
#'
#' @return The exponential of \code{M}.
#' @export
Eigen_exp <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_exp_cplx(Re(M), Im(M))
    Mexp <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Mexp <- EigenR_exp_real(M)
  }
  Mexp
}

#' Logarithm of a matrix
#' 
#' @description Logarithm of a real or complex square matrix, when possible.
#'
#' @param M a square matrix, real or complex
#'
#' @return The logarithm of \code{M}.
#' @details The logarithm of a matrix does not always exist. 
#'   See \href{https://eigen.tuxfamily.org/dox/unsupported/group__MatrixFunctions__Module.html#title7}{matrix logarithm}.
#' @export
Eigen_log <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_log_cplx(Re(M), Im(M))
    Mlog <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Mlog <- EigenR_log_real(M)
  }
  Mlog
}

#' Matrix cosine
#' 
#' @description Matrix cosine of a real or complex square matrix.
#'
#' @param M a square matrix, real or complex
#'
#' @return The matrix cosine of \code{M}.
#' @export
#' @examples library(EigenR)
#' M <- toeplitz(c(1,2,3))
#' cosM <- Eigen_cos(M) 
#' sinM <- Eigen_sin(M)
#' cosM %*% cosM + sinM %*% sinM # identity matrix
Eigen_cos <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_cos_cplx(Re(M), Im(M))
    Mcos <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Mcos <- EigenR_cos_real(M)
  }
  Mcos
}

#' Matrix sine
#' 
#' @description Matrix sine of a real or complex square matrix.
#'
#' @param M a square matrix, real or complex
#'
#' @return The matrix sine of \code{M}.
#' @export
Eigen_sin <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_sin_cplx(Re(M), Im(M))
    Msin <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Msin <- EigenR_sin_real(M)
  }
  Msin
}

#' Matrix hyperbolic cosine
#' 
#' @description Matrix hyperbolic cosine of a real or complex square matrix.
#'
#' @param M a square matrix, real or complex
#'
#' @return The matrix hyperbolic cosine of \code{M}.
#' @export
#' @examples library(EigenR)
#' M <- toeplitz(c(1,2,3))
#' Eigen_cosh(M)
#' (Eigen_exp(M) + Eigen_exp(-M)) / 2 # identical
Eigen_cosh <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_cosh_cplx(Re(M), Im(M))
    Mcosh <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Mcosh <- EigenR_cosh_real(M)
  }
  Mcosh
}

#' Matrix hyperbolic sine
#' 
#' @description Matrix hyperbolic sine of a real or complex square matrix.
#'
#' @param M a square matrix, real or complex
#'
#' @return The matrix hyperbolic sine of \code{M}.
#' @export
#' @examples library(EigenR)
#' M <- toeplitz(c(1,2,3))
#' Eigen_sinh(M)
#' (Eigen_exp(M) - Eigen_exp(-M)) / 2  # identical
Eigen_sinh <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_sinh_cplx(Re(M), Im(M))
    Msinh <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Msinh <- EigenR_sinh_real(M)
  }
  Msinh
}

#' Matricial power
#' 
#' @description Matricial power of a real or complex square matrix, when possible.
#'
#' @param M a square matrix, real or complex
#' @param p a number, real or complex, the power exponent
#'
#' @return The matrix \code{M} raised at the power \code{p}.
#' @details The power is defined with the help of the exponential and the 
#'   logarithm. See \href{https://eigen.tuxfamily.org/dox/unsupported/group__MatrixFunctions__Module.html#title8}{matrix power}.
#' @export
Eigen_pow <- function(M, p){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  stopifnot(isRealOrComplexScalar(p))
  if(is.complex(p) && !is.complex(M)){
    Zeros <- matrix(0, nrow = nrow(M), ncol = ncol(M))
    parts <- EigenR_pow_cplx(Re(M), Zeros, p)
    Mpow <- parts[["real"]] + 1i * parts[["imag"]]
  }else if(is.complex(M) && !is.complex(p)){
    p <- as.complex(p)
  }
  if(is.complex(M) && is.complex(p)){
    parts <- EigenR_pow_cplx(Re(M), Im(M), p)
    Mpow <- parts[["real"]] + 1i * parts[["imag"]]
  }else if(!is.complex(M) && !is.complex(p)){
    Mpow <- EigenR_pow_real(M, p)
  }
  Mpow
}


#' Square root of a matrix
#' 
#' @description Square root of a real or complex square matrix, when possible.
#'
#' @param M a square matrix, real or complex
#'
#' @return A square root of \code{M}.
#' @details See \href{https://eigen.tuxfamily.org/dox/unsupported/group__MatrixFunctions__Module.html#title12}{matrix square root}.
#' @export
#' @examples # Rotation matrix over 60 degrees:
#' M <- cbind(c(cos(pi/3), sin(pi/3)), c(-sin(pi/3), cos(pi/3)))
#' # Its square root, the rotation matrix over 30 degrees:
#' Eigen_sqrt(M)
Eigen_sqrt <- function(M){
  stopifnot(isSquareMatrix(M))
  stopifnot(isRealOrComplex(M))
  if(is.complex(M)){
    parts <- EigenR_sqrt_cplx(Re(M), Im(M))
    Msqrt <- parts[["real"]] + 1i * parts[["imag"]]
  }else{
    Msqrt <- EigenR_sqrt_real(M)
  }
  Msqrt
}

#' Real Schur decomposition
#' @description Real Schur decomposition of a square matrix.
#'
#' @param M real square matrix
#'
#' @return A list with the \code{T} and \code{U} matrices.
#' @export
#' @details See \href{https://eigen.tuxfamily.org/dox/classEigen_1_1RealSchur.html}{Eigen::RealSchur}.
#'
#' @examples
#' library(EigenR)
#' M <- cbind(c(3, 2, 3), c(1, 1, 1), c(5, 0, -2))
#' schur <- Eigen_realSchur(M)
#' T <- schur$T
#' U <- schur$U
#' M - U %*% T %*% t(U)
Eigen_realSchur <- function(M) {
  stopifnot(isSquareMatrix(M), isReal(M))
  EigenR_realSchur(M)
}


#' Complex Schur decomposition
#' @description Complex Schur decomposition of a square matrix.
#'
#' @param M real or complex square matrix
#'
#' @return A list with the \code{T} and \code{U} matrices.
#' @export
#' @details See \href{https://eigen.tuxfamily.org/dox/classEigen_1_1ComplexSchur.html}{Eigen::ComplexSchur}.
#'
#' @examples
#' library(EigenR)
#' M <- cbind(c(3, 2i, 1+3i), c(1, 1i, 1), c(5, 0, -2i))
#' schur <- Eigen_complexSchur(M)
#' T <- schur$T
#' U <- schur$U
#' M - U %*% T %*% t(Conj(U))
Eigen_complexSchur <- function(M) {
  stopifnot(isSquareMatrix(M), isRealOrComplex(M))
  schur <- EigenR_complexSchur(Re(M), Im(M))
  T <- schur[["T"]]
  U <- schur[["U"]]
  list(
    "T" = T[["real"]] + 1i * T[["imag"]],
    "U" = U[["real"]] + 1i * U[["imag"]]
  )
}

#' Hessenberg decomposition
#' @description Hessenberg decomposition of a square matrix.
#'
#' @param M real or complex square matrix
#'
#' @return A list with the \code{H} and \code{Q} matrices.
#' @export
#' @details See \href{https://eigen.tuxfamily.org/dox/classEigen_1_1HessenbergDecomposition.html}{Eigen::HessenbergDecomposition}.
#'
#' @examples
#' library(EigenR)
#' M <- cbind(c(3, 2i, 1+3i), c(1, 1i, 1), c(5, 0, -2i))
#' Eigen_Hessenberg(M)
Eigen_Hessenberg <- function(M) {
  stopifnot(isSquareMatrix(M), isRealOrComplex(M))
  if(isReal(M)) {
    EigenR_Hessenberg_real(M)
  } else {
    hd <- EigenR_Hessenberg_cplx(Re(M), Im(M))
    H <- hd[["H"]]
    Q <- hd[["Q"]]
    list(
      "H" = H[["real"]] + 1i * H[["imag"]],
      "Q" = Q[["real"]] + 1i * Q[["imag"]]
    )
  }
}
