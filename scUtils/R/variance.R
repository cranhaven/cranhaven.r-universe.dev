
#' @title Variance computation for sparse matrices
#' @description Compute variance for each column / each row of
#' a dgCMatrix (from Matrix package).
#' @param spm A sparse matrix of class dgCMatrix from the Matrix package.
#' @return Vector with variances.
#' @details The only supported format currently is dgCMatrix. While the Matrix
#' package has other formats, this one is used for scRNAseq raw count data.
#' Function code written by Simon Anders.
#' @examples
#' library(Matrix)
#'  mat <- as(matrix(rpois(900,1), ncol=3), "dgCMatrix")
#'  colVars_spm(mat)
#' @rdname colVars_spm
#' @seealso \code{vignette("Intro2Matrix", package="Matrix")}
#' \link[Matrix]{CsparseMatrix-class}
#' @export
colVars_spm <- function( spm ) {
  stopifnot( methods::is( spm, "dgCMatrix" ) )
  ans <- sapply( base::seq.int(spm@Dim[2]), function(j) {
    if( spm@p[j+1] == spm@p[j] ) { return(0) } # all entries are 0: var is 0
    mean <- base::sum( spm@x[ (spm@p[j]+1):spm@p[j+1] ] ) / spm@Dim[1]
    sum( ( spm@x[ (spm@p[j]+1):spm@p[j+1] ] - mean )^2 ) +
      mean^2 * ( spm@Dim[1] - ( spm@p[j+1] - spm@p[j] ) ) } ) / ( spm@Dim[1] - 1 )
  names(ans) <- spm@Dimnames[[2]]
  ans
}

#' same for row-wise matrix computation
#'
#' @rdname colVars_spm
#' @export
rowVars_spm <- function( spm ) {
  stopifnot( methods::is( spm, "dgCMatrix" ) )
  colVars_spm( Matrix::t(spm) )
}

