#' normalize
#'
#' Normalize a vector or a matrix (by columns), using euclidian norm
#'
#' @param x Vector or matrix to be normalized
#'
#' @return The normalized matrix (1 column if x is a vector)
#'
#' @examples
#' x <- matrix(c(1,2,-1,3), ncol=2)
#' normalize(x) #column 1 is 1/sqrt(5) (1 2),
#'              #and column 2 is 1/sqrt(10) (-1, 3)
#' @export
normalize <- function(x)
{
  x <- as.matrix(x)
  norm2 <- sqrt( colSums(x^2) )
  sweep(x, 2, norm2, '/')
}

# Computes a tensor-vector product
#
# @param Te third-order tensor (size dxdxd)
# @param w vector of size d
#
# @return Matrix of size dxd
#
.T_I_I_w <- function(Te, w)
{
  d <- length(w)
  Ma <- matrix(0,nrow=d,ncol=d)
  for (j in 1:d)
    Ma <- Ma + w[j] * Te[,,j]
  Ma
}

# Computes the second-order empirical moment between input X and output Y
#
# @param X matrix of covariates (of size n*d)
# @param Y vector of responses (of size n)
#
# @return Matrix of size dxd
#
.Moments_M2 <- function(X, Y)
{
  n <- nrow(X)
  d <- ncol(X)
  M2 <- matrix(0,nrow=d,ncol=d)
  matrix( .C("Moments_M2", X=as.double(X), Y=as.double(Y), pn=as.integer(n),
    pd=as.integer(d), M2=as.double(M2), PACKAGE="morpheus")$M2, nrow=d, ncol=d)
}

# Computes the third-order empirical moment between input X and output Y
#
# @param X matrix of covariates (of size n*d)
# @param Y vector of responses (of size n)
#
# @return Array of size dxdxd
#
.Moments_M3 <- function(X, Y)
{
  n <- nrow(X)
  d <- ncol(X)
  M3 <- array(0,dim=c(d,d,d))
  array( .C("Moments_M3", X=as.double(X), Y=as.double(Y), pn=as.integer(n),
    pd=as.integer(d), M3=as.double(M3), PACKAGE="morpheus")$M3, dim=c(d,d,d) )
}

#' computeMoments
#'
#' Compute cross-moments of order 1,2,3 from X,Y
#'
#' @inheritParams computeMu
#'
#' @return A list L where L[[i]] is the i-th cross-moment
#'
#' @examples
#' X <- matrix(rnorm(100), ncol=2)
#' Y <- rbinom(100, 1, .5)
#' M <- computeMoments(X, Y)
#'
#' @export
computeMoments = function(X, Y)
  list( colMeans(Y * X), .Moments_M2(X,Y), .Moments_M3(X,Y) )

# Find the optimal assignment (permutation) between two sets (minimize cost)
#
# @param distances The distances matrix, in columns
#   (distances[i,j] is distance between i and j)
#
# @return A permutation minimizing cost
#
.hungarianAlgorithm <- function(distances)
{
  n <- nrow(distances)
  .C("hungarianAlgorithm", distances=as.double(distances), pn=as.integer(n),
    assignment=integer(n), PACKAGE="morpheus")$assignment
}

#' alignMatrices
#'
#' Align a set of parameters matrices, with potential permutations.
#'
#' @param Ms A list of matrices, all of same size DxK
#' @param ref A reference matrix to align other matrices with
#' @param ls_mode How to compute the labels assignment: "exact" for exact algorithm
#'   (default, but might be time-consuming, complexity is O(K^3) ), or "approx1", or
#'   "approx2" to apply a greedy matching algorithm (heuristic) which for each column in
#'   reference (resp. in current row) compare to all unassigned columns in current row
#'   (resp. in reference)
#'
#' @return The aligned list (of matrices), of same size as Ms
#'
#' @examples
#' m1 <- matrix(c(1,1,0,0),ncol=2)
#' m2 <- matrix(c(0,0,1,1),ncol=2)
#' ref <- m1
#' Ms <- list(m1, m2, m1, m2)
#' a <- alignMatrices(Ms, ref, "exact")
#' # a[[i]] is expected to contain m1 for all i
#'
#' @export
alignMatrices <- function(Ms, ref, ls_mode=c("exact","approx1","approx2"))
{
  if (!is.matrix(ref) || any(is.na(ref)))
    stop("ref: matrix, no NAs")
  ls_mode <- match.arg(ls_mode)

  K <- ncol(Ms[[1]])
  L <- length(Ms)
  for (i in 1:L)
  {
    m <- Ms[[i]] #shorthand

    if (ls_mode == "exact")
    {
      #distances[i,j] = distance between m column i and ref column j
      distances = apply( ref, 2, function(col) ( sqrt(colSums((m-col)^2)) ) )
      assignment = .hungarianAlgorithm(distances)
      col <- m[,assignment]
      Ms[[i]] <- col
    }
    else
    {
      # Greedy matching:
      #   approx1: li[[i]][,j] is assigned to m[,k] minimizing dist(li[[i]][,j],m[,k'])
      #   approx2: m[,j] is assigned to li[[i]][,k] minimizing dist(m[,j],li[[i]][,k'])
      available_indices = 1:K
      for (j in 1:K)
      {
        distances =
          if (ls_mode == "approx1")
          {
            apply(as.matrix(m[,available_indices]), 2,
              function(col) ( sqrt(sum((col - ref[,j])^2)) ) )
          }
          else #approx2
          {
            apply(as.matrix(ref[,available_indices]), 2,
              function(col) ( sqrt(sum((col - m[,j])^2)) ) )
          }
        indMin = which.min(distances)
        if (ls_mode == "approx1")
        {
          col <- m[ , available_indices[indMin] ]
          Ms[[i]][,j] <- col
        }
        else #approx2
        {
          col <- available_indices[indMin]
          Ms[[i]][,col] <- m[,j]
        }
        available_indices = available_indices[-indMin]
      }
    }
  }
  Ms
}
