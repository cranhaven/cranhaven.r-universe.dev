#' Minimal or maximal \eqn{2 \times 2} (non-zero) matrix of missing values
#' 
#' Finds \eqn{2 \times 2} (non-zero) block with the minimum or maximum
#' amount of missing values within a general missing values matrix derived from
#' satellite images.
#' 
#' @param sieve matrix
#' @param  type character. Default is \code{"min"}.
#' @param  rank numeric. See \bold{Details}. 
#' 
#' @export
#' 
#' @details
#' In what follows we describe the case \code{type="min"}. This function searches for the 
#' minimal \eqn{2\times 2} (non-zero) sub-matrix within a \emph{general} \code{sieve} matrix. 
#' \emph{blockMissingness} is defined as \eqn{\log( cumsum (a_{i,j}) )} for \eqn{1\leq i,j\leq 2}. 
#' The minimal block is defined as that block with the minimum blockMissingness. The \code{cumsum} function 
#' is preferred rather than other quantities, such as \code{cumprod}
#' or \code{det}, because \code{sieve} could have a large amount of zeros.
#' 
#' In the first stage of the search, a vector with the sorted non-zero
#' values of \code{sieve} is calculated. Consider the \eqn{i}-th entry of this sorted
#' vector. This value corresponds to some (maybe more than once) cell within \code{sieve}.
#' Notice that, with the exception of the edges of the sieve, this cell belongs to four \eqn{2\times 2} matrices. The blockMissingness
#' of each of these 4 matrices is calculated. The matrix with the smallest blockMissingness
#' is called a \emph{localMinBlock}.
#' 
#' The procedure just described is applied to each of the \code{rank} entries of the
#' sorted vector; the \emph{globalMinBlock} is that localMinBlock with the
#' smallest blockMissingness.
#' 
#' The case \code{type="max"} is analogous to the one above but the searches is now for the cell with 
#' the largest blockMissingness and the search now runs (in descending order) over the last \code{rank}-th entries 
#' of the sorted vector.
#' 
#' The argument \code{rank} is defined as follows. Let \code{sorted_vector} be a numeric vector
#' with the non-zero, ordered (in ascending order) values of \code{sieve}. When \code{type="min"},
#' \code{rank} defines the first \code{rank}-th values of \code{sorted_vector}. When \code{type="max"},
#' \code{rank} defines the last \code{rank}-th values of \code{sorted_vector}.
#' 
#' @seealso \code{\link[igapfill]{mvSieve}}
#' 
#' @return A list containing:
#' \item{rows}{a numeric vector given the rows of \code{sieve} where the minimal \eqn{2\times 2} block is found.}
#' \item{cols}{a numeric vector given the cols of \code{sieve} where the minimal \eqn{2\times 2} block is found.}
#' \item{block}{a \eqn{2\times 2} sub-matrix of \code{sieve},  the actual minimal block.}
#' \item{blockMissingness}{a numeric with the blockMissingness of the minimal block.}
#' 
minmaxBlock <- function(sieve, type=c("min", "max"), rank){
  type <- match.arg(type)
  
  out <- list()
  
  if( type == "min" ){
    out <- sieveMinBlock(sieve=sieve, rank=rank)
  } else {
    out <- sieveMaxBlock(sieve=sieve, rank=rank)
  }
  
  out
}

sieveMinBlock <- function(sieve, rank=25){
  
  if( !inherits(sieve, "matrix" ) ){
    stop("sieve must be a matrix")
  }
  
  sieve_aux <- sieve
  
  sieve_aux[ sieve_aux == 0 ] <- NA
  
  sortedValues <- base::sort(sieve_aux)
  
  getGlobalMinMaxBlock(sorted=sortedValues[1:rank], sieve = sieve)
}

sieveMaxBlock <- function(sieve, rank=25){
  
  if( !inherits(sieve, "matrix" ) ){
    stop("sieve must be a matrix")
  }
  
  sieve_aux <- sieve
  
  sieve_aux[ sieve_aux == 0 ] <- NA
  
  sortedValues <- base::sort(sieve_aux)
  
  getGlobalMinMaxBlock(sorted=sortedValues[length(sortedValues):(length(sortedValues)-rank)], 
                       sieve = sieve, type="max")
}

