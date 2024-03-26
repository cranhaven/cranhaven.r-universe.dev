#' @title Matrix reduction
#'
#' @description
#' Reduce a matrix by removing alternatively columns and rows that sum to 0. If the matrix is dense or if every columns sum to more than 0, then nothing is changed.
#'
#'
#' @param B a matrix that contains lot of 0s.
#'
#'
#' @return Returns a list including:
#' @return \code{BB} the reduced matrix of \code{B}.
#' @return \code{ind_col} a vector that contains the index of the remaining columns of \code{B} in \code{BB}.
#' @return \code{ind_row} a vector that contains the index of the remaining rows of \code{B} in \code{BB}.
#'
#'
#' @author Raphael Jauslin \email{raphael.jauslin@@unine.ch}
#'
#'
#' @examples
#' set.seed(1)
#' B  <- matrix(sample(c(0,0,0,1),80,replace=TRUE), nrow = 8, ncol =  10)
#' ReducedMatrix(B)
#'
#' @export
ReducedMatrix <- function(B){

  ##----------------------------------------------------------------
  ##                        Initialization                         -
  ##----------------------------------------------------------------

  EPS      <- 1e-8
  sums_col <- colSums(B)
  sums_row <- rowSums(B)
  BB       <- B
  ind_col  <- (1:ncol(B))
  ind_row  <- (1:nrow(B))


  ##---------------------------------------------------------------
  ##                          Main loop                           -
  ##---------------------------------------------------------------

  while(any(abs(sums_col) < EPS)){

    ## extract columns with sum larger than 0
    col <- which(abs(sums_col) > EPS)
    if(length(col) <= 1){ break }
    BB      <- BB[,col]
    ind_col <- ind_col[col]

    ## extract rows with sum larger than 0
    sums_row <- rowSums(BB)
    row      <- which(abs(sums_row) > EPS)
    if(length(row) <= 1){ break }
    BB       <- BB[row,]
    ind_row  <- ind_row[row]

    # if we have not enough row then compress B
    if(nrow(BB) > ncol(BB)){
      ind_row <- ind_row[1:(ncol(BB)+1)]
      BB      <- BB[1:(ncol(BB)+1),]
    }

    ## recompute
    sums_col <- colSums(BB)
  }


  return(list(BB = BB, ind_col = ind_col, ind_row = ind_row))
}
