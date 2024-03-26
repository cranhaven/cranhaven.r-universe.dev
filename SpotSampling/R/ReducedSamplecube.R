#' Internal function of ReducedSamplecube
#' @noRd
onestep <- function(B,pik,EPS){

  kern <- MASS::Null(B)
  if(length(kern) == 0){
    return(NULL)
  }else{
    N    <- length(pik)
    u    <- kern[,1]

    l1  <- min(pmax((1-pik)/u,-pik/u))
    l2  <- min(pmax((pik-1)/u,pik/u))

    if(stats::runif(1) < l2/(l1+l2)){
      l <- l1;
    }else{
      l <- -l2;
    }
    pik <- pik + l*u

    return(pik)
  }
}


#' @title Cube method with reduction of the auxiliary variables matrix
#'
#' @description Modified cube method.
#' This function reduces considerably the execution time when the matrix of auxiliary variables \code{X} contains lot of 0s.
#' It is based on the function \code{\link[sampling:samplecube]{samplecube}} from the package \code{sampling}.
#'
#'
#' @param X a matrix of size (N x p) of auxiliary variables on which the sample must be balanced.
#' @param pik a vector of size N of inclusion probabilities.
#' @param redux a boolean value that specify if matrix \code{X} is reduced during the cube method. Default value is TRUE.
#' @param t the maximum number of constraints that can potentially be removed during the landing phase.
#'
#'
#' @details In case where the number of auxiliary variables is great (i.e. p very large), even if we use the fast implementation proposed by
#' (Chauvet and Tille 2005), the problem is time consuming.
#' This function reduces considerably the execution time when the matrix of auxiliary variables \code{X} contains lot of 0s.
#' It considers a reduced matrix \code{X} by removing columns and rows that sum to 0 (see  \code{\link{ReducedMatrix}}).
#'
#' Moreover, the landing by variable suppression is used.
#' \code{t} specifies the maximum number of constraints that can potentially be removed during the landing phase. This means that the first (N-T) constraints in \code{X} can be exactly satisfied.
#'
#'
#' @return the updated vector of \code{pik} that contains only 0s and 1s that indicates if a unit is selected or not at each wave.
#'
#'
#' @author Esther Eustache \email{esther.eustache@@unine.ch}, Raphael Jauslin \email{raphael.jauslin@@unine.ch}
#'
#'
#' @references
#' Chauvet, G. and Tille, Y. (2006). A fast algorithm of balanced sampling. Computational Statistics, 21/1:53-62
#'
#'
#' @seealso \code{\link[sampling:samplecube]{samplecube}}, \code{\link[sampling:landingcube]{landingcube}}, \code{\link{ReducedMatrix}}.
#'
#'
#' @examples
#' set.seed(1)
#' ## Matrix of 8 auxilary variables and 10 units with lot of 0s ##
#' X <- matrix(c(0.6,0.0,0.0,0.0,
#'               0.1,0.0,0.1,0.0,
#'               0.3,0.0,0.0,0.3,
#'               0.0,0.3,0.0,0.3,
#'               0.0,0.6,0.0,0.0,
#'               0.0,0.1,0.1,0.0), ncol = 4, byrow = TRUE)
#'
#' ## Inclusion probabilities with 10 units ##
#' pik <- c(0.60,0.10,0.30,0.30,0.60,0.10)
#'
#' ## parameter t ##
#' t <- 2
#'
#' ## Cube method ##
#' s   <- ReducedSamplecube(X, pik, redux = TRUE, t)
#' s
#'
#' @export
ReducedSamplecube <- function(X, pik, redux = TRUE, t){

  ##----------------------------------------------------------------
  ##                        Initialization                         -
  ##----------------------------------------------------------------

  EPS <- 1e-8
  A   <- X/pik
  J   <- ncol(X)

  ##----------------------------------------------------------------
  ##                Number of non 0-1 inclusion prob               -
  ##----------------------------------------------------------------

  i      <- which(pik > EPS & pik < (1-EPS))
  i_size <- length(i)
  if(i_size >= (J+1)){
    i <- i[1:(J+1)]
    B <- A[i,]
  }else{
    B <- A[i,]
  }



  ##---------------------------------------------------------------
  ##                          Main loop                           -
  ##---------------------------------------------------------------

  while(i_size > 0){

    ##  if redux is desired
    if(redux == TRUE){
      pik_tmp <- pik[i]
      tmp     <- ReducedMatrix(B)

      B_tmp   <- tmp$BB
      res     <- onestep(B_tmp, pik_tmp[tmp$ind_row],EPS)
      if(is.null(res)){
        break
      }else{
        pik_tmp[tmp$ind_row] <- res
      }
      pik[i]  <- pik_tmp

    }else{
      pik[i]  <- onestep(B,pik[i],EPS)
    }

    ##  update i
    i      <- which(pik > EPS & pik < (1-EPS))
    i_size <- length(i)

    ##  Depending if we have enough rows
    if(i_size >= (J+1)){
      i <- i[1:(J+1)]
      B <- A[i,]
    }else if(i_size > t){
      B <- A[i,]
    }else{

      B <- A[i,]

      if(i_size > EPS){
        kern <- MASS::Null(B)
        if(length(kern) == 0){
          break
        }
      }
    }
  }


  ##---------------------------------------------------------------
  ##                        Landing phase                         -
  ##---------------------------------------------------------------
  TEST <- ((pik>EPS)&(pik<1-EPS))
  m    <- 0
  if(any(TEST)){
    pikR <- pik[TEST]
    AA   <- ReducedMatrix(A[TEST,])$BB
    while((any(TEST)) && (m < t)){
      AA   <- AA[,1:(ncol(AA)-1)]
      res2 <- onestep(AA, pikR, EPS)
      if(!is.null(res2)){
        pik[TEST] <- res2
        TEST      <- ((pik>EPS)&(pik<1-EPS))

        if(any(TEST)){
          TEST2     <- ((res2>EPS)&(res2<1-EPS))
          pikR      <- res2[TEST2]
          AA        <- AA[TEST2,]
        }
      }
      m    <- m+1
    }
  }
  return(pik)
}


