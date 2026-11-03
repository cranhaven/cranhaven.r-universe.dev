#' This function finds the boundary of the identified set in one specified direction using the AS test and Newton's method.
#'
#' @param start the starting points for the bissection method
#' @param Yp the observations of the outcome variable.
#' @param Xb the observations of the noncommon regressor (possibly conditional on Xc).
#' @param N_max the maximal number of iterations. Default is 30.
#' @param tol the tolerance of the method. Default is e-4.
#' @param tuningParam the list of tuning parameters. For the details see the function "test" in the package RationalExp.
#'
#' @return a list containing, in order:
#'  -  the value of estimated radial function in this direction
#'  -  value of the objective function
#'  -  the number of iterations
#'

AS_bounds <- function( start, Yp ,Xb  , N_max = 30, tol = 10^(-4),tuningParam=NULL){

  Ybarre = mean(Yp);
  Xbarre = mean(Xb);
  YY = Yp -    Ybarre
  XX = Xb -    Xbarre
  out = vector("list")

  ## initialisation
  a1 =  start[1]
  b1 =  start[2]
  kmin =  AStest(a1,YY,XX,tuningParam)
  kmax =  AStest(b1,YY,XX,tuningParam)
  ind=0
  if( is.na(kmin) || is.na(kmax) ){
    out[[3]] <- NA
  }else if(kmax ==0 ){

    ## value of lambda
    out[[1]] <-  b1
    ## entropy value
    out[[2]] <-  kmax
    ## Skappa
    out[[3]] <-  0
  }else if(kmin ==1 ){

    ## value of lambda
    out[[1]] <- a1
    ## entropy value
    out[[2]] <- kmin
    ## Skappa
    out[[3]] <- 0
  }else{
    ## add condition if kmin !=1 et   kmax !=0
    ind = 1
    stop = 0
    # N_max=1000
    while( ind <= N_max && stop ==0){
      c <-  (a1+ b1)/2
      solve = AStest(c,YY,XX,tuningParam)
      if(   (b1-a1)/2 < tol   ){
        stop = 1
      }else{
        ind = ind+1
        if( solve == 0   ){
          a1 = c
        }else{
          b1 = c
        }
      }
    }

    ## value of lambda
    out[[1]] <- c
    ## entropy value
    out[[2]] <- solve
    ## Skappa
    out[[3]] <-ind

  }

  # out[[4]] <- ind
  return( out)
}

