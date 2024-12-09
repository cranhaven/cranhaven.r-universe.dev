#'
#' Fast version of mbu. It runs mbu without input checks.
#'
#' @param Y matrix with dichotomous responses
#' @param W matrix with weights for each entrance of Y or vector with weights for each row of Y
#' @param XU in unsupervised analysis starting values for row coordinates; in supervised analysis matrix with predictor variables for rows
#' @param BU for supervised analysis matrix with regression weights for the row coordinates
#' @param XV in unsupervised analysis starting values for column coordinates; in supervised analysis matrix with predictor variables for columns
#' @param BV for supervised analysis matrix with regression weights for the column coordinates
#' @param mains whether offsets for the items should be estimated
#' @param MAXINNER maximum number of iterations in the inner loop
#' @param FCRIT convergence criterion for STRESS in the inner loop
#' @param MAXITER maximum number of iterations in the outer loop
#' @param DCRIT convergence criterion for the deviance
#'
#' @return U estimated coordinate matrix for row objects
#' @return BU for supervised analysis the estimated matrix with regression weights for the rows
#' @return V estimated coordinate matrix for column objects
#' @return BV for supervised analysis the estimated matrix with regression weights for the columns
#' @return Mu estimated offsets
#' @return Lastinner number of iterations in the last call to STRESS
#' @return Lastfdif last difference in STRESS values in the inner loop
#' @return lastouter  number of iterations in the outer loop
#' @return lastddif last difference in deviances in outer loop
#' @return deviance obtained deviance
#'
#' @export
#' @useDynLib lmap, .registration=TRUE

fastmbu <- function( Y = NULL, W = NULL, XU = NULL, BU = NULL, XV = NULL, BV = NULL, mains = TRUE, MAXINNER = 32, FCRIT = 0.001, MAXITER = 65536, DCRIT = 0.000001 )
{
  # initialization
  n <- nrow( Y )
  r <- ncol( Y )
  if ( is.vector( W ) ) W <- matrix( W, n, r )
  if ( !is.null( BU ) ) {
    pu <- ncol( XU )
    m <- ncol( BU )
  }
  else m <- ncol( XU )
  if ( !is.null( BV ) ) {
    pv <- ncol( XV )
    m <- ncol( BV )
  }
  else m <- ncol( XV )
  mu <- rep( 0, r )
  iters <- 0
  deviance <- 0.0

  # execution
  if ( is.null( W ) ) {
    if ( is.null( BU ) ) {
      if ( is.null( BV ) ) res <- ( .C( "Cmulvarbinmduneg", 
                                        n=as.integer(n), 
                                        r=as.integer(r), 
                                        Y=as.double(Y), 
                                        m=as.integer(m), 
                                        XU=as.double(XU), 
                                        XV=as.double(XV), 
                                        mains=as.integer(mains), 
                                        mu=as.double(mu), 
                                        MAXINNER=as.integer(MAXINNER), 
                                        FCRIT=as.double(FCRIT), 
                                        MAXITER=as.integer(MAXITER), 
                                        DCRIT=as.double(DCRIT), 
                                        deviance=as.double(deviance), PACKAGE = "lmap" ) )
      else res <- ( .C( "Cmulvarbincolresmduneg", 
                        n=as.integer(n), 
                        r=as.integer(r), 
                        Y=as.double(Y), 
                        m=as.integer(m), 
                        XU=as.double(XU), 
                        pv=as.integer(pv), 
                        XV=as.double(XV), 
                        BV=as.double(BV), 
                        mains=as.integer(mains), 
                        mu=as.double(mu), 
                        MAXINNER=as.integer(MAXINNER), 
                        FCRIT=as.double(FCRIT), 
                        MAXITER=as.integer(MAXITER), 
                        DCRIT=as.double(DCRIT), 
                        deviance=as.double(deviance), PACKAGE = "lmap" ) )								
    }
    else {
      if ( is.null( BV ) ) res <- ( .C( "Cmulvarbinrowresmduneg", 
                                        n=as.integer(n), 
                                        r=as.integer(r), 
                                        Y=as.double(Y), 
                                        pu=as.integer(pu), 
                                        XU=as.double(XU), 
                                        m=as.integer(m), 
                                        BU=as.double(BU), 
                                        XV=as.double(XV), 
                                        mains=as.integer(mains), 
                                        mu=as.double(mu), 
                                        MAXINNER=as.integer(MAXINNER), 
                                        FCRIT=as.double(FCRIT), 
                                        MAXITER=as.integer(MAXITER), 
                                        DCRIT=as.double(DCRIT), 
                                        deviance=as.double(deviance), PACKAGE = "lmap" ) )
      else res <- ( .C( "Cmulvarbinresmduneg", 
                        n=as.integer(n), 
                        r=as.integer(r), 
                        Y=as.double(Y), 
                        m=as.integer(m), 
                        pu=as.integer(pu), 
                        XU=as.double(XU), 
                        BU=as.double(BU), 
                        pv=as.integer(pv), 
                        XV=as.double(XV), 
                        BV=as.double(BV), 
                        mains=as.integer(mains), 
                        mu=as.double(mu), 
                        MAXINNER=as.integer(MAXINNER), 
                        FCRIT=as.double(FCRIT), 
                        MAXITER=as.integer(MAXITER), 
                        DCRIT=as.double(DCRIT), 
                        deviance=as.double(deviance), PACKAGE = "lmap" ) )    }
  }
  else res <- ( .C( "Cmulvarbinwgtmduneg", 
                    n=as.integer(n), 
                    r=as.integer(r), 
                    Y=as.double(Y), 
                    W=as.double(W), 
                    m=as.integer(m), 
                    XU=as.double(XU), 
                    XV=as.double(XV), 
                    mains=as.integer(mains), 
                    mu=as.double(mu), 
                    MAXINNER=as.integer(MAXINNER), 
                    FCRIT=as.double(FCRIT), 
                    MAXITER=as.integer(MAXITER), 
                    DCRIT=as.double(DCRIT), 
                    deviance=as.double(deviance), PACKAGE = "lmap" ) )
  
  # finalization
  if ( is.null( BU ) ) {
    U <- matrix( res$XU, n, m )
  }
  else {
    BU <- matrix( res$BU, pu, m )
    U <- XU %*% BU
  }
  if ( is.null( BV ) ) {
    V <- matrix( res$XV, r, m )
  }
  else {
    BV <- matrix( res$BV, pv, m )
    V <- XV %*% BV
  }

  return( list( U=U, BU=BU, V=V, BV=BV, mu=res$mu, lastiter=res$MAXITER, lastddif=res$DCRIT, deviance=res$deviance ) )

} # fastmbu
