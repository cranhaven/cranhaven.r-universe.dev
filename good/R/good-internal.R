#' Good Distribution mean statistic
#' @description Expected mean of the Good distribution
#' @usage goodmean ( z , s )
#'
#' @param z vector of first parameter for the Good distribution.
#' @param s vector of second parameter for the Good distribution.
#'
#' @details The expected Good distribution mean is definded as:
#' \deqn{E(X) = F(z,s-1) / F(z,s) - 1}
#' where parameters z and s should be respectively in  \eqn{ (0,1)} and the reals.
#' In addition, \eqn{F(z,s)} is the polylogarithm function defined as:
#' \deqn{F(z,s)=\sum_{i=1}^{\infty} z^n/n^s,}
#'
#' @return \code{goodmean} returns a vector with the expected mean for the Good distribution
#' with parameters z and s. If z is not within the interval \eqn{(0,1)}, \code{goodmean}
#' returns \eqn{NA}.
#'
#' @importFrom copula polylog
#' @keywords internal
#' @noRd

goodmean <- function ( z , s ) {
  aux <- function ( z , s ){
    apply ( expand.grid ( z , c ( s , s - 1 ) ) , 1 ,
            function ( j ) ifelse ( z < 1 && z > 0 , polylog ( z = j [ 1 ] , s = j [ 2 ] , n.sum = 10^3 ) , NA ) ) }
  polyLog <- apply ( expand.grid ( z , s ) , 1 , function ( j ) aux ( j [ 1 ] , j [ 2 ] ) )
  return ( ( polyLog [ 2 , ] / polyLog [ 1 , ] ) - 1 ) }

#' Predict method for \code{glm.good} model fit
#' @description Predicted values for a \code{glm.good} object.
#' @aliases predict predict.glm.good
#' @method predict glm.good
#' @usage
#' \method{predict}{glm.good}( object , newdata = NULL, se.fit = FALSE, ... )
#'
#' @param object object of class \code{glm.good}.
#' @param newdata An optional data frame in which to look for variables with which to predict.
#' If omitted, the fitted values are used.
#' @param se.fit logical, default FALSE. If TRUE, standard errors are provided.
#' @param ... additional arguments.
#'
#' @return \code{predict.glm.good} produces a vector of predictions. If standard errors are required,
#' these are computed with the multivariate Delta method. If se.fit is TRUE, a list with the
#' components below is returned:
#' \item{fit}{vector with the fitted mean values}
#' \item{se.fit}{vector with standard errors of predicted mean values}
#'
#' @importFrom copula polylog
#' @keywords internal
#' @export

predict.glm.good <- function ( object , newdata = NULL , se.fit = FALSE , ... ) {

  X <- attr ( object , "x" )
  fitv <- object$fitted.values
  coef <- object$coefficients
  link <- attr ( object , "link" )
  vcov <- object$vcov

  if ( !is.null ( newdata ) ) {
    if ( dim ( newdata ) [ 2 ] != dim ( X ) [ 2 ] ) {
      stop ( "dim ( newdata ) is not compatible with  dim ( X )" ) } }
  if ( ! is.logical ( se.fit ) ) {
    stop ( "se.fit should be logical; default FALSE" ) }

  gradient.glm.good <- function ( z , s , link = "log" , xreg = NULL ) {

    diffs <- function ( z , s , lim = 10^3 ) {
      aux <- function ( z , s , n ) {
        return ( ( - z^n * log ( n ) ) / n^s ) }
      return ( sum ( apply ( cbind ( 1 : lim ) , 1 , function ( j ) aux ( z , s , j ) ) ) ) }

    diffZ <- function ( z , s , lim = 10^3 ) {
      aux <- function ( z , s , n ) {
        return ( ( ( z^n ) / ( ( n^s ) * ( exp ( log ( z / ( 1 - z ) ) ) + 1 ) ) ) ) }
      return ( sum ( apply ( cbind ( 1 : lim ) , 1 , function ( j ) aux ( z , s , j ) ) ) ) }

    polyLog <- apply ( expand.grid ( z , c ( s , s - 1 , s - 2 ) ) , 1 ,
                       function ( j ) polylog ( z = j [ 1 ] , s = j [ 2 ] , n.sum = 10^3 ) )
    num   <- polyLog [ 2 ]
    den   <- polyLog [ 1 ]
    diffnums <- diffs ( z = z , s = ( s - 1 ) )
    diffdens <- diffs ( z = z , s = s )
    if ( link == "log" ) {
      diffnumz <- polyLog [ 3 ]
      diffdenz <- polyLog [ 2 ]
    } else if ( link == "logit" ) {
      diffnumz <- diffZ ( z = z , s = ( s - 2 ) )
      diffdenz <- diffZ ( z = z , s = ( s - 1 ) )
    } else {
      diffnumz <- polyLog [ 3 ] / z
      diffdenz <- polyLog [ 2 ] / z }
    if ( is.null ( xreg ) ) {
      xreg <- 1 }
    return ( c ( ( diffnums * den - num * diffdens ) / den^2 ,
                   apply ( cbind ( 1 : length ( xreg ) ) , 1 ,
                         function ( j ) ( diffnumz * xreg [ j ] * den - num * diffdenz * xreg [ j ] ) / den^2 ) ) ) }

  s <- coef [ 1 ]
  if ( !isTRUE ( se.fit ) ) {
    se.fit <- NULL }
  if ( is.null ( newdata ) ) {
    if ( isTRUE ( se.fit ) ) {
      if ( length ( coef ) < 3 ) {
        z <- do.call ( make.link ( link ) $ linkinv , list ( coef [ -1 ] ) )
        gr.glm <- gradient.glm.good ( z = z , s = s , link = link , xreg = NULL )
        se.fit <- rep ( sqrt ( t ( gr.glm ) %*% vcov %*% gr.glm ) , nrow ( X ) )
        } else {
          lr.com <-  as.matrix ( X ) %*% as.matrix ( coef [ -1 ] )
          z <- do.call ( make.link ( link ) $ linkinv , list ( lr.com ) )
          gr.glm <- apply ( cbind ( 1 : length ( z ) ) , 1 ,
                            function ( j ) gradient.glm.good ( z = z [ j ] , s = s ,
                                                               link = link , xreg = X [ j , ] ) )
          se.fit <- apply ( gr.glm , 2 , function ( j ) sqrt ( t ( j ) %*% vcov %*% j ) ) } }
    } else {
      lr.com <-  newdata %*% as.matrix ( coef [ -1 ] )
      z <- do.call ( make.link ( link ) $ linkinv , list ( lr.com ) )
      fitv <- apply ( expand.grid ( z , s ) , 1 , function ( j ) goodmean ( j [ 1 ] , j [ 2 ] ) )
      if ( isTRUE ( se.fit ) ) {
        gr.glm <- apply ( cbind ( 1 : length ( z ) ) , 1 ,
                          function ( j ) gradient.glm.good ( z = z [ j ] , s = s ,
                                                             link = link , xreg = newdata [ j , ] ) )
        se.fit <- apply ( gr.glm , 2 , function ( j ) sqrt ( t ( j ) %*% vcov %*% j ) ) } }

  result <- list ( fit = fitv ,
                   se.fit = se.fit )
  class ( result )  <- "predict.glm.good"
  return ( result ) }


#' \code{print.predict.glm.good}: utility functions for printing.
#' @description print method for an object of class \code{predict.glm.good}.
#' Users are not encouraged to call these internal functions directively.
#'
#' @param x object of class \code{predict.glm.good}.
#' @param ... additional arguments.
#'
#' @details Internal functions for package \code{good}.
#'
#' @keywords internal
#' @export
#' @noRd

print.predict.glm.good <- function  ( x , ... ) {
  cat ( "--\nfitted values:\n" )
  print ( x $ fit )
  if ( !is.null ( x $ se.fit ) ) {
    cat ( "--\nstandard errors:\n" )
    print ( x $ se.fit ) } }

#' Summary of \code{glm.good} model fit
#' @description summary method for an object of class \code{glm.good}.
#' @aliases summary summary.glm.good
#' @method summary glm.good
#' @usage
#' \method{summary}{glm.good}( object , ... )
#'
#' @param object object of class \code{glm.good}.
#' @param ... additional arguments.
#'
#' @return
#' \item{coefficients}{matrix of dimension p x 4 with columns given respectively the estimated coefficient,
#' standard errors, t-statistic and (two-sided) p-value.}
#' \item{loglik}{log-likelihood function of the fitted model.}
#' \item{aic}{Akaike Information Criterion value of the model.}
#' \item{bic}{Bayesian Information Criterion value of the model.}
#' \item{call}{unevaluated function call of the object}
#' \item{transformed}{point estimate and approximate standard error of \eqn{z=h^{-1}(z^*)} where \eqn{h^{-1}(\cdot)} is
#' the inverse of the link function and \eqn{z^{*}} is the corresponding estimated value given in coefficients.
#' The standard error is computed using the univariate Delta method. If the Good regression
#' has covariates, transformed returns a NULL.}
#' \item{testloglik}{log-likelihood function of the model without covariates (with intercept only).}
#' \item{LRT}{Likelihood Ratio Test statistic.}
#' \item{df}{degrees of freedom of the LRT.}
#' \item{pval}{p.value of the LRT.}
#' \item{testdist}{either Logarithmic or Geometric distributions tested against Good distribution with LRT.}
#'
#' @importFrom stats pnorm pchisq optim dgeom
#' @importFrom maxLik maxLik
#' @keywords internal
#' @export

summary.glm.good <- function  ( object , ... ) {
  call   <- attr ( object , "Call" )
  coef.p <- object$coefficients
  covmat <- object$vcov
  var.cf <- diag ( covmat )
  s.err  <- sqrt ( var.cf )
  tvalue <- coef.p / s.err
  dn <- c ( "Estimate" , "Std. Error" )
  pvalue <- 2 * pnorm ( - abs ( tvalue ) )
  coef.table <- cbind ( coef.p , s.err , tvalue , pvalue )
  dimnames ( coef.table ) <- list ( names ( coef.p ) , c ( dn , "z value" , "p-value" ) )
  resid <- attr ( object , "y" ) - object$fitted.values
  loglik <- object$loglik
  aic <- 2 * length ( coef.p ) - 2 * loglik
  bic <- length ( coef.p ) * log ( length ( resid ) ) - 2 * loglik
  transformed <- NULL

  linkstr <- attr ( object , "link" )
  linkobj <- make.link ( linkstr )
  linkinv <- linkobj$linkinv

  if ( length ( coef.p ) > 2 ) {
    logLik <- function ( par, Y ) {
      s <- par [ 1 ]
      z <- exp ( par [ 2 ] )
      res <- sum ( log ( dgood ( Y , z , s ) ) )
      return ( res ) }

    constraints <- list ( ineqA = matrix ( c ( 0 , -1 ) , nrow = 1 ) , ineqB = 0 )
    start <- c ( -2 , log ( 0.5 ) )
    fit <- maxLik ( logLik = logLik, start = start, Y = attr ( object, "y" ) ,
                    constraints = constraints, iterlim = 10^3 )
    testloglik <- fit$maximum
    LRT  <- - 2 * ( testloglik - object$loglik )
    df   <- length ( object$coefficients ) - 2
    pval <- pchisq ( LRT , df = df , lower.tail = FALSE )
    testdist <- "good"
  } else {

    if ( attr ( object , "link" ) == "log" ) {
      transformed <- matrix ( c ( 1 , s.err [ 2 ] ) * exp ( coef.p [ 2 ] ) , nrow = 1 )
    } else if ( attr ( object , "link" ) == "logit" ) {
      transformed <- matrix ( c ( 1 / ( 1 + exp ( - coef.p [ 2 ] ) ) , s.err [ 2 ] * exp ( - coef.p [ 2 ] ) / ( ( 1 + exp ( - coef.p [ 2 ] ) )^2 ) ) , nrow = 1 )
    } else {
      transformed <- matrix ( c ( coef.p [ 2 ] ,s.err [ 2 ]  ) , nrow = 1 )
    }
    dimnames ( transformed )  <- list ( "z", dn )

    testloglik <- NULL
    LRT <- NULL
    df <- NULL
    pval <- NULL
    testdist <- c ( "logarithmic" , "geometric")

      nlogLikL <- function ( z , Y ) {
        res <- sum ( ( Y + 1 ) * log ( z ) - log ( Y + 1 ) - log ( - log ( 1 - z ) ) )
        return ( - res ) }
      fitL <- optim ( par = 0.5,
                     fn = nlogLikL,
                     Y = attr ( object, "y" ) ,
                     method = "L-BFGS-B",
                     lower = 10^-5 , upper = ( 1 - 10^-5 ) )
      testloglik[1] <- - fitL$value
      LRT[1] <- - 2 * ( testloglik[1] - object$loglik )
      df[1] <- length ( object$coefficients ) - 1
      pval[1] <- pchisq ( LRT[1] , df = df[1] , lower.tail = FALSE )

      nlogLikG <- function ( z , Y ) {
        res <-  sum( log ( dgeom ( Y , z , log = FALSE ) ) )
        return ( - res ) }
      fitG <- optim ( par = 0.5 ,
                     fn = nlogLikG ,
                     Y = attr ( object , "y" ) ,
                     method = "L-BFGS-B" ,
                     lower = 10^-5 , upper = ( 1 - 10^-5 ) )
      testloglik[2] <- - fitG$value
      LRT[2] <- - 2 * ( testloglik[2]  - object$loglik )
      df[2] <- length ( object$coefficients ) - 1
      pval[2] <- pchisq ( LRT[2]  , df = df[2]  , lower.tail = FALSE )
    }

  ans <- list ( resid = resid ,
                coefficients = coef.table ,
                loglik = loglik ,
                aic = aic ,
                bic = bic ,
                call = call,
                transformed = transformed ,
                testloglik = testloglik ,
                LRT = LRT ,
                df = df ,
                pval = pval ,
                testdist = testdist )
  class ( ans )  <- "summary.glm.good"
  return ( ans ) }

#' \code{print.summary.glm.good}: utility functions for printing.
#' @description print method for an object of class \code{summary.glm.good}.
#' Users are not encouraged to call these internal functions directively.
#'
#' @param x object of class \code{summary.glm.good}.
#' @param ... additional arguments.
#'
#' @details Internal functions for package \code{good}.
#'
#' @importFrom stats quantile
#' @keywords internal
#' @export
#' @noRd

print.summary.glm.good <- function ( x, ... )
{
  cat ( "Call:\n" )
  print ( x$call )
  cat ( "--\nDeviance Residuals:\n" )
  resid <- quantile ( x$resid , c ( 0 , 0.25 , 0.5 , 0.75 , 1 )  )
  names ( resid )  <- c ( "Min" , "1Q" , "Median" , "3Q" , "Max" )
  print ( resid )
  cat ( "--\nCoefficients:\n" )
  print ( x$coefficients )
  if ( NROW ( x$coefficients ) < 3 )  {
    cat ( "--\nTransformed intercept-only parameter\n" )
    print ( x$transformed ) }
  if ( !is.null ( x$testdist ) ) {
    if ( x$testdist[1] == "good" ) {
      names <- all.vars ( x$call$formula )
      cat ( "--\nLikelihood Ratio Test:" )
      cat ( "\nModel 1:" , names [ 1 ] , "~ 1" )
      if ( length ( names ) > 2 ) {
        cat ( "\nModel 2:" , names [ 1 ] , "~" , names [ 2 ] , paste ( "+" , names [ -c ( 1 , 2 )  ]  ) , "\n" )
      } else  {
          cat ( "\nModel 2:" , names [ 1 ] , "~" , names [ 2 ] , "\n" ) }
      testtable <- data.frame ( c ( 1 , 2 ) , c ( round ( x$testloglik , 2 ) , round ( x$loglik , 2 )  ) ,
                              c ( "" , 1 ) , c ( "" , round ( x$LRT , 4 )  ) , c ( "" , format.pval ( x$pval )  )  )
      names ( testtable )  <- c ( paste0 ( "\U0023" , "Df" ) , "LogLik" , "Df" , "LRT" , "p.value" )
      print( testtable )
      } else if (!any(is.na(x$testloglik))) {
        testtableL <- data.frame ( c ( 1 , 2 ) , c ( round ( x$testloglik[1] , 2 ) , round ( x$loglik , 2 )  ) ,
                                  c ( "" , 1 ) , c ( "" , round ( x$LRT[1] , 4 )  ) , c ( "" , format.pval ( x$pval[1] )  )  )
        testtableG <- data.frame ( c ( 1 , 2 ) , c ( round ( x$testloglik[2] , 2 ) , round ( x$loglik , 2 )  ) ,
                                   c ( "" , 1 ) , c ( "" , round ( x$LRT[2] , 4 )  ) , c ( "" , format.pval ( x$pval[2] )  )  )
        names ( testtableL )  <- names ( testtableG )  <- c ( paste0 ( "\U0023","Df" ) , "LogLik", "Df", "LRT", "p.value" )
        cat ( "--\nLikelihood ratio test:" )
        cat ( "\nModel 1:" , x$testdist[1] , "(s=1)")
        cat ( "\nModel 2:" , "good \n" )
        print ( testtableL )
        cat ( "\nModel 1:" , x$testdist[2] , "(s=0)")
        cat ( "\nModel 2:" , "good \n" )
        print ( testtableG)
        }
    }
  cat ( "--\nLogLik:", round ( x$loglik, 2 ) , "   AIC:", round ( x$aic, 2 ) , "   BIC:", round ( x$bic, 2 ) , "\n" ) }

