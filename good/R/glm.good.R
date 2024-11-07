#' Maximum Likelihood Estimation and Good Regression
#'
#' @description \code{glm.good} is used to fit generalized linear models with a response variable following a
#' Good distribution with parameters z and s. \code{glm.good} allows incorporating predictors in
#' the model with a link function (log, logit and identity) that relates parameter z and
#' predictors. A summary method over an object of class \code{glm.good} provides essential
#' information regarding the fitted model such as parameters estimates, standard errors,
#' and some goodness-of-fit measures. A prediction method over an object of class \code{glm.good}
#' provides the fitted values with the estimated model and optionally standard errors and predictions
#' for a new data set.
#'
#' @usage glm.good ( formula , data , link = "log" , start = NULL )
#'
#' @param formula symbolic description of the model to be fitted. A typical predictor
#' has the form response ~ terms where the response is the integer-valued response
#' vector following a Good distribution with parameters s and z, and terms is a series of
#' predictors.
#' @param data an optional data frame with the variables in the model.
#' @param link character specification of link function: "logit", "log" or "identity".
#' By default link="log".
#' @param start a vector with the starting values for the model parameters. Used for numerically maximize
#' the likelihood function for parameters estimation. By default start = NULL.
#'
#' @return \code{glm.good} returns an object of class \code{glm.good} that is a list including:
#' \item{coefs}{ The vector of coefficients.}
#' \item{loglik}{ Log-likelihood of the fitted model.}
#' \item{vcov}{ Variance-covariance matrix of all model parameters
#' (derived from the Hessian matrix returned by nlm() ).}
#' \item{hess}{ Hessian matrix, returned by nlm().}
#' \item{fitted.values}{ The fitted mean values. These are obtained by
#' transforming the linear predictors by the link function inverse. }
#'
#' @seealso
#' See also \code{\link[copula]{polylog}} from \pkg{copula}, \code{\link[good]{dgood}},
#' and \code{\link[good]{pgood}}, \code{\link[good]{qgood}} and \code{\link[good]{rgood}}
#' from \pkg{good}, and \code{\link[maxLik]{maxLik}} from \pkg{maxLik}.
#'
#'
#' @author
#' Jordi Tur, David Moriña, Pere Puig, Alejandra Cabaña, Argimiro Arratia,
#' Amanda Fernández-Fontelo
#'
#' @references
#' Good, J. (1953). The  population  frequencies  of  species  and  the  estimation  of  population
#' parameters. Biometrika, 40: 237–264.
#'
#' Zörnig, P. and Altmann, G. (1995). Unified representation of zipf distributions.
#' Computational Statistics & Data Analysis, 19: 461–473.
#'
#' Kulasekera, K.B. and Tonkyn, D. (1992). A new distribution with applications to survival
#' dispersal anddispersion. Communication in Statistics - Simulation and Computation,
#' 21: 499–518.
#'
#' Doray, L.G. and Luong, A. (1997). Efficient estimators for the good family.
#' Communications in Statistics - Simulation and Computation, 26: 1075–1088.
#'
#' Johnson, N.L., Kemp, A.W. and Kotz, S. Univariate Discrete Distributions.
#' Wiley, Hoboken, 2005.
#'
#' Kemp. A.W. (2010). Families of power series distributions, with particular
#' reference to the lerch family. Journal of Statistical Planning and Inference,
#' 140:2255–2259.
#'
#' Wood, D.C. (1992). The Computation of Polylogarithms. Technical report. UKC,
#' University of Kent, Canterbury, UK (KAR id:21052).
#'
#' @importFrom stats model.matrix model.response model.offset is.empty.model make.link nlm
#' @importFrom maxLik maxLik
#'
#' @examples
#' strikes <- c ( rep ( 0, 46 ) , rep ( 1, 76 ) , rep ( 2, 24 ) , rep ( 3, 9 ) , rep ( 4, 1 )  )
#' mle <- glm.good ( strikes ~ 1 , link = "log" )
#' names ( mle )
#' mle$coefficients
#' mle$fitted.values
#' mean ( strikes )
#' summary ( mle )
#' predict ( mle , newdata = NULL , se.fit = TRUE )
#' @export

glm.good <- function ( formula , data , link = "log" , start = NULL ) {
  Call <- match.call ( )
  if( !link %in% c ( "log" , "logit" , "identity" ) ) {
    stop ( gettextf ( "Error in make.link(link) : '%s' link not recognised" , link ) , domain = NA ) }
  if ( missing ( data ) ) {
    data <- environment( formula ) }
  mf  <- match.call ( expand.dots = FALSE )
  mf2 <- match ( c ( "formula" , "data" , "offset" ) , names ( mf ) , 0L )
  mf  <- mf [ c ( 1L , mf2 ) ]
  mf$drop.unused.levels <- TRUE
  mf[[ 1L ]] <- quote ( stats::model.frame )
  mf <- eval ( mf , parent.frame ( ) )
  mt <- attr ( mf , "terms" )
  X <- model.matrix ( mt , mf )
  if ( !is.null ( start ) && ( dim ( X ) [ 2 ] + 1 ) != length ( start ) ) {
    stop ( "start vector dimension must equate the number of parameters to estimate" ) }
  Y <- model.response ( mf , "numeric" )
  if ( length ( dim ( Y ) ) == 1L ) {
    nm <- rownames ( Y )
    dim ( Y ) <- NULL
    if ( !is.null ( nm ) ) {
      names ( Y ) <- nm } }

  X <- if ( !is.empty.model ( mt ) ) model.matrix ( mt , mf )
  Xnames <- dimnames ( X )[[ 2L ]]
  kx <- NCOL ( X )
  rx <- nrow ( X )
  intercept  <- ifelse ( all ( X [ , 1 ] == 1) , TRUE , FALSE )
  linkstr <- link
  linkobj <- make.link ( linkstr )
  linkinv <- linkobj$linkinv

  loglik <- function ( par ) {
    s <- par [ 1 ]
    b <- par [ -1 ]
    z <- linkinv ( as.numeric ( X %*% b ) )
    aux <-  log ( dgood ( Y , z , s ) )
    return ( sum ( aux ) ) }

  if ( link %in% c ( "log" , "identity" ) && is.null ( start ) && !intercept ) {
    stop ( "if no intercept is considerd, please use link logit or provide feasible starting values" ) }

  if ( link %in% c ( "log" , "identity" ) ) {
    if ( link == "log" ) {
      A <- cbind ( matrix ( rep ( 0 , rx ) , ncol = 1 ) , - X )
      B <- rep ( 0 , rx )
      if ( is.null ( start ) ) {
        start <- c ( -2 , log ( 0.5 ) , rep ( 0 , kx - 1 ) ) }
      } else if ( link == "identity" ) {
        A <- rbind( cbind ( matrix ( rep ( 0 , rx ) , ncol = 1 ) , - X ) ,
                    cbind ( matrix ( rep ( 0 , rx ) , ncol = 1 ) ,   X ) )
        B <- c ( rep ( 1 , rx ), rep ( 0 , rx ) )
        if ( is.null ( start ) ) {
          start <- c ( - 2 , 0.5 , rep ( 0 , kx - 1 ) ) } }
    if ( any ( A %*% start + B < 0 ) ) {
      stop ( "initial value not feasible" ) }
    constraints <- list ( ineqA = A , ineqB = B )
    fit <- suppressWarnings ( maxLik ( logLik = loglik , start = start , constraints = constraints , iterlim = 10^3 ) )
    } else {
      if ( is.null ( start ) ) {
        start <- c ( -2 , rep ( 0, kx ) ) }
      fit <- suppressWarnings ( nlm ( f = function ( par ) return ( - loglik ( par ) ) , p = start , hessian = TRUE ) ) }

  coeff <- fit$estimate
  names ( coeff ) <- c ( "s" , Xnames )
  q  <- as.vector ( linkinv ( X %*% coeff [ -1 ] ) )
  nu <- coeff [ 1 ]
  mu <- goodmean( q , nu )
  if ( link == "logit" ) {
    hess <- fit$hessian
  } else {
      hess <- - fit$hessian }

  vcov <- try( solve ( hess ) , silent = T )
  if ( any ( class( vcov ) == "try-error" ) ){
    start <- round(start, 2)
    stop(gettextf ( "algorithm did not converge with initial values = c(%s); use a different set",
                    paste ( round(start, 2), collapse = ",") ) )
  }else if ( any ( diag( vcov ) < 0 ) ) {
    start <- round(start, 2)
    stop(gettextf ( "algorithm did not converge with initial values = c(%s); use a different set",
                    paste ( round(start, 2), collapse = ",") ) )
  }

  output <- list ( )
  output$coefficients <- coeff
  output$loglik <- ifelse ( link == "logit" , -fit$minimum , fit$maximum )
  output$vcov <- vcov
  output$hess <- hess
  output$fitted.values <- mu
  class ( output ) <- "glm.good"
  attr ( output , "Call" ) <- Call
  attr ( output , "link" ) <- link
  attr ( output , "x" ) <- X
  attr ( output , "y" ) <- Y
  return ( output ) }
