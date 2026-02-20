#' Link Functions in the Class of Generalized Linear Models
#'
#' This function provides characteristics of common link functions (logit, probit, and comlementary log-log). Specifically, based on the link name, the function with its inverse, first derivative, and second derivative is provided. 
#'
#' @param fn.name One of the three: "logit", "probit", and "cloglog".
#'
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#'
#' @return A list with components:
#' \item{g}{The link function corresponding to "logit", "probit", or "cloglog".}
#' \item{dg}{The first derivative of g.}
#' \item{d2g}{The second derivative of g.}
#' \item{gInv}{The inverse of g.}
#' 
#' @export
#' 
#' @examples
#' 
#' library(groupTesting)
#' 
#' ## Try:
#' glmLink("logit")
#' 
glmLink <- function(fn.name=c("logit","probit","cloglog")){
  fn.name <- match.arg(fn.name)
  
  # Logit link
  if(fn.name == "logit"){
    g <- function(u){exp(u)/(1+exp(u))}
    dg <- function(u){exp(u)/(1+exp(u))^2}
    d2g <- function(u){exp(u)*(1-exp(u))/(1+exp(u))^3}
    g.inv <- function(u){log(u/(1-u))}
  }
  # Probit link
  if(fn.name == "probit"){
    g <- function(u){stats::pnorm(u)}
    dg <- function(u){exp(-u^2/2)/sqrt(2*pi)}
    d2g <- function(u){-u*exp(-u^2/2)/sqrt(2*pi)}
    g.inv <- function(u){stats::qnorm(u)}
  }
  # Complementary log-log link
  if(fn.name == "cloglog"){
    g <- function(u){1 - exp(-exp(u))}
    dg <- function(u){exp(u)*exp(-exp(u))}
    d2g <- function(u){exp(u)*exp(-exp(u))*(1-exp(u))}
    g.inv <- function(u){log(-log(1-u))}
  }
  list("g"   = g,
       "dg"  = dg,
       "d2g" = d2g,
       "gInv"= g.inv)
}
