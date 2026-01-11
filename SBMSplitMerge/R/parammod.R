#' @title Parameter Model
#' @description create a \code{parammod} object
#' @details
#' A parameter model is a probability model for a \code{params} object.
#' This class creates a closure with five functions:
#' - a random method for sampling a \code{params} object
#' - a log-density method for computing the log-density of a given \code{params} object
#' - a transformation function \code{t} that maps a parameter value to the real line
#' - the inverse of t
#' - the log-gradient of t
#' @param logd \code{function(params)} - log-density function for parameters
#' @param r \code{function(kappa)} - random function to draw parameters
#' @param t mapping parameter space to real line
#' @param invt mapping real line to parameter space
#' @param loggradt log of the gradient of mapping \code{t}
#' @param ... additional arguments to store in the \code{parammod} object
#' @return a \code{parammod} object
#' @seealso \code{\link{param_beta}} \code{\link{param_gamma}} \code{\link{param_nbin}} \code{\link{param_norm}}
#' @export
parammod <- function(logd, r, t, invt, loggradt, ...){
    structure(
        list(
            logd=logd
           ,
            r=r
           ,
            t = t
           ,
            invt = invt
           ,
            loggradt = loggradt
           ,
            ...
        )
       ,
        class = "parammod"
    )
}

#' @export
print.parammod <- function(x, ...){
    cat("A parammod object with parameters:\n")
    print(x[-(1:5)])
}

#' @importFrom stats dbeta rbeta
#' @title Beta parameter model
#' @description A \code{parammod} with beta-distributed parameters
#' @details This model represents a prior on theta with:
#' \deqn{theta_0 ~ Beta(a0,a1)}
#' \deqn{theta_k ~ Beta(b0,b1)} for k = 1 ... kappa
#' @param a0 \code{theta_0 ~ Beta(a0,a1)}
#' @param a1 \code{theta_0 ~ Beta(a0,a1)}
#' @param b0 \code{theta_k ~ Beta(b0,b1)}
#' @param b1 \code{theta_k ~ Beta(b0,b1)}
#' @return a \code{parammod}
#' @examples
#' ## theta0 ~ Beta(1,9); thetak ~ Beta(9,1)
#' pb <- param_beta(1,9,9,1)
#' pb$r(5) ## a draw with 5 within-block parameters
#' @export
param_beta <- function(a0, a1, b0, b1){
    parammod(
        function(params){
            stats::dbeta(params$theta0, a0, a1, log=TRUE) +
                sum(stats::dbeta(params$thetak, b0, b1, log=TRUE))
        }
       ,
        function(kappa){
            params(stats::rbeta(1, a0, a1), stats::rbeta(kappa, b0, b1))
        }
       ,
        function(x){ log(x) - log(1-x)}
       ,
        function(x){ 1/(1 + exp(-x))}
       ,
        function(x){ -log(x)-log(1-x)}
       ,
        name="beta", a0=a0, a1=a1, b0=b0, b1=b1
    )
}

#' @importFrom stats rgamma dgamma
#' @title Gamma parameter model
#' @description A \code{parammod} with gamma-distributed parameters
#' @details This model represents a prior on theta with:
#' \deqn{theta_0 ~ Gamma(a0,a1)}
#' \deqn{theta_k ~ Gamma(b0,b1)} for k = 1 ... kappa
#' @param a0 \code{theta_0 ~ Gamma(a0,a1)}
#' @param a1 \code{theta_0 ~ Gamma(a0,a1)}
#' @param b0 \code{theta_k ~ Gamma(b0,b1)}
#' @param b1 \code{theta_k ~ Gamma(b0,b1)}
#' @return a \code{parammod}
#' @examples
#' ## theta0 ~ Gamma(1,1); thetak ~ Gamma(5,5)
#' pg <- param_gamma(1,1,5,5)
#' pg$r(5) ## a draw with 5 within-block parameters
#' @export
param_gamma <- function(a0, a1, b0, b1){
    parammod(
        function(params){
            stats::dgamma(params$theta0, a0, a1, log=log) +
                sum(stats::dgamma(params$thetak, b0, b1, log=log))
        }
       ,
        function(kappa){
            params(stats::rgamma(1, a0, a1), stats::rgamma(kappa, b0, b1))
        }
       ,
        function(x){ log(x)}
       ,
        function(x){ exp(x)}
       ,
        function(x){ -log(x)}
       ,
        name="gamma", a0=a0, a1=a1, b0=b0, b1=b1
    )
}

#' @title Parameter model for Normal Model
#' @description Normal parameter model:
#' \code{theta_0 = (mu0, sigma0)}
#' \code{theta_k = (muk, sigmak)}
#' @param a0,a1 \code{mu0 ~ Normal(a0,a1)}
#' @param b0,b1 \code{sig_0 ~ Gamma(b0,b1)}
#' @param c0,c1 \code{muk ~ Normal(c0, c1)}
#' @param d0,d1 \code{sig_k ~ Gamma(d0,d1)}
#' @return \code{parammod} representing Normal distributed parameters
#' @importFrom stats rgamma dgamma rnorm dnorm
#' @examples
#' ## theta0 = (mu0, sigma0); mu0~Normal(0,5); sigma0 ~ Gamma(1,1);
#' ## thetak = (muk, sigmak); muk~Normal(0,3); sigmak ~ Gamma(5,2);
#' pn <- param_norm(0,5,1,1,0,3,5,2)
#' pn$r(5) ## a draw with 5 within-block parameters
#' @export
param_norm <- function(a0, a1, b0, b1, c0, c1, d0, d1){
    parammod(
        function(params){
            stats::dnorm(params$theta0[1], a0, a1, log=TRUE) +
                stats::dgamma(params$theta0[2], b0, b1, log=TRUE) +
                    sum(stats::dnorm(params$thetak[,1], c0, c1, log=TRUE)) +
                    sum(stats::dgamma(params$thetak[,2], d0, d1, log=TRUE))
        }
       ,
        function(kappa){
            params(
                c(stats::rnorm(1, a0, a1), stats::rgamma(1, b0, b1))
               ,
                cbind(stats::rnorm(kappa, c0, c1), stats::rgamma(kappa, d0, d1))
            )
        }
       ,
        function(x){ cbind(x[1], log(x[2]))}
       ,
        function(x){ cbind(x[1], exp(x[2]))}
       ,
        function(x){ -log(x[2])}
       ,
        name="normal", a0=a0, a1=a1, b0=b0, b1=b1, c0=c0, c1=c1, d0=d0, d1=d1
    )
}

#' @title Parameter model for Negative Binomial
#' @description Negative Binomial parameter model:
#' \code{theta_0 = (mu0, sigma0)}
#' \code{theta_k = (muk, sigmak)}
#' @param a0,a1 \code{mu0 ~ Gamma(a0,a1)}
#' @param b0,b1 \code{sig_0 ~ Beta(b0,b1)}
#' @param c0,c1 \code{muk ~ Gamma(c0, c1)}
#' @param d0,d1 \code{sig_k ~ Beta(d0,d1)}
#' @return \code{parammod} representing Negative-Binomial distributed parameters
#' @importFrom stats rgamma dgamma rbeta dbeta
#' @examples
#' ## theta0 = (r0, p0); r0~Gamma(1,1); p0 ~ Beta(1,1);
#' ## thetak = (rk, pk); rk~Gamma(3,3); pk ~ Beta(5,5);
#' pn <- param_nbin(1,1,1,1,3,3,5,5)
#' pn$r(5) ## a draw with 5 within-block parameters
#' @export
param_nbin <- function(a0, a1, b0, b1, c0, c1, d0, d1){
    parammod(
        function(params){
            stats::dgamma(params$theta0[1], a0, a1, log=TRUE) +
                stats::dbeta(params$theta0[2], b0, b1, log=TRUE) +
                    sum(stats::dgamma(params$thetak[,1], c0, c1, log=TRUE)) +
                    sum(stats::dbeta(params$thetak[,2], d0, d1, log=TRUE))
        }
       ,
        function(kappa){
            params(
                c(stats::rgamma(1, a0, a1), stats::rbeta(1, b0, b1))
               ,
                cbind(stats::rgamma(kappa, c0, c1), stats::rbeta(kappa, d0, d1))
            )
        }
       ,
        function(x){ cbind(log(x[1]), log(x[2])-log(1-x[2]))}
       ,
        function(x){ cbind(exp(x[1]), 1/(1+exp(-x[2])))}
       ,
        function(x){ -log(x[1])-log(x[2])-log(1-x[2])}
       ,
        name="negative_binomial", a0=a0, a1=a1, b0=b0, b1=b1, c0=c0, c1=c1, d0=d0, d1=d1
    )
}
