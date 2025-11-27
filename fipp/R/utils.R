# A function to recursively compute the MFM coefficient V
#
# Intended for internal use by nClusters in static MFM
# setting.
#
mfmCoefsRecursive <- function(N,gamma,priorK,priorKparams,maxK){
  # log of V_{N-1,0}
  recInitNminus1 <- sapply(1:maxK,function(K){
    lgamma(gamma*K)-lgamma(gamma*K+N-1)+
      do.call(priorK,args = c(list(x = K-1),priorKparams,list(log = TRUE)))
  })
  # log of V_{N,0}
  recInitN <- sapply(1:maxK,function(idx) recInitNminus1[idx] - log(gamma*idx+N-1))
  # log of V_{N,1}
  recFirstN <- sapply(1:maxK,function(idx){
    # here we use ln(p1-p2) = ln(exp(ln(p1))-exp(ln(p2))) 
    # = ln(exp(ln(p1))(1-exp(ln(p2)-ln(p1)))) = ln(p1) + log(1-exp(ln(p2)-ln(p1)))
    # then we use the result from M"achler 2012 to choose between log1p(-exp(x)) or log(-expm1(x))
    # ln(V_N_0)+ln((N-1)/gamma + 0)-(ln(V_N-1_0)-ln(gamma)) = ln(V_N_0)-ln(V_N-1_0)+ln(N-1)
    val <- recInitN[idx]+log(N-1)-recInitNminus1[idx]
    recInitNminus1[idx]-log(gamma) +ifelse(-val>log(2),log1p(-exp(val)),log(-expm1(val)))
  })
  res <- list(recFirstN)
  # log of V_{N,k}
  recNk <- function(k){
    if(length(res) < k || is.null(res[[k]])){
        recNkminus1 <- recNk(k-1)
        idxval <- cbind(seq_along((k-1):maxK),(k-1):maxK)
        recNminus1kminus1 <- sapply(1:nrow(idxval),function(idx){
            recNkminus1[idxval[idx,1]]+log(gamma*idxval[idx,2]+N-1)
        })

        retval <- sapply(1:length(recNminus1kminus1),function(idx){
            ## log((N-1)/gamma+(k-1)) = log(N-1+gamma*(k-1))-log(gamma)
            ## we also have +log(gamma) so these two will cancel out
            val <- recNkminus1[idx]+log(N-1+gamma*(k-1))-recNminus1kminus1[idx]
            recNminus1kminus1[idx]-log(gamma) + ifelse(-val>log(2),log1p(-exp(val)),log(-expm1(val)))
        })[2:length(recNminus1kminus1)]
        length(res) <- max(k, length(res))
        res[[k]] <- retval
        p <- environment(recNk)
        p[["res"]] <- res
    }
    res[[k]]
  }
}

# A function to recursively compute the composition C_N with the Toepelitz trick
#
# Intended for internal use in functions: nClusters,Exlfunc and Excrosslfunc.
# 
condCompositions <- function(N, gammaK) {
  res <- list()
  cKk <- function(k){
    if(length(res) < k || is.null(res[[k]])){
        p <- environment(cKk)
        if(k == 1){
            initVec <- sapply(N:1, function(n) lgamma(n + gammaK) - lgamma(n + 1))
            res <- list(matrix(initVec, ncol = 1))
        } else {
            ws <- sapply(1:(N-k+1),function(n) lgamma(n+gammaK) - lgamma(n+1))
            length(res) <- max(k, length(res))
            p[["res"]] <- res
            res[[k]] <- logTopdot(ws, cKk(k-1))
        }
        p[["res"]] <- res
    }
    res[[k]]
  }
}

#' Probability density function of the BNB distribution
#' 
#' Evaluates the probability density function of the
#' beta-negative-binomial (BNB) distribution with a mean parameter and
#' two shape parameters.
#' 
#' @param x vector of quantiles.
#' @param mu mean parameter.
#' @param a 1st shape parameter.
#' @param b 2nd shape parameter.
#' @param log logical; if TRUE, density values p are given as log(p).
#'
#' @return Numeric vector of density values.
#'
#' @details
#'
#' The BNB distribution has density
#'
#' \deqn{
#'   f(x) = \frac{\Gamma(\mu + x) B(\mu + a, x + b)}{\Gamma(\mu) \Gamma(x + 1) B(a, b)},
#' }{
#'   f(x) = (\Gamma(\mu + x) B(\mu + a, x + b)) / (Gamma(\mu) Gamma(x + 1) B(a, b)),
#' }
#' where \eqn{\mu} is the mean parameter and \eqn{a} and \eqn{b} are the first and
#' second shape parameter.
#' 
#' @examples
#' ## Similar to other d+DISTRIBUTION_NAME functions such as dnorm, it
#' ## evaluates the density of a distribution (in this case the BNB distri)
#' ## at point x
#' ##
#' ## Let's try with the density of x = 1 for BNB(1,4,3)
#' x <- 1
#' dbnb(x, mu = 1, a = 4, b = 3)
#' 
#' ## The primary use of this function is in the closures returned from
#' ## fipp() or nCluststers() as a prior on K-1
#' pmf <- nClusters(Kplus = 1:15, N = 100, type = "static",
#' gamma = 1, maxK = 150)
#' 
#' ## Now evaluate above when K-1 ~ BNB(1,4,3)
#' pmf(priorK = dbnb, priorKparams = list(mu = 1, a = 4, b = 3))
#' 
#' ## Compare the result with the case when K-1 ~ Pois(1)
#' pmf(priorK = dpois, priorKparams = list(lambda = 1))
#' 
#' ## Although both BNB(1,4,3) and Pois(1) have 1 as their mean, the former
#' ## has a fatter rhs tail. We see that it is reflected in the induced prior 
#' ## on K+ as well
#' 
#' @references Frühwirth-Schnatter, S., Malsiner-Walli, G., and Grün, B. (2020) 
#'  Generalized mixtures of finite mixtures and telescoping sampling 
#'  \url{https://arxiv.org/abs/2005.09918}
#' 
#' @export
dbnb <- function(x, mu, a, b, log = FALSE) {
  val <- lgamma(mu + x) + lbeta(mu + a, x + b) - lgamma(mu) - lgamma(x + 1) - lbeta(a, b)
  if (log){
    return(val)
  }else{
    return(exp(val))
  }
}
