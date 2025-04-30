#'Expected Shortfall
#'
#' @description
#' \loadmathjax{}
#'  Computes the Expected Shortfall of a given amount of loss based on variance-covariance method.
#'
#' @param variance It could be either a scalar or a matrix containing the variances and covariances
#'  of the losses. Provide a covariance matrix when analyzing correlated losses or a scalar when
#'  dealing with a single loss.
#' @param alpha A numeric value (either a single one or a vector) consisting of the significance level
#'  at which ES has to be computed, it can either be a single numeric value or a vector of numeric values.
#' @param weights A vector of weights of size \emph{N} for weighting the variance of losses.
#'  When \code{weights=NULL} variances used to compute ES are the original values supplied to  \code{variance}
#'  with no weighting scheme.
#' @param model A character string indicating which distribution is to be used for computing the ES,
#'  the default value is the \code{normal} distribution, the other alternative is \code{t-student}
#'  distribution with \eqn{\upsilon}{\upsilon} degrees of freedom. When \code{model='both'} \code{'normal'}
#'  as well as \code{'t-student'} are used when computing the ES, see examples.
#' @param df An integer indicating the degrees of freedom for the t-student distribution when setting
#' \code{model='t-student'} and \code{model='both'}. \code{df} must be greater than 2.
#' @param percentage Logical indicating whether the file names in the VaR table should be presented in percentage or decimal.
#'
#' @details
#' \code{ES} computes the Expected Shortfall (ES) of a certaing amount of loss based upon the following general formulation:
#'
#' \mjtdeqn{ES_\alpha = \frac{1}{1-\alpha}\int_{\alpha}^{1} VaR_u(X)du = E[X|X > F_{X}^{-1}(\alpha)]}{ES_\alpha = \dfrac{1}{1-\alpha}\int_{\alpha}^{1} VaR_u(X)du = E[X|X > F_{X}^{-1}(\alpha)]}{}
#'
#'where \mjteqn{\alpha}{\alpha}{} is the significance level, \mjteqn{VaR_u(X)}{VaR_u(X)}{} is the Value-at-Risk of \mjteqn{X.}{X.}{}
#'
#' \code{ES} for the normal case is based on the following expression:
#'
#'\mjtdeqn{ES_{\alpha} = \mu + \sigma \frac{\phi(\Phi^{-1}(\alpha))}{1-\alpha}}{ES_{\alpha} = \mu + \sigma \frac{\phi(\Phi^{-1}(\alpha))}{1-\alpha}}{}
#'
#' Meanwhile, \code{ES} for the t-student distribution takes comes from:
#'
#'\mjtdeqn{ES_{\alpha}(\tilde{X}) = \frac{g_{\upsilon}(t_{\upsilon}^{-1}(\alpha))}{1-\alpha} \left( \frac{\upsilon+(t_{\upsilon}^{-1}(\alpha))^{2}}{\upsilon - 1} \right)}{ES_{\alpha}(\tilde{X}) = \frac{g_{\upsilon}(t_{\upsilon}^{-1}(\alpha))}{1-\alpha} \left( \frac{\upsilon+(t_{\upsilon}^{-1}(\alpha))^{2}}{\upsilon - 1} \right)}{}
#'
#' @references
#' Dhaene J., Tsanakas A., Valdez E. and Vanduffel S. (2011). \emph{Optimal Capital Allocation Principles}. The Journal of Risk and Insurance. Vol. 00, No. 0, 1-28.
#'
#' McNeil, A. J.; Frey, R. & Embrechts, P. \emph{Quantitative risk management: concepts, techniques and tools}. Princeton University Press, 2005.
#'
#' Urbina, J. (2013) \emph{Quantifying Optimal Capital Allocation Principles based on Risk Measures.} Master Thesis, Universitat Politècnica de Catalunya.
#'
#' Urbina, J. and Guillén, M. (2014). \emph{An application of capital allocation principles to operational risk and the cost of fraud}. Expert Systems with Applications. 41(16):7023-7031.

#'
#' @author Jilber Urbina
#' @noMd
#' @export
#' @importFrom mathjaxr preview_rd

#' @examples
#'# Exercise 2.21, page 46 in McNeil et al (2005)
#'alpha <- c(.90, .95, .975, .99, .995)
#'(ES(variance=(0.2/sqrt(250))^2, alpha=alpha, model='normal'))*10000
#'(ES(variance=(0.2/sqrt(250))^2, alpha=alpha, model='t-student', df=4))*10000
#'
#'# Both type of models at once.
#'(ES(variance=(0.2/sqrt(250))^2, alpha=alpha, model='both', df=4))*10000
#'
#'# A vector of losses
#'variance <- matrix(c(100,150,150,900), 2) # covariance matrix
#'w <- c(0.5, 0.5)                        # a vector weights
#'ES(variance=variance, weights=w, alpha=0.95)
#'


ES <- function(variance, alpha=0.95, weights=NULL, model=c('normal', 't-student', 'both'),
         df=NULL, percentage = FALSE)
{
  # alpha <- as.numeric(alpha)
  model <- match.arg(model)



  if (is.null(weights)) {
    if(is.matrix(variance)){
      w <- ncol(variance)
    } else {
      w <- 1
    }
  }
  else {
    w <- weights
  }

  alpha <- as.numeric(alpha)

  sigma <- sqrt(tcrossprod(w, crossprod(w, variance)))


  if(model=='normal' | model=='both'){
    ES.n <- as.matrix(sapply(1:length(alpha), function(y, i) {
       y *dnorm(qnorm(alpha[i]))/(1-alpha[i])
    },  y=sigma))
    ES.n <- t(data.frame(ES.n))

    if(percentage){
      colnames(ES.n) <- paste(alpha*100, "%", sep = "")
    } else {
      colnames(ES.n) <- paste(alpha)
    }

    rownames(ES.n) <- 'ES normal'
           }

  if(model=='t-student' | model=='both'){
    ES.t <- as.matrix(sapply(1:length(alpha), function(sigma, alpha, df, i)
       .5* sqrt(sigma)*( dt(qt(alpha[i], df), df)/(1-alpha[i]) )*(df+qt(alpha[i], df)*qt(alpha[i], df))/(df-1),
                           sigma=(df*sigma^2)/(df-2), alpha=alpha, df=df))
    ES.t <- t(data.frame(ES.t))
    if(percentage){
      colnames(ES.t) <- paste(alpha*100, "%", sep = "")
    } else {
      colnames(ES.t) <- paste(alpha)
    }

    rownames(ES.t) <- 'ES t-student'

  }


  if(model=='normal'){
    return(ES.n)
  }

  if(model=='t-student'){
    return(ES.t)
  }

  if(model=='both'){
  .ES <- rbind(ES.n, ES.t)
  return(.ES)
  }



}
