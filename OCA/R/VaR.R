#' Value at Risk
#'
#' @description
#' \loadmathjax{}
#' Analytical approach for calculating VaR based on Variance-Covariance Method based on both normal and t-student distribution.
#'
#' @param variance It could be either a scalar or a matrix containing the variances and covariances of the losses.
#' Provide a covariance matrix when analyzing correlated losses or a scalar when dealing with a single loss.
#' @param alpha The confidence level at which either the VaR will be computed, by default \code{alpha} is set to 0.95.
#' @param weights A vector of weights of size \emph{N} for weighting the variance of losses. When \code{weights=NULL}, variances
#'  used to compute VaR are the original values supplied to  \code{variance} with no weighting scheme.
#' @param model A character string indicating which probability model has to be used for computing the risk
#'  measures, it could be a normal distribution or a t-student distribution with \mjteqn{v}{v}{} degrees of freedom.
#'  The normal distibution is the default model for this funcion. \code{model} also allows the user to set
#'  \code{'both'} if she wishes both normal and t-student VaR or ES depending on what she choses in
#'  \code{measure}. See example below.
#' @param df An integer (df>2) denoting the degrees of freedom, only required if \code{model='t-student'}.
#'  Otherwise it has to be \code{NULL}.
#' @param percentage Logical indicating whether the file names in the VaR table should be presented in percentage or decimal.
#'
#' @return
#'  A \code{data.frame} containing the VaR at its corresponding confidence level.
#'
#' @references
#' Dhaene J., Tsanakas A., Valdez E. and Vanduffel S. (2011). \emph{Optimal Capital Allocation Principles}. The Journal of Risk and Insurance. Vol. 00, No. 0, 1-28.
#'
#' Urbina, J. (2013) \emph{Quantifying Optimal Capital Allocation Principles based on Risk Measures.} Master Thesis, Universitat Politècnica de Catalunya.
#'
#' Urbina, J. and Guillén, M. (2014). \emph{An application of capital allocation principles to operational risk and the cost of fraud}. Expert Systems with Applications. 41(16):7023-7031.
#'
#' @seealso
#'\code{\link{Risk}}, \code{\link{ES}}
#'
#' @author Jilber Urbina
#' @noMd
#' @export
#' @importFrom mathjaxr preview_rd
#' @importFrom stats cov dnorm dt qnorm qt var

#' @examples
#'
#'# Reproducing VaR from Table 2.1 in page 47 of
#'# McNeal A., Frey R. and Embrechts P (2005).
#'
#'alpha <- c(.90, .95, .975, .99, .995)
#'VaR(variance=(10000*0.2/sqrt(250))^2, alpha=alpha, model='both', df=4)
#'
#'# only normal VaR results
#'VaR(variance=(10000*0.2/sqrt(250))^2, alpha=alpha)



VaR <- function (variance, alpha = 0.95, weights = NULL,
                 model = c("normal", "t-student", "both"), df = NULL,
                 percentage = FALSE)

{

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

  if (!is.null(df) && df <= 2)
    stop("'df' must be greater than 2")
  model <- match.arg(model)

  sigma <- c(sqrt(tcrossprod(w, crossprod(w, variance))))
  if (model == "normal" | model == "both") {
    a <- qnorm(alpha)
    VaR.n <- a * sigma
    VaR.n <- t(data.frame(VaR.n))
    if(percentage){
      colnames(VaR.n) <- paste(alpha*100, "%", sep = "")
    } else {
      colnames(VaR.n) <- paste(alpha)
    }
    rownames(VaR.n) <- "VaR normal"
  }

  if (model == "t-student" | model == "both") {
    sigma <- sqrt((df * sigma^2)/(df - 2))
    a <- qt(alpha, df = df)
    VaR.t <- ( sigma * a * 0.5)
    VaR.t <- t(data.frame(VaR.t))
    if(percentage){
      colnames(VaR.t) <- paste(alpha*100, "%", sep = "")
    } else {
      colnames(VaR.t) <- paste(alpha)
    }
    rownames(VaR.t) <- "VaR t-student"
  }
  if (model == "normal") {
    return(VaR.n)
  }
  if (model == "t-student") {
    return(VaR.t)
  }
  if (model == "both") {
    return(rbind(VaR.n, VaR.t))
  }

}
