\name{summary.scaleboot}
\alias{summary.scaleboot}
\alias{summary.scalebootv}
\alias{print.summary.scaleboot}
\alias{print.summary.scalebootv}
\title{P-value Calculation for Multiscale Bootstrap}
\description{
  \code{summary} method for class \code{"scaleboot"} and \code{"scalebootv"}.
}
\usage{

\method{summary}{scaleboot}(object,models=names(object$fi),k=3,sk=k,s=1,sp=-1,
              hypothesis=c("auto","null","alternative"),
              type=c("Frequentist","Bayesian"),...)

\method{summary}{scalebootv}(object,models=attr(object,"models"),k=3,sk=k,
              hypothesis="auto",type="Frequentist", select="average",...)

\method{print}{summary.scaleboot}(x,sort.by=c("aic","none"),verbose=FALSE,...)

\method{print}{summary.scalebootv}(x,...)
}
\arguments{
  \item{object}{an object used to select a method. }
  \item{models}{character vector of model names. If numeric,
    \code{names(object$fi)[models]} is used for each \code{"scaleboot"} object.}
  \item{k}{numeric vector of \eqn{k} for calculating p-values.}
  \item{sk}{numeric vector of \eqn{k} for calculating selective inference p-values.}
  \item{s}{\eqn{\sigma_0^2}}
  \item{sp}{\eqn{\sigma_p^2}}
  \item{hypothesis}{specifies type of selective infernece.
  "null" takes the region as null hypothesis, and "alternative" takes the region as alternative hypothesis.
  "auto" determins it by the sign of beta0. The selectice pvalues (\code{sk.1}, \code{sk.2}, ...) are selective pvalues when "null", and they are one minus selective pvalues when "alternative".}
  \item{type}{If numeric, it is passed to \code{sbpsi} functions as
    \code{lambda} to specify p-value type. If "Frequentist" or
    "Bayesian", then equivalent to specifying \code{lambda} = 1 or 0,
    respectively.}
  \item{select}{character of model name (such as "poly.3") or one of
    "average" and "best". If "average" or "best", then the averaging by
    Akaike weights or the best model is used, respectively.}
  \item{x}{object.}
  \item{sort.by}{sort key.}
  \item{verbose}{logical.}
  \item{...}{further arguments passed to and from other methods.}  
}

\details{ For each model, a class of approximately unbiased p-values,
indexed by \eqn{k=1,2,...}, is calculaed.  The p-values are named 
\code{k.1}, \code{k.2}, ..., where \eqn{k=1} (\code{k.1}) corresponds to
the ordinary bootstrap probability, and \eqn{k=2} (\code{k.2})
corresponds to the third-order accurate p-value of Shimodaira (2002). As the
\eqn{k} value increases, the bias of testing decreases, although the
p-value becomes less stable numerically and the monotonicity of rejection
regions becomes worse. Typically, \eqn{k=3} provides a reasonable
compromise. The \code{\link{sbpval}} method is available to extract p-values from
the \code{"summary.scaleboot"} object.

The p-value is defined as
  \deqn{ p_k = 1 - \Phi\left( \sum_{j=0}^{k-1}
\frac{(\sigma_p^2-\sigma_0^2)^j}{j!}  \frac{d^j \psi(x|\beta)}{d
x^j}\Bigr|_{\sigma_0^2} \right),} where \eqn{\psi(\sigma^2|\beta)} is the
model specification function, \eqn{\sigma_0^2} is the evaluation point
for the Taylor series, and \eqn{\sigma_p^2} is an additional
parameter. Typically, we do not change the default values
\eqn{\sigma_0^2=1} and \eqn{\sigma_p^2=-1}.

The p-values are justified only for good fitting models. By default,
the model which minimizes the AIC value is selected. We can modify the
AIC value by using the \code{sbaic} function. We also diagnose the
fitting by using the \code{plot} method.

Now includes selective inference p-values.
The method is described in Terada and Shimodaira (2017; arXiv:1711.00949)
"Selective inference for the problem of regions via multiscale bootstrap".
}
\value{
  \code{summary.scaleboot} returns
  an object of the class \code{"summary.scaleboot"}, which is inherited
  from the class \code{"scaleboot"}. It is a list containing all the components of class
  \code{"scaleboot"} and the following components:
  \item{pv}{matrix of p-values of size \code{length(models)} *
    \code{length(k)} with elements \eqn{p_k}.}
  \item{pe}{matrix of standard errors of p-values.}
  \item{spv}{matrix of selective inference p-values of size \code{length(models)} *
    \code{length(sk)} with elements \eqn{sp_k}.}
  \item{spe}{matrix of standard errors of selective inference p-values.}
  \item{betapar}{list array containing (beta0, beta1) and its covariance matrix
        for each model. They are obtained by linear extrapolation.
        This will be used for interpreting the fitting in terms of signed distance and curvature.}
  \item{best}{ a list consisting of components \code{model} for the best
  fitting model name, \code{aic} for its AIC value, \code{pv} and \code{spv} for 
  vector of p-values, and \code{pe} and \code{spe} for vectors of standard errors.
  Also includes \code{betapar} for the best model.}
  \item{average}{ a list of results for the average model computed by Akaike weight.}
  \item{parex}{a list of components \code{k}, \code{s}, and \code{sp}.}
}
\author{Hidetoshi Shimodaira}
\seealso{\code{\link{sbfit}}, \code{\link{sbpsi}}, \code{\link{sbpval}},
   \code{\link{sbaic}}.}
\examples{
data(mam15)
## For a single hypothesis
a <- mam15.relltest[["t4"]] # an object of class "scaleboot"
summary(a) # calculate and print p-values (k=3)
summary(a,k=2) # calculate and print p-values (k=2)
summary(a,k=1:4) # up to "k.4" p-value.

## For multiple hypotheses
b <- mam15.relltest[1:15] # an object of class "scalebootv"
summary(b) # calculate and print p-values (k=3)
summary(b,k=1:4) # up to "k.4" p-value.

}
\keyword{ models }
\keyword{ nonparametric }
