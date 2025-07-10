\name{sbaic}
\alias{sbaic}
\alias{sbaic<-}
\alias{sbaic.scaleboot}
\alias{sbaic.scalebootv}
\alias{sbaic<-.scaleboot}
\alias{sbaic<-.scalebootv}
\title{Akaike's Information Criterion}
\description{
Extract or modify the AIC values for models.
}
\usage{
sbaic(x,...)
\method{sbaic}{scaleboot}(x,k,...)
\method{sbaic}{scalebootv}(x,...)

sbaic(x) <- value
\method{sbaic}{scaleboot}(x) <- value
\method{sbaic}{scalebootv}(x) <- value

}
\arguments{
  \item{x}{an object used to select a method. }
  \item{k}{numeric, the penalty per parameter to be used.}
  \item{value}{numeric vector of AIC values for models.}
  \item{...}{further arguments passed to and from other methods.}
}
\details{
  \code{sbaic} can be used to modify the \code{aic} components for
  models in \code{x} as shown in the examples below. 
}
\value{
  For an object of class \code{"scaleboot"},
  \code{sbaic} returns a numeric vector of AIC values for
  models.  If
  \code{k} is missing, then the \code{aic} components in the \code{fi} vector of
  \code{x} are returned.  If \code{k} is specified, \code{rss-k*df} is
  calculated for each model. For the usual AIC, k=2.  For the BIC
  (Schwarz's Bayesian information criterion), \code{k=log(sum(x$nb))}.
}
\references{
  Sakamoto, Y., Ishiguro, M., and Kitagawa G. (1986).
  \emph{Akaike Information Criterion Statistics}.
  D. Reidel Publishing Company.
}
\author{Hidetoshi Shimodaira}
\seealso{\code{\link{sbfit}}.}
\examples{
data(mam15)
a <- mam15.relltest[["t4"]] # an object of class "scaleboot"
sbaic(a) # print AIC for models
sbaic(a,k=log(sum(a$nb))) # print BIC for models
sbaic(a) <- sbaic(a,k=log(sum(a$nb))) # set BIC
sbaic(a) # print BIC for models
}
\keyword{ models }
