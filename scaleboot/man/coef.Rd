\name{coef}
\alias{coef.scaleboot}
\alias{coef.scalebootv}

\title{Extract Model Coefficients}
\description{
Extract the estimated parameters from \code{"scaleboot"} or
\code{"scalebootv"} objects.
}
\usage{

\method{coef}{scaleboot}(object,sd=FALSE,...)

\method{coef}{scalebootv}(object,...)

}
\arguments{
  \item{object}{an object used to select a method.}
  \item{...}{further arguments passed to or from other methods.}  
  \item{sd}{logical. Should standard errors be returned as well?}
}
\value{
  The \code{coef} method for the class \code{"scaleboot"} returns a
  matrix consisting of row vectors of beta's for models. If
  \code{sd=TRUE}, it returns a list with components \code{estimate} and
  \code{sd} for the beta matrix and its standard error respectively.
}
\author{Hidetoshi Shimodaira}
\seealso{\code{\link{sbfit}}}
\examples{
data(mam15)
a <- mam15.relltest[["t4"]] # an object of class "scaleboot"
coef(a) # print the estimated beta values
coef(a,sd=TRUE) # with sd
}
\keyword{ models }
