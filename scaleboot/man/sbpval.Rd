\name{sbpval}
\alias{sbpval}
\alias{sbpval.summary.scaleboot}
\alias{sbpval.summary.scalebootv}
\title{Extract P-values}
\description{
  \code{sbpval} extracts p-values from \code{"summary.scaleboot"} or
    \code{"summary.scalebootv"} objects.
}
\usage{
sbpval(x, ...)

\method{sbpval}{summary.scaleboot}(x,select=c("average","best","all"),...)

\method{sbpval}{summary.scalebootv}(x,...)

}
\arguments{
  \item{x}{an object used to select a method.}
  \item{select}{character. If "average" or "best", only the p-values of
    corresponding models are returned. If "all", then p-values of all
    the models are returned.}
  \item{...}{further arguments passed to or from other methods.}  
}
\details{
  This method is used only to extract previously calculated p-values from the
  summary object.
}
\value{
  The \code{sbpval} method for the class \code{"summary.scaleboot"} returns a
  list of three components (pvalue, sd, hypothesis).
  pvalue is a vector of pvalues 
  \eqn{p_k} for \eqn{k} as specified in the \code{summary} method.
  sd is a vector of their standard errors.
  hypothesis is either "null" or "alternative" for the selective inference.

  The \code{sbpval} method for the class \code{"summary.scalebootv"} returns a
  list of the three components, where pvalue and sd are matrices
  and hypothesis is a vector.
}
\author{Hidetoshi Shimodaira}
\seealso{\code{\link{summary.scaleboot}}.}
\examples{
data(mam15)
a <- mam15.relltest[["t4"]] # an object of class "scaleboot"
b <- summary(a) # calculate p-values
b # print the p-values
sbpval(b) # extract a vector of p-values which are averaged by Akaike weights.
sbpval(b,select="all") # extract a matrix of p-values
}
\keyword{ models }
\keyword{ nonparametric }
