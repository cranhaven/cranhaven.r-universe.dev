\name{fastmerge}
\alias{fastmerge}
\docType{data}
\title{
Pre-binning of Data Points. 
}
\description{
Numerical tolerance problems in non-parametric regression makes it necessary for pre-binning of data points. This procedure is implicitly performed by most of the regression function in R. This function implements this procedure with a given tolerance level.  
}
\usage{
fastmerge(DataMat, w = NULL, tol = 1e-04)
}

\arguments{
	\item{DataMat}{a numeric matrix/vector with rows as data points.}
	\item{w}{an optional numeric vector of the same length as \eqn{x}; Defaults to all elements 1.}
	\item{tol}{a numeric value providing the tolerance for identifying duplicates with respect to the first column.}
}

\details{
If two values in the first column of DataMat are separated by a value less than tol then the corresponding rows are merged.
}
\value{
	A list including the elements
	\item{DataMat}{a numeric matrix/vector with rows sorted with respect to the first column.}
	\item{w}{obtained weights corresponding to the merged points.}
}
\seealso{
	See also the function \code{\link{smooth.spline}}.
}
\author{Arun Kumar Kuchibhotla, arunku@wharton.upenn.edu.}
\examples{
args(fastmerge)
x <- runif(100,-1,1)
y <- runif(100,-1,1)
DataMat <- cbind(x, y)
tmp <- fastmerge(DataMat)
}
\keyword{Pre-binning}