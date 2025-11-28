\name{pca}

\alias{pca}

\title{Principal Components Analysis}

\description{Performs a principal components analysis on the given data matrix and returns the results as an object of class "\code{prcomp}".}

\usage{
pca(x, ...)
}

\arguments{
  \item{x}{a matrix of data.}
  \item{...}{arguments passed to \code{\link{prcomp}} function.}
}


\value{
    The function returns  a list with class \code{prcomp} containing the following components:
    \item{sdev}{the standard deviations of the principal components (i.e., the square roots of the eigenvalues of the covariance/correlation matrix, though the calculation is actually done with the singular values of the data matrix).}
    \item{rotation}{the matrix of variable loadings (i.e., a matrix whose columns contain the eigenvectors). The function \code{princomp} returns this in the element \code{loadings}.}
    \item{x}{if \code{retx} is \code{TRUE} the value of the rotated data (the centred (and scaled if requested) data multiplied by the \code{rotation} matrix) is returned. Hence, \code{cov(x)} is the diagonal matrix \code{diag(sdev^2)}. For the formula method, \code{napredict()} is applied to handle the treatment of values omitted by the \code{na.action}.}
    \item{center, scale}{the centering and scaling used, or \code{FALSE}.}
    \item{txt}{the component of variance of each Principal Component.}
  }

\author{Stefano Cacciatore}

\references{
Pearson, K	\cr
On Lines and Planes of Closest Fit to Systems of Points in Space.\cr
\emph{Philosophical Magazine} 1901;2 (11): 559-572. doi:10.1080/14786440109462720. \href{https://www.tandfonline.com/doi/abs/10.1080/14786440109462720}{Link}
}
 
\seealso{\code{\link{prcomp}}}

\examples{
data(MetRef)
u=MetRef$data;
u=u[,-which(colSums(u)==0)]
u=normalization(u)$newXtrain
u=scaling(u)$newXtrain
class=as.numeric(as.factor(MetRef$gender))
cc=pca(u)
plot(cc$x,pch=21,bg=class)
}

\keyword{pca}
