\name{circ.kernel}
\alias{circ.kernel}
\title{Circular Kernel Density Estimation}
\description{
This function is used to perform circular kernel density estimation on the
sample data set in order to obtain the minimum points of the kernel density estimator.
}
\usage{
circ.kernel(data, sp, to = 1, grid = 512, m = 1)
}
\arguments{
 \item{data }{the data vector from which the circular kernel density estimator is to be computed.}
  \item{sp }{a real value \eqn{(0 < sp < 1)} for the smoothing parameter to be used. This value can be
obtained by using \code{\link{findh}}.}
   \item{to }{the value of the maximum domain of the data. Values will usually
either be 1 or 2\eqn{\pi}.}
  \item{grid }{the number of equally spaced grid points at which the density is to be estimated.}
  \item{m }{the number of local minimum points included in the output.}
}
\details{The Epanechnikov kernel function is used in the circular kernel density estimation. 
Circular kernel density estimation is perform according to the method proposed in 'Topics in circular statistics' (see references).
}
\value{a list containing the following components:
\item{x }{a vector of sorted \eqn{x} values that represents the equally-spaced grid points used during the kernel
density estimation.}
\item{y }{a vector of density-values of the circular kernel density estimator corresponding to \eqn{x}.}
\item{minimum }{a vector of the kernel grid point(s) of lowest density
derived from the circular kernel density estimator. The length of the vector will depend on the choice of \code{m}.}
}
\references{Hall P, Watson G, Cabrera J (1987). Kernel density estimation with spherical data.
Biometrika, 74 (4), 751-762.\cr
Jammalamadaka S, SenGupta A (2001). Topics in circular statistics. World Scientific Publishing 
Co. Pte. Ltd. \cr
Schutte WD (2014). Nonparametric estimation of the off-pulse interval(s) of a pulsar light
curve. Ph.D. thesis, North-West University. URL \url{http://hdl.handle.net/10394/12199} \cr
Schutte WD, Swanepoel JWH (2016). SOPIE: an R package for the non-parametric estimation of the off-pulse interval of a pulsar light curve. Monthly Notices of the Royal Astronomical Society, 461, 627-640.\cr
Sheather, S. & Jones, M. (1991). A reliable data-based bandwidth selection method for kernel
density estimation, Journal of the Royal Statistical Society, Series B, 53:683-690. \cr
Silverman, B. (1986). Density estimation for Statistics and Data analysis, Chapman and Hall.
Taylor, C. (2008). Automatic bandwith selection for circular density estimation, Computational
Statistics & Data Analysis, 52:3493-3500.
Wand, M. & Jones, M. (1995). Kernel Smoothing, Chapman and Hall.}
\author{ Willem Daniel Schutte }

\examples{
simdata<-von_mises_sim(n=5000,k=1,c=0.3,noise=0.2)
circ.kernel(simdata, findh(simdata), to = 1, grid = 512, m = 1)
}
\keyword{smooth}