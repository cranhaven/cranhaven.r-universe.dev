\name{findh}
\alias{findh}
\title{Calculate the Estimated Smoothing Parameter
}
\description{
This function is used to obtain the estimated smoothing parameter \eqn{\hat h} that will be
used in the circular kernel density estimator (see \code{\link{circ.kernel}}).
}
\usage{
findh(data, h = 1, to = 1)
}

\arguments{
  \item{data }{the data vector from which to calculate the estimated 
smoothing parameter \eqn{\hat h} that will be used in the circular kernel density estimator.}
   \item{h }{integer value from 1 to 9, specifying the smoothing parameter to calculate
according to the following table:
\tabular{l}{
\eqn{\hat h_1=1.06sn^{-1/5}}\cr
\eqn{\hat h_2=1.06s_\circ n^{-1/5}}\cr
\eqn{\hat h_3=1.06\bar{D}_\circ n^{-1/5}}\cr
\eqn{\hat h_4=1.06|{D}_\circ|n^{-1/5}}\cr
\eqn{\hat h_5=1.06{IQR}_\circ n^{-1/5}}\cr
\eqn{\hat h_6=\frac{1.06}{1.349}{IQR}_\circ n^{-1/5}}\cr
\eqn{\hat h_7=0.9s_\circ n^{-1/5}}\cr
\eqn{\hat h_8=\frac{0.9}{1.349}{IQR}_\circ n^{-1/5}}\cr
\eqn{\hat h_9=\frac{1}{8}\sum _{i=1}^8{h_i}}
}
}
  \item{to }{the value of the maximum domain of the data. Values will usually
either be 1 or 2\eqn{\pi}.}
}
\value{
The function produces a single real value between 0 and 1, representing the rounded value (to 2 decimal
places) of the estimating smoothing parameter.
}
\references{
Hall P, Watson G, Cabrera J (1987). Kernel density estimation with spherical data.
Biometrika, 74 (4), 751-762.\cr
Jammalamadaka S, SenGupta A (2001). Topics in circular statistics. World Scientific Publishing
Co. Pte. Ltd.\cr
Schutte WD (2014). Nonparametric estimation of the off-pulse interval(s) of a pulsar light
curve. Ph.D. thesis, North-West University. URL \url{http://hdl.handle.net/10394/12199} \cr
Schutte WD, Swanepoel JWH (2016). SOPIE: an R package for the non-parametric estimation of the off-pulse interval of a pulsar light curve. Monthly Notices of the Royal Astronomical Society, 461, 627-640.\cr
Sheather, S. & Jones, M. (1991). A reliable data-based bandwidth selection method for kernel
density estimation, Journal of the Royal Statistical Society, Series B, 53:683-690. \cr
Silverman, B. (1986). Density estimation for Statistics and Data analysis, Chapman and Hall.
Taylor, C. (2008). Automatic bandwith selection for circular density estimation, Computational
Statistics & Data Analysis, 52:3493-3500.
Wand, M. & Jones, M. (1995). Kernel Smoothing, Chapman and Hall.
}
\author{ Willem Daniel Schutte }

\examples{
simdata<-von_mises_sim(n=5000,k=1,c=0.3,noise=0.2)
findh(simdata,h=9,to=1)
}