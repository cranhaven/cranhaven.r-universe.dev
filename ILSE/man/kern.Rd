\name{kern}
\alias{kern}
\title{Kernel Function}
\description{
  Different type of kernel functions.
}
\usage{
  kern(u, type='epk')
}
\arguments{
  \item{u}{a numeric vector, evluated points in kernel funciton.}
  \item{type}{a optional character string, specify the type of used kernel functionand support 'epk', 'biweight', 'triangle', 'guassian', 'triweight', 'tricube', 'cosine', 'uniform' in current version, defualt as 'epk'.}
}
\details{
  Note that K(u_i)=K(X_i-x_0) where u = (X_1-x_0, ..., X_n-x_0) and K_h(u_i)=1/h*K((X_i-x_0)/h) where h is bandwidth.
}
\value{
 Return a numeric vector with length equal to 'u'.
}
\author{
Liu Wei
}
\seealso{
KernSmooth package
}
\examples{
library(graphics)
u <- seq(-1,1,by=0.01)
(Ku <- kern(u))
plot(u, Ku, type='l')
# guassian kernel
plot(u, kern(u, type='gaussian'), type ='l')
# cosine kernel
plot(u, Ku <- kern(u, type='cosine'), type ='l')
}
