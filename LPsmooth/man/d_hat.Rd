\name{d_hat}
\alias{d_hat}

\title{
Comparison density estimate
}
\description{
Estimates the comparison density for continuous and discrete data.}
\usage{
d_hat(data,m=4,g,range=NULL,lattice=NULL,selection=TRUE,criterion="BIC")
}
\arguments{
  \item{data}{A data vector. See details.
}
  \item{m}{If \code{selection = FALSE}, it corresponds to the desired size of the polynomial basis to be used.
If \code{selection = TRUE}, it is the size of the polynomial basis from which the terms to include in the model are selected.
}
  \item{g}{
Function corresponding to the parametric start. See details.
}
  \item{range}{Interval corresponding to the support of the continuous data distribution.
}
  \item{lattice}{
Support of the discrete data distribution.
}
  \item{selection}{
A logical argument indicating if model selection  should be performed. See details.
}
  \item{criterion}{
If \code{selection=TRUE}, the selection criterion to be used. The two possibilities are \code{"AIC"} or \code{"BIC"}. See details.
}
}
\details{
The argument \code{data} collects the data for which we want to test if its distribution corresponds to the one of the postulated model specified in the argument \code{g}. The parametric start is assumed to be fully specified and takes \code{x} as the only argument.
The value \code{m} determines the smoothness of the estimated comparison density, with smaller values of \code{m} leading to smoother estimates.
If \code{selection=TRUE}, the largest coefficient estimates are selected according to either the AIC or BIC criterion as described in Algeri and Zhang, 2020 (see also Ledwina, 1994 and Mukhopadhyay, 2017). The resulting estimator is the one in Gajek's formulation with orthonormal basis corresponding to LP score functions (see Algeri and Zhang, 2020 and Gajek, 1986).
}
\value{
\item{LPj}{Estimates of the coefficients.}
\item{du}{Function corresponding to the estimated comparison density in the u domain corresponding to the probability integral transformation.}
\item{dx}{Function corresponding to the estimated comparison density in the x domain.}
\item{f}{Function corresponding to the estimated probability function of the data.}
}
\references{
Algeri S. and Zhang X. (2020). Exhaustive goodness-of-fit via smoothed inference and graphics. arXiv:2005.13011.

Gajek, L. (1986). On improving density estimators which are not bona fide functions. The Annals of sStatistics, 14(4):1612--1618.

Ledwina, T. (1994). Data-driven version of neymany's smooth test of fit. Journal of the American Statistical Association, 89(427):1000--1005.

Mukhopadhyay, S. (2017). Large-scale mode identification and data-driven sciences. Electronic Journal of Statistics 11 (2017), no. 1, 215--240.
}
\author{
Sara Algeri and Xiangyu Zhang
}


\seealso{
\code{\link{CDplot}}
}
\examples{
library("LPBkg")
#Example discrete
data<-rbinom(1000,size=20,prob=0.5)
g<-function(x)dpois(x,10)/(ppois(20,10)-ppois(0,10))
ddhat<-d_hat(data,m=4,g, range=NULL,lattice=seq(0,20), selection=TRUE,criterion="BIC")
xx<-seq(0,20)
ddhat$dx(xx)
ddhat$LPj

#Example continuous
data<-rnorm(1000,0,1)
g<-function(x)dt(x,10)
ddhat<-d_hat(data,m=4,g, range=c(-100,100), selection=TRUE,criterion="AIC")
uu<-seq(0,1,length=10)
ddhat$du(uu)
ddhat$LPj
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Comparison density estimate }% use one of  RShowDoc("KEYWORDS")
\keyword{ Gajek estimator }% __ONLY ONE__ keyword per line
