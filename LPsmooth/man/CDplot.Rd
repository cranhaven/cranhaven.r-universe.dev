\name{CDplot}
\alias{CDplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  CD-plot and Deviance test
}
\description{
Constructs the CD-plot and computes the deviance test for exhaustive goodness-of-fit.
}
\usage{
CDplot(data,m=4,g,par0=NULL,range=NULL,lattice=NULL,selection=TRUE,criterion="BIC",
        B=1000,samplerG=NULL,h=NULL,samplerH=NULL,R=500,ylim=c(0,2),CD.plot=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  A data vector. See details.
}
  \item{m}{
  If \code{selection = FALSE}, it corresponds to the desired size of the polynomial basis to be used.
If \code{selection = TRUE}, it is the size of the polynomial basis from which the terms to include in the model are selected.
}
  \item{g}{
Function corresponding to the parametric start. See details.
}
  \item{par0}{
  A vector of starting values for the parameters of \code{g} when the latter is not fully known. See details.
}
  \item{range}{
  Interval corresponding to the support of the continuous data distribution.
}
  \item{lattice}{
  Support of the discrete data distribution.
}
  \item{selection}{
  A logical argument indicating if model selection should be performed. See details.
}
  \item{criterion}{
  If \code{selection=TRUE}, the selection criterion to be. The two possibilities are \code{"AIC"} or \code{"BIC"}. See details.}
  \item{B}{
  A positive integer corresponding to the number of bootstrap replicates.
}
  \item{samplerG}{
  A function corresponding to the random sampler for the parametric start \code{g}. See details.
  }
  \item{h}{
  Instrumental probability function. If \code{samplerG} is not \code{NULL}, the argument \code{h} will not be used.
 }
  \item{samplerH}{
 A function corresponding to the random sampler for the  instrumental probability function \code{h}. If \code{samplerG} is not \code{NULL}, the argument \code{samplerH} will not be used.
}
  \item{R}{
  A positive integer corresponding to the size of the grid of equidistant points at which the comparison densities are evaluated. The default is \code{R = 500}, a larger value may be needed when the smoothness of the comparison densities decrease.
}
  \item{ylim}{
  If \code{check.plot=TRUE}, the range of the y-axis of the respective comparison density plot. The default is \code{c(0, 2)}.}
  \item{CD.plot}{
  A logical argument indicating if the comparison density plot should be displayed or not. The default is \code{TRUE}.
}
}
\details{
The argument \code{data} collects the data for which we want to test if its distribution corresponds to the one of the postulated model specified in the argument \code{g}.
If the parametric start is fully known, it must be specified in a way that it takes \code{x} as the only argument. If the parametric start is not fully known, it must be specified in a way that it takes arguments \code{x} and \code{par}, with \code{par} corresponding to the vector of unknown parameters. The latter are estimated numerically via maximum likelihood estimation and \code{par0} specifies the initial values of the parameters to be used in the optimization.
The value \code{m} determines the smoothness of the estimated comparison density, with smaller values of \code{m} leading to smoother estimates.
If \code{selection=TRUE}, the largest coefficient estimates are selected according to either the AIC or BIC criterion as described in Algeri and Zhang, 2020 (see also Ledwina, 1994 and Mukhopadhyay, 2017). The resulting estimator is the one in Gajek's formulation with orthonormal basis corresponding to LP score functions (see Algeri and Zhang, 2020 and Gajek, 1986).
}
\value{
\item{Deviance}{Value of the deviance test statistic.}
\item{p_value}{P-value of the deviance test.}
}
\references{
Algeri S. and Zhang X. (2020). Exhaustive goodness-of-fit via smoothed inference and graphics. arXiv:2005.13011.

Gajek, L. (1986). On improving density estimators which are not bona fide functions. The Annals of sStatistics, 14(4):1612--1618.

Ledwina, T. (1994). Data-driven version of neymany's smooth test of fit. Journal of the American Statistical Association, 89(427):1000--1005.

Mukhopadhyay, S. (2017). Large-scale mode identification and data-driven sciences. Electronic Journal of Statistics 11 (2017), no. 1, 215--240.}
\author{
Sara Algeri and Xiangyu Zhang
}

\seealso{
\code{\link{d_hat}}, \code{\link{find_h_disc}}, \code{\link{find_h_cont}}.
}
\examples{
data<-rbinom(50,size=20,prob=0.5)
g<-function(x)dpois(x,10)/(ppois(20,10)-ppois(0,10))
samplerG<-function(n){xx<-rpois(n*3,10)
                      xxx<-sample(xx[xx<=20],n)
                      return(xxx)}
CDplot(data,m=4,g,par0=NULL,range=NULL,lattice=seq(0,20),
       selection=FALSE,criterion="BIC",B=10,samplerG,R=300,ylim=c(0,2))
}

\keyword{ Comparison density plot }% use one of  RShowDoc("KEYWORDS")
\keyword{ Deviance test }% use one of  RShowDoc("KEYWORDS")
