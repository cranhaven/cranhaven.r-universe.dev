\name{simul_shapetest}
\alias{simul_shapetest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Testing the shape of a functional coefficient in the median and/or the variabilty function
}
\description{
Testing a functional coefficient of a covariate in the median and/or the variability function, considering the general hetersocedastic varying-coefficient model in Gijbels etal (2017a).
\deqn{Y(t)=\sum_{k=0}^{p}\beta_{k}(t)X^{(k)}(t)+\gamma(t)\exp(\sum_{k=1}^{p}\theta_{k}(t)X^{(k)}(t))\epsilon(t).}
}
\usage{
simul_shapetest(times, subj, X, y, d, kn, degree, lambda, gam, v,
                       nr.bootstrap.samples, seed, test,omega)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{times}{
The vector of time variable.
}
\item{subj}{
The vector of subject indicator.
}
\item{X}{
The covariates, containing 1 as its first component
(including intercept in the model).
}
\item{y}{
The response vector.
}
\item{d}{
The order of differencing operator for each covariate.
}
\item{kn}{
The number of knot intervals for each covariate.
}
\item{degree}{
The degree of B-spline basis for each covariate.
}
\item{lambda}{
The grid of smoothing parameter to control the trade between
fidelity and penalty term (use a fine grid of lambda).
}
\item{gam}{
The power used in estimating the smooting parameter for each
covariate (e.g. gam=1 or gam=0.5).
}
\item{v}{
The covariate indicator for which the shape test is interested.
}
\item{nr.bootstrap.samples}{
The number of bootstrap samples used.
}
\item{seed}{
The seed for the random generator in the bootstrap resampling.
}
\item{test}{
The requested type of testing, it consists two arguments:
the first argument for median and the second for the variability function.
"c" stands for constancy, "m" stands for monotonicity, and "conv" stands for convexity.
insert NA to the other argument when only for median/ variability function is needed..
}
\item{omega}{
A user defined constraint parameter for monotonicity or convexity (in Equation (7) of
Gijbels etal (2017a)), chosen as large as possible.
}
}
%\details{
%The grid of lambda contributes on the wiggliness of the estimated functional coefficient. Hence, we suggest to use a finer grid of lambda. %Some warning messages see \url{http://cran.r-project.org/web/packages/quantreg/quantreg.pdf}
%}
\value{
\item{result}{The testing procedures.}
\item{P}{The p-values.}
\item{GR}{The test statistics for the given data.}
\item{Gb}{The bootstrap test statistics.}
}
\references{
%Andriyana, Y. (2015). P-splines quantile regression in varying coefficient models. \emph{Doctoral Dissertation}, KU Leuven, Department of Mathematics, January 2015.

Andriyana, Y. and Gijbels, I. & Verhasselt, A. (2014). P-splines quantile regression estimation in varying coefficient models. \emph{Test}, 23, 153-194.

Andriyana, Y., Gijbels, I. and Verhasselt, A. (2017). Quantile regression in varying-coefficient models: non-crossing quantile curves and heteroscedasticity. \emph{Statistical Papers,} DOI:10.1007/s00362-016-0847-7

Gijbels, I., Ibrahim, M. A., and Verhasselt, A. (2017a). Shape testing in quantile varying coefficient models with heteroscedastic error. \emph{Journal of Nonparametric Statistics, 29(2):391-406.}

Gijbels, I., Ibrahim, M. A., and Verhasselt, A. (2017b). Testing the heteroscedastic error structure in quantile varying coefficient models. \emph{The Canadian Journal of Statistics,} DOI:10.1002/cjs.11346.

He, X. (1997). Quantile curves without crossing. \emph{The American Statistician}, 51, 186-192.
}
\author{
Mohammed Abdulkerim Ibrahim
}
\note{
Some warning messages are related to the function \code{\link[quantreg]{rq.fit.sfn}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%\code{\link{quantreg}}%, SparseM, truncSP}}%, ~~~
\code{\link[quantreg]{rq.fit.sfn}}
\code{\link[SparseM]{as.matrix.csr}}
\code{\link{truncSP}}
%\code{\link{quantreg}}
%\code{\link{SparseM}}
%\code{\link{truncSP}}
%\url{http://cran.r-project.org/web/packages/quantreg/quantreg.pdf}
%\url{http://cran.r-project.org/web/packages/SparseM/SparseM.pdf}
%\url{http://cran.r-project.org/web/packages/truncSP/truncSP.pdf}
}
\examples{
data(wages)
y = wages$resp ## the hourly wage
times = wages$exper ## the duration of work experience in years
subj = wages$id ## subject indicator (individual)
dim=length(y) ## number of rows in the data = 6402
x0 = rep(1,dim) ## for intercept
### the covariates
## creating 2 dummy variables for the race covariate
wages$r1[wages$race=="black"]=1
wages$r1[wages$race!="black"]=0
wages$r2[wages$race=="hisp"]=1
wages$r2[wages$race!="hisp"]=0
x1 = wages$r1 # stands for black
x2 = wages$r2 # stands for hispanic
x3 = wages$hgc ## the highest grade completed by the indiviadual
X = cbind(x0, x1, x2, x3) ## the covariate matrix
px=ncol(X)

##########################
### Input parameters ####
#########################
lambda = 1 # we used 10^seq(-2, 1, 0.1) in Gijbels etal (2017a)
kn = rep(1,px) # used rep(5,px) in Gijbels etal (2017a)
degree = rep(2,px) # the degree of splines
d = rep(1,px)
gam=0.25
nr.bootstrap.samples=2 # used 200 in Gijbels etal (2017a)
seed=110
#########################
test1=simul_shapetest(times=times, subj=subj, X=X, y=y, d=d, kn=kn,
                     degree=degree, lambda=lambda, gam=gam, v=1,
                     nr.bootstrap.samples=nr.bootstrap.samples,seed=seed,
                     test=c("c",NA),omega=10^3)
#### Testing results
test1$result  #the testing procedures
test1$P  ## p-values
test1$R ## test statistics


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
