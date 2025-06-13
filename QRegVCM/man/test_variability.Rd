\name{test_variability}
\alias{test_variability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variability Estimation and Testing
}
\description{
Estimating and Testing the variability function using the following hetersocedastic varying-coefficient model.
\deqn{Y(t)=\sum_{k=0}^{p}\beta_{k}(t)X^{(k)}(t)+V(X(t),t)\epsilon(t)}
where V(X(t),t) one of the six variability function in Gijbels etal (2017).
}
\usage{
test_variability(times, subj, X, y, d, kn, degree, lambda, gam,tau,
                       nr.bootstrap.samples, seed, test, model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{times}{
The vector of time variable.
}
\item{subj}{
The vector of subjects/individuals.
}
\item{X}{
The covariate containing 1 as its first component
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
\item{tau}{
The quantiles of interest.
}
\item{nr.bootstrap.samples}{
The number of bootstrap samples used.
}
\item{seed}{
The seed for the random generator in the bootstrap resampling.
}
\item{test}{
To request for testing the specific shape of the variability function ("Y" for test
and "N" for only estimation of the parameters, the default is "Y").
}
\item{model}{
The variability model used to estimate the quantile of errors
(the default is 4, model 4).
}
}
%\details{
%The grid of lambda contributes on the wiggliness of the conditional quantile curves estimators. Hence, we suggest to use a finer grid of lambda. %Some warning messages see \url{http://cran.r-project.org/web/packages/quantreg/quantreg.pdf}
%}
\value{
\item{est_median}{the median estimator.}
\item{hat_bt50}{The median coefficients estimators.}
\item{qhat5_s2_m0}{The variability (model 0) estimator.}
\item{qhat5_s2_m1}{The variability (model 1) estimator.}
\item{qhat5_s2_m2}{The variability (model 2) estimator.}
\item{qhat5_s2_m3}{The variability (model 3) estimator.}
\item{qhat5_s2_m4}{The variability (model 4) estimator.}
\item{qhat5_s2_m5}{The variability (model 5) estimator.}
\item{hat_btV_0}{The variability coefficients (model 0) estimators.}
\item{hat_btV_1}{The variability coefficients (model 1) estimators.}
\item{hat_btV_2}{The variability coefficients (model 2) estimators.}
\item{hat_btV_3}{The variability coefficients (model 3) estimators.}
\item{hat_btV_4}{The variability coefficients (model 4) estimators.}
\item{hat_btV_5}{The variability coefficients (model 5) estimators.}
\item{C}{The estimators of the tau-th quantile of the estimated residuals.}
\item{comp}{The pairwise comparisons for testing the variabilty function.}
\item{P}{The p-values.}
\item{GR}{The test statistics for the given data.}
\item{Gb}{The bootstrap test statistics.}
}
\references{
%Andriyana, Y. (2015). P-splines quantile regression in varying coefficient models. \emph{Doctoral Dissertation}, KU Leuven, Department of Mathematics, January 2015.

Andriyana, Y. and Gijbels, I. & Verhasselt, A. (2014). P-splines quantile regression estimation in varying coefficient models. \emph{Test}, 23, 153-194.

Andriyana, Y., Gijbels, I. and Verhasselt, A. (2017). Quantile regression in varying-coefficient models: non-crossing quantile curves and heteroscedasticity. \emph{Statistical Papers,} DOI:10.1007/s00362-016-0847-7.

Gijbels, I., Ibrahim, M. A., and Verhasselt, A. (2017). Testing the heteroscedastic error structure in quantile varying coefficient models. \emph{The Canadian Journal of Statistics,} DOI:10.1002/cjs.11346.

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
%\code{\link{truncSP}}
%\code{\link{quantreg}}
%\code{\link{SparseM}}
%\code{\link{truncSP}}
%\url{http://cran.r-project.org/web/packages/quantreg/quantreg.pdf}
%\url{http://cran.r-project.org/web/packages/SparseM/SparseM.pdf}
%\url{http://cran.r-project.org/web/packages/truncSP/truncSP.pdf}
}
\examples{
############################################################################
##### The real data Example in Section S3 of the supplementary material
############################################################################
data(PM10)
PM10 = PM10[order(PM10$day,PM10$hour,decreasing=FALSE),]

y = PM10$PM10 ## the logarithm of the concentration of PM10
times = PM10$hour  ## the time in hours
subj = PM10$day  ## subject indicator (day)
dim=length(y) ## number of rows in the data = 500
x0 = rep(1,dim) ## for intercept
# the covariates
x1 = PM10$cars ## logarithm of number of cars per hour
x2 = PM10$wind.speed ## the wind speed (in meters/second)
x3 = PM10$temp ## the temperature (in degree Celsius)
X = cbind(x0, x1, x2, x3) ## the covariate matrix
px=ncol(X)

##########################
### Input parameters ####
#########################
lambda = 1 # used 10^seq(-2, 1, 0.1) in Gijbels etal (2017)
kn = rep(3,px) # used rep(10,px) in Gijbels etal (2017)
degree = rep(3,px)
d = rep(1,px)
gam=0.25
nr.bootstrap.samples= 4 # used 200 in Gijbels etal (2017)
seed=110
taus = 0.1
#########################

test1=test_variability(times=times, subj=subj, X=X, y=y, d=d, kn=kn,
                       degree=degree, lambda=lambda, gam=gam, tau=taus,
                       nr.bootstrap.samples=nr.bootstrap.samples,seed=seed,
                       test="Y",model=4)
#### Testing results
test1$comp  #the comparisons
test1$P  ## p-values
test1$GR ## test statistics

### estimation results
qhat5_s2_m4=test1$qhat5_s2_m4
qhat5_s2_m5=test1$qhat5_s2_m5
qhat5_s2_m0=test1$qhat5_s2_m0*rep(1,dim)
gamma0=test1$hat_btV_4[1:dim]
gamma1=test1$hat_btV_4[(dim+1):(dim*2)]
gamma2=test1$hat_btV_4[(dim*2+1):(dim*3)]
gamma3=test1$hat_btV_4[(dim*3+1):(dim*4)]

i = order(times, qhat5_s2_m4, qhat5_s2_m5, qhat5_s2_m0,gamma0,gamma1,
gamma2,gamma3);
times_o = times[i];  qhat5_s2_m4_o=qhat5_s2_m4[i];
qhat5_s2_m5_o=qhat5_s2_m5[i]; qhat5_s2_m0_o=qhat5_s2_m0[i]; gamma0_o=gamma0[i];
gamma1_o=gamma1[i]; gamma2_o=gamma2[i];gamma3_o=gamma3[i]

#####  variability functions plots
plot(qhat5_s2_m4_o~times_o, col="magenta", cex=0.2,
xlab="hour", ylab="estimated variability function")
lines(qhat5_s2_m5_o~times_o, col="red", cex=0.2, lty=1, lwd=2);
lines(qhat5_s2_m0_o~times_o, col="black", cex=0.2, lty=5, lwd=2);
legend("topleft", c("Model 4", "Model 5", "Model 0"), ncol=1,
        col=c("magenta","red","black"), lwd=c(1,2,2), lty=c(3,1,5))

### Plot of coefficients for variability function
plot(gamma0_o~times_o, lwd=2, type="l", xlab="hour",
ylab=expression(hat(gamma)(T)));
plot(gamma1_o~times_o, lwd=2, type="l", xlab="hour",
ylab="coefficient of logarithm of number of cars per hour");
plot(gamma2_o~times_o, lwd=2, type="l", xlab="hour",
ylab="coefficient of wind speed");
plot(gamma3_o~times_o, lwd=2, type="l", xlab="hour",
ylab="coefficient of temperature")

\dontrun{
###############################################################################
###############  The real data Example in Section 6 of Gijbels etal (2017)
###############################################################################
data(CD4)

subj = CD4$subj ## subject indicator (a man)
dim = length(subj) ## number of rows in the data = 1817
y = CD4$CD4 ## the CD4 percentage
X0 = rep(1,dim) ## the intercept
X1 = CD4$Smooking ## the smoking status
X2 = CD4$Age ## age at HIV infection
X3 = CD4$PreCD4 ## the pre-infection CD4 percentage
times = CD4$Time ## the time in years
X = cbind(X0, X1, X2, X3) ## the covariate matrix
px=ncol(X)

lambdas = c(0.01,1,10) # used 10^seq(-2, 1, 0.1) in Gijbels etal (2017)
kn = rep(10,px) # the number of internal knots for each covariate
degree = rep(3,px) # the degree of splines
d = rep(1,px) ## The differencing order in the penalty term for each covariate
gam=0.25  ## the smooting parameter for each covariate
nr.bootstrap.samples=100 ## used 200 in Gijbels etal (2017)
seed=110 ## the seed for the random generator in the bootstrap resampling
taus = seq(0.1,0.9,0.2)

test2=test_variability(times=times, subj=subj, X=X, y=y, d=d, kn=kn,
                         degree=degree, lambda=lambdas, gam=gam,tau=taus,
                         nr.bootstrap.samples=nr.bootstrap.samples,seed=seed,
                         test="Y",model=4)

test2$comp
test2$P  ## p-values
test2$GR ## test statistics

### estimation results
  qhat5_s2_m4=test2$qhat5_s2_m4
  qhat5_s2_m5=test2$qhat5_s2_m5
  qhat5_s2_m0=test2$qhat5_s2_m0*rep(1,dim)
  gamma0=test2$hat_btV_4[1:dim]
  gamma1=test2$hat_btV_4[(dim+1):(dim*2)]
  gamma2=test2$hat_btV_4[(dim*2+1):(dim*3)]
  gamma3=test2$hat_btV_4[(dim*3+1):(dim*4)]

i = order(times, qhat5_s2_m4, qhat5_s2_m5, qhat5_s2_m0,gamma0,gamma1,
            gamma2,gamma3);
times_o = times[i];  qhat5_s2_m4_o=qhat5_s2_m4[i]; qhat5_s2_m5_o=qhat5_s2_m5[i]
qhat5_s2_m0_o=qhat5_s2_m0[i]; gamma0_o=gamma0[i]; gamma1_o=gamma1[i];
gamma2_o=gamma2[i];gamma3_o=gamma3[i]

#####  variability functions plots
plot(qhat5_s2_m4_o~times_o, col="black", cex=0.2, xlab="time since infection",
ylab="estimated variability function")
lines(qhat5_s2_m5_o~times_o, col="red", cex=0.2, lty=5, lwd=2);
lines(qhat5_s2_m0_o~times_o, col="magenta", cex=0.2, lty=1, lwd=2);
legend("topleft", c("Model 4", "Model 5", "Model 0"),
         ncol=1, col=c("black","red","magenta"),
         lwd=c(1,2,2), lty=c(3,5,1))

### Plot of coefficients for variability function
plot(gamma0_o~times_o, lwd=2, type="l", xlab="time since infection",
ylab=expression(hat(gamma)(T)));
plot(gamma1_o~times_o, lwd=2, type="l", xlab="time since infection",
ylab="coefficient of smoking status");
plot(gamma2_o~times_o, lwd=2, type="l", xlab="time since infection",
ylab="coefficient of age");
plot(gamma3_o~times_o, lwd=2, type="l", xlab="time since infection",
ylab="coefficient of pre-infection CD4")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
