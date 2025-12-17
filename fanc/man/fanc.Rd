\name{fanc}
\alias{fanc}
\alias{print.fanc}
\title{
fanc (penalized maximum likelihood factor analysis via nonconvex penalties)
}
\description{
This package computes the solution path of penalized maximum likelihood estimates via MC+ penalties.  
}
\usage{
fanc(x, factors, n.obs, rho, gamma, cor.factor=FALSE, normalize=TRUE, 
     normalize.penalty=FALSE, covmat, type="MC", model="FA", control=list())}
\arguments{
  \item{x}{
A data matrix.
}
  \item{factors}{
The number of factors.
}
  \item{cor.factor}{
An indicator of the factor correlation.  If \code{"TRUE"}, the factor correlation is considered.  Default is \code{"FALSE"}.
}
  \item{normalize}{
If \code{"TRUE"}, each variable is normalized, otherwise it is left alone.
}
  \item{normalize.penalty}{
If \code{"TRUE"}, the penalty term for each variable has a weight so that the loading matrix is normalized.
}
  \item{rho}{
The values of rho.  It can be a scalar or a matrix.  
}
  \item{gamma}{
The values of gamma.  It must be a vector.
}
  \item{covmat}{
A covariance matrix, which is needed if the data matrix \code{"x"} is not available.
}
  \item{n.obs}{
The number of observations, which is needed to calculate the model selection criteria and goodness-of-fit indices when the data matrix \code{"x"} is not available.
}
  \item{type}{
Type of penalty. If \code{"MC"}, the MC penalty is used. If \code{"prenet"}, the prenet penalty is used. If \code{"enet"}, the elastic penalty is used. Default is \code{"MC"}.
}
  \item{model}{
Type of model. \code{"FA"}, the factor analysis model is used. If \code{"PPCA"}, the probabilistic principal component analysis is conducted. In the PPCA, the unique variances have the same value.  Default is \code{"FA"}. 
}
 \item{control}{A list of control parameters. See \sQuote{Details}.}
}

\details{
The \code{control} argument is a list that can supply any of the following components:
\describe{
	 \item{\code{length.rho}}{
Candidates of tuning parameters which is used for grid search of reparametrization of MC+.
}
  \item{\code{length.gamma}}{
A length of tuning parameter which controls sparsenesses.  For each \code{rho}, \code{gamma=Inf} yields soft threshold operator (i.e., lasso penalty) and \code{gamma=+1} produces hard threshold operator.
}
  \item{max.rho}{
Maximum value of rho.
}
  \item{\code{max.gamma}}{
A maximum value of gamma (excludes \code{Inf}.).
}
  \item{\code{min.gamma}}{
A minimum value of gamma.
}
  \item{\code{eta}}{
A tuning parameter used for preventing the occurrence of improper solutions.  \code{eta} must be non-negative.
}
  \item{\code{ncand.initial}}{
The number of candidates of initial values of factor loadings. 
}
  \item{\code{ncand.initial.prenet}}{
The number of candidates of initial values for prenet penalty.  Because the prenet penalty is unstable when \code{rho} is large, \code{ncand.initial.prenet} must be large. Default is 1000.
}
  \item{\code{maxit.em}}{
A maximum number of iterations for EM algortihm. 
}
  \item{\code{maxit.cd}}{
A maximum number of iterations for coordinate descent algortihm. 
}
  \item{\code{maxit.bfgs}}{
A maximum number of iterations for BFGS algorithm used in the update of factor correlation. 
}
  \item{\code{maxit.initial}}{
A maximum number of iterations for choosing the initial values. 
}
  \item{\code{start}}{
	Type of start.  If \code{"cold"}, the initial value of factor loadings is randomly chosen for each tuning parameter, which can be slow.
}
  \item{\code{Delta}}{
A proportion of maximum value of rho to minimum value of rho, i.e., \code{rho.min}
\code{=Delta*rho.max}. 
}
  \item{\code{min.uniquevar}}{
A minimum value of unique variances.
}
  \item{\code{tol.em}}{
A positive scalar giving the tolerance at which the parameter in EM is considered close enough to zero to terminate the algorithm. 
}
  \item{\code{tol.cd}}{
A positive scalar giving the tolerance at which the factor loadings in coordinate descent is considered close enough to zero to terminate the algorithm. 
}
  \item{\code{tol.bfgs}}{
A positive scalar giving the tolerance at which the factor correlation in BFGS algorithm is considered close enough to zero to terminate the algorithm. 
}
   \item{\code{min.rhozero}}{
	If \code{"TRUE"}, the minimum value of \code{"rho"} is zero.
}
   \item{\code{zita}}{
	A value of hyper-parameter of factor correlation.
}
   \item{\code{progress}}{
	If \code{"TRUE"}, the progress for each tuning parameter is displayed.
}
   \item{\code{openmp}}{
	If \code{"TRUE"}, the parallel computation via OpenMP is excecuted.
}
   \item{\code{num.threads}}{
	The number of threads of the openmp. Only used when \code{openmp} is \code{"TRUE"}, 
}
   \item{\code{gamma.ebic}}{
	The value of gamma used in the extended BIC 
}

}
}
\value{
\item{loadings}{factor loadings}
\item{uniquenesses}{unique variances}
\item{Phi}{factor correlation}
\item{rho}{rho}
\item{AIC}{AIC}
\item{BIC}{BIC}
\item{CAIC}{CAIC}
\item{df}{degrees of freedom (number of non-zero parameters for the lasso estimation)}
\item{criteria}{values of AIC, BIC and CAIC}
\item{goodness.of.fit}{values of GFI and AGFI}
\item{gamma}{a value of gamma}
\item{Npflag}{If the number of observation is larger than the number of variables, 1, otherwise 0.}
\item{factors}{the number of factors}
\item{cor.factor}{An indicator of the factor correlation}
\item{x}{data matrix}
\item{convergence}{indicator of convergence of EM algorithm, coordinate descent and BFGS.  If all of these variables are 0, the algorithm has been converged}
}

\references{
Hirose, K. and Yamamoto, M. (2014). 
\emph{Sparse estimation via nonconcave penalized likelihood in a factor analysis model},\cr
  \emph{Statistics and Computing, in press}
}
\author{
Kei Hirose\cr
\email{mail@keihirose.com}
}

\seealso{\code{out} and \code{plot.fanc} objects.}
\examples{
#generate data
set.seed(0)
loadings0 <- matrix(c(rep(0.8,5),rep(0,5),rep(0,5),rep(0.8,5)),10,2)
common.factors0 <- matrix(rnorm(50*2),50,2)
unique.factors0 <- matrix(rnorm(50*10,sd=sqrt(0.36)),50,10)
x <- common.factors0 \%*\% t(loadings0) + unique.factors0

#fit data
fit <- fanc(x,2)
fit2 <- fanc(x,2,cor.factor=TRUE) #factor correlation is estimated

#print candidates of gamma and rho
print(fit)

#output for fixed tuning parameters
out(fit, rho=0.1, gamma=Inf)

#select a model via model selection criterion
select(fit, criterion="BIC", gamma=Inf)

\donttest{
#plot solution path
plot(fit)
}
}
