\name{sparse.tscgm}
\alias{sparse.tscgm}
\title{
Sparse time series chain graphical models
}
\description{
Computes sparse vector autoregressive coefficients matrices of order 1 and 
  order 2   and precision matrix estimates for time series chain graphical 
  models using SCAD penalty. In time series chain graphs \code{directed}
  edges are identified by nonzero entries of the autoregressive 
  coefficients matrix and \code{undirected} edges  are identified by 
  nonzero entries  of the precision matrix.
}
\usage{
sparse.tscgm(data = data, lam1 = NULL, lam2 = NULL, nlambda = NULL, 
      model = c("ar1", "ar2"), penalty=c("scad","lasso"),
      optimality = c("NULL", "bic", "bic_ext", "bic_mod", "aic", "gic"), 
      control = list())
}
\arguments{
  \item{data}{
Longitudinal data format.
}
  \item{lam1}{
Either a scalar or a vector of tuning parameter values for the 
  SCAD penalty on the precision matrix. The default is \code{lam1}=NULL and 
  the program generates a sequence of tuning parameter values based on 
  \code{nlambda}. The user can also specify a sequence to change the default 
  sequence. If \code{lam1} is a vector with length at least 2, information 
  criteria based model selection will be performed to select the optimal 
  tuning parameter.
}
  \item{lam2}{
Either a scalar or a vector of tuning parameter values for the 
  SCAD penalty on the precision matrix. The default is \code{lam2}=NULL and 
  the program generates a sequence of tuning parameter values based on 
  \code{nlambda}. The user can also specify a sequence to chance the default 
  sequence. If \code{lam2} is a vector with length at least 2, information 
  criteria based model selection will be performed to select the optimal 
  tuning parameter.
}
  \item{nlambda}{
The number of values used in \code{lam1} and \code{lam2}. Default value is 10.
}
  \item{model}{
This specifies the order of vector autoregressive models. Vector autoregressive 
  model of order 1 is applied if \code{model} = "ar1" and Vector autoregressive 
  model of order 2 is applied if  method = "ar2". 
}
\item{penalty}{
This specifies the type of penalty function uto be used. SCAD penalty function 
  is applied if \code{penalty} = "scad" and GLASSO is applied if
  \code{penalty} = "lasso". 
}
  \item{optimality}{
This specifies the type of information based model selection criteria. When
  optimality is "NULL" it computes the time series chain graphical model 
  solutions   for specified scalar values of the tuning parameters \code{lam1} 
  and \code{lam2}. The rest optimality criteria first selects the optimal SCAD
  tuning parameter values using the minimum information criterion from vectors
  of tuning parameters for \code{lam1} and \code{lam2} then computes the time 
  series chain graphical model solutions based on the optimal tuning parameter
  values. 
  These are:  "BIC" (Bayesian Information Criteria),
  "bic_ext" (Extended Bayesian Information Criteria),
  "bic_mod" (Modified Bayesian Information Criteria)
  "aic" (Akakie Information Criteria), and
  "gic" (Generalized Information Criteria).
}
  \item{control}{
  The argument \code{control} includes a list of control parameters passed to 
  the main function. The user can specify its own control parameter values to 
  replace the default values. 
  
   control = list(maxit.out = 5, maxit.in = 50, tol.out = 1e-04, 
   silent = TRUE)  
   
   \code{maxit.out} : The maximum allowable outer iterations of the algorithm. 
   The outer-loop iterates alternatively to estimate the precision matrix and 
   the autoregression matrix until convergence. Default is 5.  
   \code{maxit.in}: The maximum allowable iterations of the algorithm that 
   minimizes with respect to the autoregression coefficient matrix. 
   Default is 50.
   \code{tol.out}: Convergence tolerance for outer-loop of the algorithm that 
  alternate between the estimation of precision matrix and autoregressive 
  coefficients matrix.Default is 1e-04.
  \code{silent}: Either \code{TRUE} or \code{FALSE}.  If \code{silent=FALSE} 
  this function will display progress updates to the screen. Default is TRUE.
}
}
\details{
For description of the objective functions and computational details 
see Abegaz and Wit (2013).
}
\value{
A list containing:
\item{theta}{Precision matrix estimate. The nonzero estimates of 
the precision matrix are used for constructing \code{undirected} graphs that 
represent conditional independences at time lag 0 or instantaneous interactions.}
\item{gamma}{Autoregressive coefficients matrix estimate. The nonzero estimates
of the autoregression matrix are used for constructing \code{directed} graphs 
that represent conditional independences at time lag 1, time lag 2 or dynamic 
interactions.}
\item{lam1.opt}{The optimal tuning parameter for SCAD penalty on the precision
matrix selected with minimum information criterion.} 
\item{lam2.opt}{The optimal tuning parameter for SCAD penalty on the 
autoregressive coefficients matrix selected with minimum BIC criterion.} 
\item{min.ic}{Minimum value of the optimality criterion.}
\item{tun.ic}{A matrix containing the list of tuning parameter values and the 
corresponding model selection or optimality values.}
\item{lam1.seq}{The sequence of tuning parameter values related to 
precision matrix.}
\item{lam2.seq}{The sequence of tuning parameter values related to 
autoregression matrix.}
\item{s.theta}{Sparsity level of the precision matrix.}
\item{s.gamma}{Sparsity level of the autoregression coefficients matrix.}
}
\references{
\cite{Fentaw Abegaz and Ernst Wit (2013). Sparse time series chain graphical
  models for reconstructing genetic networks. \emph{Biostatistics}.
  \bold{14}, 3: 586-599.}
  
\cite{Rothman, A.J., Levina, E., and Zhu, J. (2010). Sparse multivariate 
  regression with covariance estimation. \emph{Journal of Computational 
  and Graphical Statistics}. \bold{19}: 947--962.}
}
\author{
  Fentaw Abegaz and Ernst Wit
}

\examples{

seed = 321
datas <- sim.data(model="ar1", time=10,n.obs=10, n.var=5,seed=seed,prob0=0.35,
         network="random")
data.fit <-  datas$data1
prec_true <- datas$theta
autoR_true <- datas$gamma
    
   
res.tscgm <- sparse.tscgm(data=data.fit, lam1=NULL, lam2=NULL, nlambda=NULL, 
 model="ar1", penalty="scad",optimality="bic_mod",control=list(maxit.out = 10, maxit.in = 100))
   
#Estimated sparse precision and autoregression matrices
prec <- res.tscgm$theta
autoR <- res.tscgm$gamma

#Optimal tuning parameter values
lambda1.opt <- res.tscgm$lam1.opt
lambda2.opt <- res.tscgm$lam2.opt

#Sparsity levels
sparsity_theta <- res.tscgm$s.theta
sparsity_gamma <- res.tscgm$s.gamma

#Graphical visualization
oldpar <- par(mfrow=c(2,2))
plot.tscgm(datas, mat="precision",main="True precision matrix")         
plot.tscgm(res.tscgm, mat="precision",main="Estimated precision matrix")     
plot.tscgm(datas, mat="autoregression",main="True autoregression coef. matrix")     
plot.tscgm(res.tscgm, mat="autoregression",
             main="Estimated autoregression coef. matrix") 
par(oldpar)

}


