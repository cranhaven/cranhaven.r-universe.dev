\name{sim.data}
\alias{sim.data}
\title{
Multivariate time series simulation with chain graphical models
}
\description{
Generates sparse vector autoregressive coefficients matrices and
  precision matrix from various network structures and using these matrices
  generates  repeated multivariate  time series dataset.
  }
\usage{
 sim.data(model=c("ar1","ar2"),time=time,n.obs=n.obs, n.var=n.var,seed=NULL,
          prob0=NULL, network=c("random","scale-free","hub","user_defined"),
          prec=NULL,gamma1=NULL,gamma2=NULL)
}
\arguments{
  \item{model}{
Specifies the order of vector autoregressive models. Vector autoregressive
  model of order 1 is applied if \code{model} = "ar1" and Vector autoregressive
  model of order 2 is applied if  method = "ar2".
}
  \item{time}{ Number of time points.}
  \item{n.obs}{ Number of observations or replicates.}
  \item{n.var}{Number of variables.}
  \item{seed}{Random number seed.}
  \item{prob0}{Initial sparsity level.}
  \item{network}{
Specifies the type of network structure. This could be random, scale-free, hub
or user defined structures. Details on simultions from the various network 
structures can be found in the R package \code{flare}.
}
  \item{prec}{Precision matrix.}
  \item{gamma1}{Autoregressive coefficients matrix at time lag 1.}
  \item{gamma2}{Autoregressive coefficients matrix at time lag 2.}
  }
\value{
A list containing:
\item{theta}{Sparse precision matrix. }
\item{gamma}{Sparse autoregressive coefficients matrix.}
\item{sigma}{Covariance matrix.}
\item{data1}{Repeated multivariate time series data in longitudinal format.}
}
\author{
  Fentaw Abegaz and Ernst Wit
}

\examples{
seed = 321
datas <- sim.data(model="ar1", time=4,n.obs=3, n.var=5,seed=seed,prob0=0.35,
         network="random")
data.ts <-  datas$data1
prec_true <- datas$theta
autoR_true <- datas$gamma
}
