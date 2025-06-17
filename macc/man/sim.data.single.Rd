\name{sim.data.single}
\alias{sim.data.single}
\title{ Generate single-level simulation data
}
\description{ This function generates a single-level dataset with given parameters.
}
\usage{
sim.data.single(Z, Theta, Sigma)
}
\arguments{
  \item{Z}{ a vector of treatment/exposure assignment.
}
  \item{Theta}{ a 2 by 2 matrix, containing the model coefficients.
}
  \item{Sigma}{ a 2 by 2 matrix, is the covariance matrix of the model errors.
}
}
\details{ The single level mediation model is
  \deqn{M=ZA+E_1,}
  \deqn{R=ZC+MB+E_2.}
  \code{Theta[1,1] = A}, \code{Theta[1,2] = C} and \code{Theta[2,2] = B}; \code{Sigma} is the covariance matrix of \eqn{(E_1,E_2)}.
}
\value{ The function returns a dataframe with variables \code{Z}, \code{M} and \code{R}.
}
\references{Zhao, Y., & Luo, X. (2014). \emph{Estimating Mediation Effects under Correlated Errors with an Application to fMRI.} arXiv preprint arXiv:1410.7217.
}
\author{ Yi Zhao, Brown University, \email{yi_zhao@brown.edu};
  Xi Luo, Brown University, \email{xi.rossi.luo@gmail.com}
}
\examples{
  ###################################################
  # Generate a single-level dataset

  # covariance matrix of errors
  delta<-0.5
  Sigma<-matrix(c(1,2*delta,2*delta,4),2,2)

  # model coefficients
  A0<-0.5
  B0<--1
  C0<-0.5

  Theta<-matrix(c(A0,0,C0,B0),2,2)

  # number of trials
  n<-100

  # generate a treatment assignment vector
  set.seed(100)
  Z<-matrix(rbinom(n,size=1,0.5),n,1)

  # Data
  set.seed(5000)
  data.single<-sim.data.single(Z,Theta,Sigma)
  ###################################################
}
\keyword{ datagen }
