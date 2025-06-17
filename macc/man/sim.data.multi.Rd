\name{sim.data.multi}
\alias{sim.data.multi}
\title{ Generate two/three-level simulation data
}
\description{ This function generates a two/three-level dataset with given parameters.
}
\usage{
sim.data.multi(Z.list, N, K = 1, Theta, Sigma, 
    Psi = diag(rep(1, 3)), Lambda = diag(rep(1, 3)))
}
\arguments{
  \item{Z.list}{ a list of data. When \code{K = 1} (a two-level dataset), each list is a vector containing the treatment/exposure assignment of the trials for each subject; When \code{K > 1} (a three-level dataset), each list is a list of length \code{K}, and each contains a vector of treatment/exposure assignment.
}
  \item{N}{ an integer, indicates the number of subjects.
}
  \item{K}{ an integer, indicates the number of sessions of each subject. 
}
  \item{Theta}{ a 2 by 2 matrix, containing the population level model coefficients.
}
  \item{Sigma}{ a 2 by 2 matrix, is the covariance matrix of the model errors in the single-level model.
}
  \item{Psi}{ the covariance matrix of the random effects in the mixed effects model of the model coefficients. This is used only when \code{K > 1}. Default is a 3-dimensional identity matrix.
}
  \item{Lambda}{ the covariance matrix of the model errors in the mixed effects model if \code{K > 1} or the linear model if \code{K = 1} of the model coefficients.
}
}
\details{ When \code{K > 1} (three-level data), for the \eqn{n_{ik}} trials in each session of each subject, the single level mediation model is
  \deqn{M_{ik}=Z_{ik}A_{ik}+E_{1ik},}
  \deqn{R_{ik}=Z_{ik}C_{ik}+M_{ik}B_{ik}+E_{2ik},}
  where \eqn{Z_{ik}}, \eqn{M_{ik}}, \eqn{R_{ik}}, \eqn{E_{1ik}}, and \eqn{E_{2ik}} are vectors of length \eqn{n_{ik}}. \code{Sigma} is the covariance matrix of \eqn{(E_{1ik},E_{2ik})} (for simplicity, \code{Sigma} is the same across sessions and subjects). For coefficients \eqn{A_{ik}}, \eqn{B_{ik}} and \eqn{C_{ik}}, we assume a mixed effects model. The random effects are from a trivariate normal distribution with mean zero and covariance \code{Psi}; and the model errors are from a trivariate normal distribution with mean zero and covariance \code{Lambda}. For the fixed effects \eqn{A}, \eqn{B} and \eqn{C}, the values are specified in \code{Theta} with \code{Theta[1,1] = A}, \code{Theta[1,2] = C} and \code{Theta[2,2] = B}.

  When \code{K = 1} (two-level data), the single-level model coefficients \eqn{A_{i}}, \eqn{B_{i}} and \eqn{C_{i}} are assumed to follow a trivariate linear regression model, where the population level coefficients are specified in \code{Theta} and the model errors are from a trivariate normal distribution with mean zero and covariance \code{Lambda}. 

  See Section 5.2 of the reference for details.
}
\value{
    \item{data}{a list of data. When \code{K = 1}, each list is a contains a dataframe; when \code{K > 1}, each list is a list of length \code{K}, and within each list is a dataframe.}
    \item{A}{the value of \eqn{A}s. When \code{K = 1}, it is a vector of length \code{N}; when \code{K > 1}, it is a \code{N} by \code{K} matrix.}
    \item{B}{the value of \eqn{B}s. When \code{K = 1}, it is a vector of length \code{N}; when \code{K > 1}, it is a \code{N} by \code{K} matrix.}
    \item{C}{the value of \eqn{C}s. When \code{K = 1}, it is a vector of length \code{N}; when \code{K > 1}, it is a \code{N} by \code{K} matrix.}
    \item{type}{a character indicates the type of the dataset. When \code{K = 1}, \code{type = twolevel}; when \code{K > 1}, \code{type = multilevel}}
}
\references{Zhao, Y., & Luo, X. (2014). \emph{Estimating Mediation Effects under Correlated Errors with an Application to fMRI.} arXiv preprint arXiv:1410.7217.
}
\author{ Yi Zhao, Brown University, \email{yi_zhao@brown.edu};
  Xi Luo, Brown University, \email{xi.rossi.luo@gmail.com}
}
\examples{
   ###################################################
    # Generate a two-level dataset

    # covariance matrix of errors
    delta<-0.5
    Sigma<-matrix(c(1,2*delta,2*delta,4),2,2)

    # model coefficients
    A0<-0.5
    B0<--1
    C0<-0.5

    Theta<-matrix(c(A0,0,C0,B0),2,2)

    # number of subjects, and trials of each
    set.seed(2000)
    N<-50
    K<-1
    n<-matrix(NA,N,K)
    for(i in 1:N)
    {
      n0<-rpois(1,100)
      n[i,]<-rpois(K,n0)
    }

    # treatment assignment list
    set.seed(100000)
    Z.list<-list()
    for(i in 1:N)
    {
      Z.list[[i]]<-rbinom(n[i,1],size=1,prob=0.5)
    }

    # Lambda
    rho.AB=rho.AC=rho.BC<-0
    Lambda<-matrix(0,3,3)
    lambda2.alpha=lambda2.beta=lambda2.gamma<-0.5
    Lambda[1,2]=Lambda[2,1]<-rho.AB*sqrt(lambda2.alpha*lambda2.beta)
    Lambda[1,3]=Lambda[3,1]<-rho.AC*sqrt(lambda2.alpha*lambda2.gamma)
    Lambda[2,3]=Lambda[3,2]<-rho.BC*sqrt(lambda2.beta*lambda2.gamma)
    diag(Lambda)<-c(lambda2.alpha,lambda2.beta,lambda2.gamma)

    # Data
    set.seed(5000)
    re.dat<-sim.data.multi(Z.list=Z.list,N=N,K=K,Theta=Theta,Sigma=Sigma,Lambda=Lambda)
    data2<-re.dat$data
    ###################################################

    ###################################################
    # Generate a three-level dataset

    # covariance matrix of errors
    delta<-0.5
    Sigma<-matrix(c(1,2*delta,2*delta,4),2,2)

    # model coefficients
    A0<-0.5
    B0<--1
    C0<-0.5

    Theta<-matrix(c(A0,0,C0,B0),2,2)

    # number of subjects, and trials of each
    set.seed(2000)
    N<-50
    K<-4
    n<-matrix(NA,N,K)
    for(i in 1:N)
    {
      n0<-rpois(1,100)
      n[i,]<-rpois(K,n0)
    }

    # treatment assignment list
    set.seed(100000)
    Z.list<-list()
    for(i in 1:N)
    {
      Z.list[[i]]<-list()
      for(j in 1:K)
      {
        Z.list[[i]][[j]]<-rbinom(n[i,j],size=1,prob=0.5)
      }
    }

    # Psi and Lambda
    sigma2.alpha=sigma2.beta=sigma2.gamma<-0.5
    theta2.alpha=theta2.beta=theta2.gamma<-0.5
    Psi<-diag(c(sigma2.alpha,sigma2.beta,sigma2.gamma))
    Lambda<-diag(c(theta2.alpha,theta2.beta,theta2.gamma))

    # Data
    set.seed(5000)
    re.dat<-sim.data.multi(Z.list,N,K,Theta,Sigma,Psi,Lambda)
    data3<-re.dat$data
    ###################################################
}
\keyword{ datagen }
