\name{FtSmlrmCMCM}
\alias{FtSmlrmCMCM}
\title{
Generates the logistic model data 
}
\description{
The function generates data from a logistic regression model. The data obtained contain: an outcome variable, the mother and child genotype coded as the number of minor allele and the environmental factors. For simulation of each environmental variable, the user can specify the coefficients of linear dependency between the mother genotype and the environmental factors.
}
\usage{
FtSmlrmCMCM(fl, N, theta, beta, interc, vpo, vprob, vcorr)
}
\arguments{
  \item{fl}{Model formula.
}
  \item{N}{Sample size.
}
  \item{theta}{Minor allele frequency.
}
  \item{beta}{Parameter vector of the effects.
}
  \item{interc}{Intercept of the model.
}
  \item{vpo}{Numeric vector containing the positions of the terms corresponding to the mother and child genotypes in the left-hand side of the formula.
}
\item{vprob}{Numeric vector containing the prevalence (success probability) of each environmental factor.
}
\item{vcorr}{Numeric vector containing the coefficients of linear dependency between the mother genotype and environmental factors. The value 0 corresponds to independence.
}
}
\details{
The function generates data, where the outcome variable is associated with the explanatory variables by a logistic regression model. 

Ex: log(P/(1-P))=B0+B1*X1+B2*X2+Bm*Gm+Bc*Gc+B2m*X2:Gm.

Where P=Pr(Y=1|X), X=(X1,X2) and Y is the outcome variable. The environmental factors are generated the following way: for each variable, a temporary variable is generated with a binomial law of success probability equal to vprob[i] plus vcorr[i]*Gm, i is the factor position. The genotypes of the mother and her child are coded as the number of minor alleles, i.e. under an additive model of the alleles on the log odds. The data generated suppose that the assumptions of \emph{Hardy-Weinberg equilibrium}, \emph{random mating type} and \emph{Mendelian inheritance} are satisfied. The function uses the formula f(x)=1/(1+exp(-x)) to generated the outcome variable. The data.frame returned by the function contains the variables whose names correspond to terms labels of the formula. The particularity of this function is to generate the genotype of a mother and her child taking into account the parental link.
}
\value{
The function returns a \code{data.frame} containing an outcome variable, the environmental factors and two genotypes of the mother and her child.
}

\examples{
# 1-Creation of database
  set.seed(13200)
   M=5000
   fl=outc~X1+X2+gm+gc+X2:gm
   vpo=c(3,4)
   vprob=c(0.35,0.55)
   vcorr=c(2,1)
   theta=0.3;
   beta=c(-0.916,0.857,0.405,-0.693,0.573)
   interc=-2.23
   Dataf<-FtSmlrmCMCM(fl,M,theta,beta,interc,vpo,vprob,vcorr)
   Dataf[1:10,]
}
\keyword{Simulation}