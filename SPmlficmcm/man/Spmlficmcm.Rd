\name{Spmlficmcm}
\alias{Spmlficmcm}
\title{
Semiparametric maximum likelihood for interaction in case-mother control-mother
}
\description{
The function builds the nonlinear system from the data, solves the system and assesses the effect of each factor of the model, computes the variance - covariance matrix and deduces from it the standard deviations of each factor.
}
\usage{
Spmlficmcm(fl, N, gmname, gcname, DatfE, typ, start, p=NULL)
}
\arguments{
  \item{fl}{Model formula.
}
  \item{N}{Numeric vector containing eligible number cases and controls in the study population N=(N0, N1).
}
  \item{gmname}{Name of mother genotype variable.
}
  \item{gcname}{Name of offspring genotype variable.
}
\item{DatfE}{\code{data.frame} in long format containing the following variables:\emph{outcome} variable, \emph{mother genotype}, \emph{offspring genotype} and \emph{environmental factors}.
}
\item{typ}{Argument indicating whether the data are complete (1) or contain missing offspring genotypes (2).
}
\item{start}{Vector of the initial values of the model parameters.
}
\item{p}{Disease prevalence 
}
}
\details{
The function \code{Spmlficmcm} builds the nonlinear system from the data and solves the nonlinear system. Then, it uses the log profile likelihood function and the one-step method to estimate the parameters of each factor of the model formula and their standard errors. The programme computes the gradient of the profile likelihood using the analytical formula and the Hessian matrix numerically from the gradient. The genotype is coded as the number of minor alleles. The model supposes that the distribution of maternal genotype and offspring genotype satisfy the following assumptions: \emph{random mating}, \emph{Hardy-Weinberg equilibrium} and \emph{Mendelian inheritance}. When the data contains missing offspring genotypes, the profile likelihood is summed over the possible genotypes of each child whose genotype is missing. The argument \code{typ} allows the user to specify whether the data is complete or not. Argument \code{start} permits to the user to give the initials values of model parameter. 
Ex: in the following equation log(P/(1-P))=B0+B1*X1+B2*X2+Bm*Gm+Bc*Gc+B2m*X2:Gm, \code{start}=(B0, B1, B2, Bm, Bc, B2m, fp) where fp is the log of the odds of the minor allelic frequency. However, if the user provides no values, the function uses logistic regression to compute the initial B=(B0, B1, B2, Bm, Bc, B2m) and takes 0.1 as the initial value of fp. If the argument \code{N} is unavailable, it is possible to specify the disease population prevalence in the argument \code{p} instead of \code{N}. In that casse, \code{N1} is set equal to 5 n1, in order to avoid observing N1<n1 when prevalence is small. We then set N0=[(1-p)/p]*N1. 
}
\value{
 A list containing components
\item{\code{Uim}}{Nonlinear system solution}
\item{\code{MatR}}{Matrix containing the estimates and their standard errors}
\item{\code{Matv}}{Variance - covariance matrix}
\item{\code{Lhft}}{Log-likelihood function. It takes as argument a vector of the model parameters}
\item{\code{Value_loglikh}}{Value of the Log-likelihood function computed at the parameters estimated}
}
\references{
Jinbo Chen, Dongyu Lin and Hagit Hochner (2012) Semiparametric Maximum Likelihood Methods for Analyzing Genetic and Environmental Effects with Case-Control Mother-Child Pair Data. \emph{Biometrics} DOI: 10.1111/j.1541-0420.2011.01728.

Moliere Nguile-Makao, Alexandre Bureau (2015), Semi-Parametric Maximum likelihood Method for interaction in Case-Mother Control-Mother designs: Package SPmlficmcm. \emph{Journal of Statistical Software DOI: 10.18637/jss.v068.i10}.
}

\examples{
# 1-Creation of database
\dontrun{
  set.seed(13200)
  M=20000;
  fl=outc~X1+X2+gm+gnch+X1:gnch+X2:gm;
  theta=0.3
  beta=c(-0.916,0.857,0.588,0.405,-0.693,0.488)
  interc=-2.23
  vpo=c(3,4)
  vprob=c(0.35,0.55)
  vcorr=c(2,1)
  Dataf<-FtSmlrmCMCM(fl,M,theta,beta,interc,vpo,vprob,vcorr)
  rho<-table(Dataf$outc)[2]/20000 # Disease prevalence
         
  # Number of subjects eligible to the study in the population 
  N=c(dim(Dataf[Dataf$outc==0,])[1],dim(Dataf[Dataf$outc==1,])[1])
         
  # Sampling of the study database  
  n0=1232;n1=327; 
  DatfE1<-SeltcEch("outc",n1,n0,"obs",Dataf)


# 2 Creation of missing data on the offspring genotype 
        DatfE=DatfE1 
        gnch<-DatfE["gnch"]
        gnch<-as.vector(gnch[,1])
        gnch1<-sample(c(0,1),length(gnch),replace=TRUE,prob=c(0.91,0.09))
        gnch[gnch1==1]<-NA
        DatfE=DatfE1
        DatfE$gnch<-NULL;DatfE$gnch<-gnch
# 3 Creation of the two databases 
      # DatfEcd :complete data
      # DatfEmd :data with missing genotypes for a subset of children.
        DatfEcd<-DatfE[is.na(DatfE["gnch"])!=TRUE,]
        DatfEmd<-DatfE
        rm(gnch);rm(gnch1) 
# data obtained
DatfEcd[26:30,]
DatfEmd[26:30,]

##4 Estimation of parameters=======================================================
## model equation         
fl=outc~X1+X2+gm+gnch+X1:gnch+X2:gm;
## Estimation of the parameters (no missing data)
        # N = (N0,N1) is available
        Rsnm1<-Spmlficmcm(fl,N,"gm","gnch",DatfEcd,1)
        #solution of the nonlinear system
        round(Rsnm1$Uim,digits=3)
        #estimates
        round(Rsnm1$MatR,digits=3)
        #variance - covariance matrix
        round(Rsnm1$Matv,digits=5)
        # N = (N0,N1) is not available
        Rsnm2<-Spmlficmcm(fl=fl,gmname="gm",gcname="gnch",DatfE=DatfEcd,typ=1,p=rho)
        #solution of the nonlinear system
        round(Rsnm2$Uim,digits=3)
        #estimates
        round(Rsnm2$MatR,digits=3)
        #variance - covariance matrix
        round(Rsnm2$Matv,digits=5)
## Estimation of the parameters (with missing data)
        # N = (N0,N1) is available
        Rswm1<-Spmlficmcm(fl,N,"gm","gnch",DatfEmd,typ=2)
        #solution of the nonlinear system
        round(Rswm1$Uim,digits=3)
        #estimates
        round(Rswm1$MatR,digits=3)
        #variance - covariance matrix
        round(Rswm1$Matv,digits=5)
        # N = (N0,N1) is not available
        Rswm2<-Spmlficmcm(fl=fl,gmname="gm",gcname="gnch",DatfE=DatfEmd,typ=2,p=rho)
        #solution of the nonlinear system
        round(Rswm2$Uim,digits=3)
        #estimates
        round(Rswm2$MatR,digits=3)
        #variance - covariance matrix
        round(Rswm2$Matv,digits=5)
}
}
\keyword{semiparametric}
\keyword{maximum likelihood}
\keyword{case-mother}