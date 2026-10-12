\name{butstrap}
\alias{butstrap}
\title{Bootstrapping AUC}
\description{Boostrap the AUC for significance testing and confidence
interval calculation}
\usage{
butstrap(X,etime,status,u=NULL,tt,s,vtimes,auc1,B=50,fc=NULL)
}
\arguments{
  \item{X}{n by S matrix of longitudinal score/biomarker for i-th
  subject at j-th occasion (NA if unmeasured)}
   \item{etime}{n vector with follow-up times}
   \item{status}{n vector with event indicators}
   \item{u}{Lower limit for evaluation of sensitivity and
   specificity. Defaults to \code{vtimes[s]} (see below)}
   \item{tt}{Upper limit (time-horizon) for evaluation of sensitivity
and specificity.}
\item{s}{Scalar number of measurements/visits to use for each subject. s<=S}
\item{vtimes}{S vector with visit times}
\item{auc1}{AUC for the original data set}
\item{B}{Number of bootstrap replicates. Defaults to \code{50}}
\item{fc}{Events are defined as fc = 1. Defaults to $I(cup X(t_j)>cutoff)$}
}

\details{
This function can be used to resample the AUC. The resulting p-value
is obtained after assumption that the resampled AUC is
Gaussian. Non-parametric confidence interval is obtained as the 2.5%
and 97.5% quantile of the resampled statistic, while parametric
confidence interval is simply given by a Gaussian approximation. 
}
\value{
  A list with the following elements: 
	\tabular{ll}{
 	\code{p.value} \tab (Parametric) p-value for H0: AUC=0.5 \cr
	\code{se} \tab Standard deviation of the AUC replicates  \cr
	\code{ci.np} \tab Non-parametric 95\% confidence interval for AUC  \cr
	\code{ci.par} \tab Parametric 95\% confidence interval for AUC  \cr
	}
}
\references{
Barbati, G. and Farcomeni, A. (2017) Prognostic assessment of
repeatedly measured time-dependent biomarkers, with application to
dilated cardiomuopathy, \emph{Statistical Methods \& Applications}, in
press 
}
\author{Alessio Farcomeni \email{alessio.farcomeni@uniroma1.it}}
\seealso{
\cite{\link{roc}}, \cite{\link{auc}}, \cite{\link{maxauc}}
}
\examples{
# parameters
n=100
tt=3
Tmax=10
u=1.5
s=2
vtimes=c(0,1,2,5)

# generate data 

ngrid=5000
ts=seq(0,Tmax,length=ngrid)
X2=matrix(rnorm(n*ngrid,0,0.1),n,ngrid)
for(i in 1:n) {
sa=sample(ngrid/6,1)
vals=sample(3,1)-1
X2[i,1:sa[1]]=vals[1]+X2[i,1:sa[1]]
X2[i,(sa[1]+1):ngrid]=vals[1]+sample(c(-2,2),1)+X2[i,(sa[1]+1):ngrid]
}

S1=matrix(sample(4,n,replace=TRUE),n,length(vtimes))
S2=matrix(NA,n,length(vtimes))

S2[,1]=X2[,1]

for(j in 2:length(vtimes)) {
tm=which.min(abs(ts-vtimes[j]))
S2[,j]=X2[,tm]}

cens=runif(n)
ripart=1-exp(-0.01*apply(exp(X2),1,cumsum)*ts/1:ngrid)

Ti=rep(NA,n)
for(i in 1:n) {
Ti[i]=ts[which.min(abs(ripart[,i]-cens[i]))]
}

cens=runif(n,0,Tmax*2)
delta=ifelse(cens>Ti,1,0)
Ti[cens<Ti]=cens[cens<Ti]

## an unimportant marker 

ro=roc(S1,Ti,delta,u,tt,s,vtimes) 
but=butstrap(S1,Ti,delta,u,tt,s,vtimes,ro)

}


