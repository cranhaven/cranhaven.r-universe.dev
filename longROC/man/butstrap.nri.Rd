\name{butstrap.nri}
\alias{butstrap.nri}
\title{Bootstrapping NRI}
\description{Boostrap the AUC for significance testing and confidence
interval calculation}
\usage{
butstrap.nri(risk1,risk2,etime,status,u,tt,nri1,wh,B=1000)
}
\arguments{
  \item{risk1}{Baseline risk measurements}
  \item{risk2}{Enhanced risk measurements}
   \item{etime}{n vector with follow-up times}
   \item{status}{n vector with event indicators}
   \item{u}{Lower limit for evaluation of sensitivity and specificity}
   \item{tt}{Upper limit (time-horizon) for evaluation of sensitivity
and specificity.}
\item{nri1}{NRI for the original data set}
\item{wh}{Which NRI to boostrap? \code{wh=1} 1/2NRI, \code{wh=2} NRI
for events, \code{wh=3} NRI for non-events}
\item{B}{Number of bootstrap replicates. Defaults to \code{1000}}
}

\details{
This function can be used to resample the NRI. The resulting p-value
is obtained after assumption that the resampled NRI is
Gaussian. Non-parametric confidence interval is obtained as the 2.5%
and 97.5% quantile of the resampled statistic, while parametric
confidence interval is simply given by a Gaussian approximation. 
}
\value{
  A list with the following elements: 
	\tabular{ll}{
 	\code{p.value} \tab (Parametric) p-value for H0: NRI=0 \cr
	\code{se} \tab Standard deviation of the NRI replicates  \cr
	\code{ci.np} \tab Non-parametric 95\% confidence interval for NRI  \cr
	\code{ci.par} \tab Parametric 95\% confidence interval for NRI  \cr
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
\cite{\link{nri}}
}
\examples{
# parameters
n=25
tt=3
Tmax=10
u=1.5
s=2
vtimes=c(0,1,2,5)

# generate data 

ngrid=1000
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

risk1=apply(S1[,1:s],1,sum)
risk1=(risk1-min(risk1))/(max(risk1)-min(risk1))
risk2=apply(S2[,1:s],1,sum)
risk2=(risk2-min(risk2))/(max(risk2)-min(risk2))
butstrap.nri(risk1,risk2,Ti,delta,u,tt,nri(risk1,risk2,Ti,delta,u,tt)$nri,wh=1,B=500)


}


