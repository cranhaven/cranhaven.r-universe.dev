\name{roc}
\alias{roc}
\title{ROC curve}
\description{Compute ROC curve}
\usage{
roc(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL)
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
\item{fc}{Events are defined as fc = 1. Defaults to $I(cup X(t_j)>cutoff)$}
}

\details{
ROC curve is defined as the curve given by (1-specificities,
sensitivities). Here these are obtained for a time-dependent multiply-measured
marker are defined as 

Se(t,c,s,u) = Pr(f_c(X(t_1),X(t_2),...,X(t_{s_i}))| u <= T <= t),

and

Sp(t,c,s,u) = 1-Pr(f_c(X(t_1),X(t_2),...,X(t_{s_i})) | T > t) 

for some fixed f_c, where c is a cutoff. 
The default for f_c is that a positive diagnosis is given as soon as
any measurement among the s considered is above the threshold. 
}
\value{
  A matrix with the following columns: 
	\tabular{ll}{
 	\code{1-spec} \tab 1-Specificities \cr
	\code{sens} \tab Sensitivities  \cr
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
\cite{\link{auc}}, \cite{\link{butstrap}}, \cite{\link{maxauc}}
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

## 

## an important marker 

ro=roc(S2,Ti,delta,u,tt,s,vtimes) 
plot(ro,type="l",col="red")
abline(a=0,b=1)

## an unrelated marker 

ro=roc(S1,Ti,delta,u,tt,s,vtimes) 
plot(ro,type="l",col="red")
abline(a=0,b=1)

}


