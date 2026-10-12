\name{maxauc}
\alias{maxauc}
\title{Optimal Score}
\description{Compute optimal score for AUC}
\usage{
maxauc(X,etime,status,u=NULL,tt,s,vtimes,fc=NULL)
}
\arguments{
  \item{X}{p by n by S array of longitudinal scores/biomarkers for i-th
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
This function can be used to find an optimal linear combination of p
scores/biomarkers repeatedly measured over time. The resulting score
is optimal as it maximizes the AUC among all possible linear
combinations. The first biomarker in array X plays a special role, as
by default its coefficient is unitary. 
}
\value{
  A list with the following elements: 
	\tabular{ll}{
 	\code{beta} \tab Beta coefficients for the optimal score \cr
	\code{score} \tab Optimal score \cr
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
n=25
tt=3
Tmax=10
u=1.5
s=2
vtimes=c(0,1,2,5)

# generate data 

ngrid=500
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

X=array(NA,c(2,nrow(S1),ncol(S1)))
X[1,,]=round(S2) #fewer different values, quicker computation 
X[2,,]=S1

sc=maxauc(X,Ti,delta,u,tt,s,vtimes)

# beta coefficients

sc$beta

# final score (X[1,,]+X[2,,]*sc$beta[1]+...+X[p,,]*sc$beta[p-1])

sc$score

}


