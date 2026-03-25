\name{HLtest}
\alias{HLtest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hosmer-Lemeshow test
}
\description{
Test whether each element of x is sampled with the probability specified by the corrsponding element in p.
}
\usage{
HLtest(x, p, g = 10, null = "all", boot = 1000, info = T, dir = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A boolean vector.
}
  \item{p}{
A probability vector having the same length with x.  
}
  \item{g}{
The group number used in the test.
}
  \item{null}{
a character in c('all', 'chi2','boot'). 
If null=='chi2', then we use (g-1) degree of freedom chi2 distribution to approximately compute p value.
If null=='boot', then we use parametric bootstrap to compute p value.
If null=='all', then both methods are used. This is the default option.
}
  \item{boot}{
%%     ~~Describe \code{boot} here~~
The resampling times to compute p value. Only effective when null=='boot' or 'all'
}
  \item{info}{
Draw the null distribution of the test statistic.
}
  \item{dir}{
The directory to save the plot of the null distribution. 
}
}
\details{
Null Hypothesis: Each element of x is sampled with a probability which is the corresponding element of p.
We group x to g groups according to p. Then we compare the success proportion with the mean value of p in each group. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list is returned:
\item{H}{
The test statistic.
}
\item{pval_chi2}{
The p value approximated by using chi2 distribution.
}
\item{pval_boot}{
The p value computed by using parametric bootstrap.
}
}
\references{
Hosmer, D. W., & Lemesbow, S. (1980). Goodness of fit tests for the multiple logistic regression model. \emph{Communications in statistics-Theory and Methods}, 9(10), 1043-1069.

Jiang, W., Xue, J-H, and Yu, W.  What is the probability of replicating a statistically significant association in genome-wide association studies?. \emph{Submitted}.
}
\author{
\packageAuthor{RRate}

Maintainer: \packageMaintainer{RRate}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[RRate:RRate]{RRate}}
\code{\link[RRate:repRateEst]{repRateEst}}, 
\code{\link[RRate:SEest]{SEest}},
\code{\link[RRate:repSampleSizeRR]{repSampleSizeRR}},
\code{\link[RRate:repSampleSizeRR2]{repSampleSizeRR2}},
}
\examples{
alpha<-5e-6               #Significance level in the primary study
alphaR<-5e-3              #Significance level in the replication study
zalpha2<-qnorm(1-alpha/2)
zalphaR2<-qnorm(1-alphaR/2)

##Load data
data('smryStats1')        #Example of summary statistics in 1st study
n2.0<-2000                #Number of individuals in control group
n2.1<-2000                #Number of individuals in case group

SE2<-SEest(n2.0, n2.1, smryStats1$F_U, smryStats1$F_A) #SE in replication study
######  RR estimation  ######
RRresult<-repRateEst(log(smryStats1$OR),smryStats1$SE, SE2,zalpha2,zalphaR2, output=TRUE,dir='.')

#### Hosmer-Lemeshow test  ####
data('smryStats2')        #Example of summary statistics in 2nd study
sigIdx<-(smryStats1$P<alpha)
repIdx<-(sign(smryStats1$Z[sigIdx])*smryStats2$Z[sigIdx]>zalphaR2)
groupNum<-10 
HLresult<-HLtest(repIdx,RRresult$RR,g=groupNum,dir='.')
}
