\name{SEest}
\alias{SEest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimating standard error of the observed log-odds ratio
}
\description{
\code{SEest} implements the Woolf's method to estimate the standard error of the observed log-odds ratio. 
}
\usage{
SEest(n0,n1,fU,fA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n0}{
Sample size in the control group.
}
  \item{n1}{
Sample size in the case group.
}
  \item{fU}{
Allele frequency in the control group.
}
  \item{fA}{
Allele frequency in the case group. 
}
}
\details{
The Woolf's method to estimate the standard error of log(OR) is based on the following formula:

se(log(OR))=sqrt(1/(n0 fU(1-fU))+1/(n1 fA(1-fA))).
}
\value{
The estimated standard error is returned.
}
\references{
Woolf, B. (1955). On estimating the relation between blood group and disease. \emph{Ann Hum Genet}, 19(4), 251-253.

Jiang, W., Xue, J-H, and Yu, W.  What is the probability of replicating a statistically significant association in genome-wide association studies?. \emph{Submitted}.
}
\author{
\packageAuthor{RRate}

Maintainer: \packageMaintainer{RRate}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[RRate:RRate]{RRate}},
\code{\link[RRate:repRateEst]{repRateEst}}, 
\code{\link[RRate:repSampleSizeRR]{repSampleSizeRR}},
\code{\link[RRate:repSampleSizeRR2]{repSampleSizeRR2}},
\code{\link[RRate:HLtest]{HLtest}}
}
\examples{
##Load data
data('smryStats1')        #Example of summary statistics in 1st study
n2.0<-2000                #Number of individuals in control group
n2.1<-2000                #Number of individuals in case group

SE2<-SEest(n2.0, n2.1, smryStats1$F_U, smryStats1$F_A) #SE in replication study
}
