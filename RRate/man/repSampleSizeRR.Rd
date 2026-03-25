\name{repSampleSizeRR}
\alias{repSampleSizeRR}
\alias{repSampleSizeRR2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sample size determination for the replication study based on RR
}
\description{
\code{repSampleSizeRR} and \code{repSampleSizeRR2} implement the RR-based sample size determination method for the replication study. If the replication study has the same control-to-case ratio with the primary study, then \code{repSampleSizeRR} can be used. Otherwise, \code{repSampleSize2} is more suitable.
}
\usage{
repSampleSizeRR(GRR, n, MUhat, SE, zalpha2, zalphaR2, idx = TRUE)

repSampleSizeRR2(GRR,CCR2, MUhat,SE,fU,fA,zalpha2,zalphaR2, idx=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GRR}{
%%     ~~Describe \code{RR} here~~
The desired global replication rate.
}
  \item{n}{
Sample size in the primary study.
}
  \item{MUhat}{
The observed effect size (log-odds ratio).
}
  \item{SE}{
The standard error of MUhat.
}
  \item{zalpha2}{
The critical value of z-values in the primary study, i.e. z_alpha/2.
}
  \item{zalphaR2}{
The critical value of z-values in the replication study, i.e. z_alphaR/2.
}
  \item{idx}{
The indexes of the SNPs having been further inverstigated in the replication study. We only calculate RR for primary associations with indexes in \code{idx}.
}
  \item{CCR2}{
The control-to-case ratio of the replication study.
}
  \item{fU}{
The allele frequency in the control group.
}
 \item{fA}{
The allele frequency in the case group.
}
}
\value{
The determined sample size of the replication study is returned.
}
\references{
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
\code{\link[RRate:HLtest]{HLtest}}
}
\examples{
alpha<-5e-6               #Significance level in the primary study
alphaR<-5e-3              #Significance level in the replication study
zalpha2<-qnorm(1-alpha/2)
zalphaR2<-qnorm(1-alphaR/2)

##Load data
data('smryStats1')        #Example of summary statistics in 1st study
#### Sample size determination ###
n1<-4000                  #Sample size of the primary study
n2_1<-repSampleSizeRR(0.8, n1, log(smryStats1$OR),smryStats1$SE,zalpha2,zalphaR2)

CCR2<-2                   #Control-to-case ration in the replication study
n2_2<-repSampleSizeRR2(0.8, CCR2, log(smryStats1$OR),smryStats1$SE,smryStats1$F_U, 
smryStats1$F_A,zalpha2,zalphaR2)
}
