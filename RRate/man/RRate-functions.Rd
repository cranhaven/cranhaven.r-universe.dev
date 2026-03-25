\name{RRate-functions}
\alias{RRate-functions}
\alias{repRateEst}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimating Replication Rate for primary associations
}
\description{
\code{repRateEst} implements a replication rate estimation method. Two-component mixture prior is used in the estimation. 
}
\usage{
repRateEst(MUhat, SE, SE2, zalpha2, zalphaR2, boot = 100, output = TRUE, 
idx = TRUE, dir = "output", info = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MUhat}{
The observed effect size (log-odds ratio) in the primary study.
}
  \item{SE}{
The standard error of the observed log-odds ratio in the primary study.
}
  \item{SE2}{
The standard error of the observed log-odds ratio in the replication study.
}
  \item{zalpha2}{
The critical value of z-values in the primary study, i.e. z_alpha/2.
}
  \item{zalphaR2}{
The critical value of z-values in the replication study, i.e. z_alphaR/2.
}
  \item{boot}{
The resampling number of bootstrop used for estimating the credible interval of
the RR.
}
  \item{output}{
Bool value.  To determine whether to output the estimated results in the
dir or not.
}
  \item{idx}{
The indexes of the SNPs having been further inverstigated in the replication study. We only calculate RR for primary associations with indexes in \code{idx}.
}
  \item{dir}{
The directory to save the estimated results. It has effect when \code{output=T}
}
  \item{info}{
Bool value. To determine whether to show the parematers inference results in the terminal or not.
}
}
\details{
The RR estimation is based on the following two-component mixture model:
mu=pi_0 delta_0+(1-pi_0) N(0, sigma_0^2).

Details can be seen the following reference paper.
}
\value{
\code{repRateEst} returns the RR, lfdr, prediction power and infered parameters. The returened value is a LIST:
\item{idx}{
The index of the SNPs which RR are estimated.
}
\item{pi0}{
The proportion of nonassociated SNPs.
}
\item{sigma02}{
The variance of the associated SNPs' effect sizes 
}
\item{RR}{
Estimated replication rate.
}
\item{RRlow}{
The lower limit of the 95\% CI for RR. 
}
\item{RRhigh}{
The upper limit of the 95\% CI for RR.
}
\item{lfdr}{
Estimated local false discovery rate of the primary study
}
\item{lfdrLow}{
The lower limit of the 95\% CI for lfdr. 
}
\item{lfdrHigh}{
The upper limit of the 95\% CI for lfdr. 
}
\item{predPower}{
The Bayesian predictive power of the replication study.
}
\item{predPowerLow}{
The lower limit of the 95\% CI for predPower. 
}
\item{perdPowerHigh}{
The upper limit of the 95\% CI for predPower. 
}
\item{GRR}{
The Global Replication Rate (Mean value of RR)
}
\item{GRRlow}{
The lower limit of the 95\% CI for GRR. 
}
\item{GRRhigh}{
The upper limit of the 95\% CI for GRR. 
}
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
\code{\link[RRate:RRate]{RRate}}, 
\code{\link[RRate:SEest]{SEest}},
\code{\link[RRate:repSampleSizeRR]{repSampleSizeRR}},
\code{\link[RRate:repSampleSizeRR2]{repSampleSizeRR2}},
\code{\link[RRate:HLtest]{HLtest}}
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
RR<-RRresult$RR           #Estimated RR
}
