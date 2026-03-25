\name{RRate-package}
\alias{RRate-package}
\alias{RRate}
\docType{package}
\title{
\packageTitle{RRate}
}
\description{
\packageDescription{RRate}
}
\details{
\tabular{ll}{
Package: \tab RRate\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-06-13\cr
License: \tab GPL-3\cr
}

The goal of genome-wide association studies (GWAS) is to discover genetic variants associated with diseases/traits. Replication is a common validation method in GWAS. We regard an association as true finding when it shows significance in both the primary and replication studies. A worth pondering question is: what is the probability of a primary association (i.e. statistically significant association in the primary study) being validated in the replication study?

We refer the Bayesian replication probability as the replication rate (RR). Here we implement the estimation method for RR which makes use of the summary statistics from the primary study. We can use the estimated RR to determine the sample size of the replication study, and to check the consistency between the results of the primary study and those of the replication study.

The principal component of RRate package is \code{repRateEst}. Also we implement sample size determination method (\code{repSampleSizeRR} and \code{repSampleSizeRR2}) and consistency checking method (Hosmer-Lemeshow test, \code{HLtest}).

1. To estimate the RR, we need obtain the summary statistics of each genotyped SNPs in the primary study. We have put a example summary statistics (smryStats1) in the package. You can use \code{data(smryStats1)} to load the example data. You can also obtain the ground-truth parameters (allele frequencies, odds ratios) of the example data using \code{data(param)}. We also put the corresponding summary statistics of the replicaition study in the package (smryStats2). 

2. You can use \code{SEest} to estimate the standard error of the observed log-odds ratio.

\code{SEest(n0,n1,fU,fA)}

Details about the function can be seen using \code{\link[RRate:SEest]{help(SEest)}}.

3. You can use \code{repRateEst} to estimate the RR for each associations discovered from the primary study (i.e. primary associations). 

\code{repRateEst(MUhat,SE, SE2,zalpha2,zalphaR2,boot=100,output=TRUE,idx=TRUE,dir='output',info=T)}

Details about the function can be seen using \code{\link[RRate:repRateEst]{help(repRateEst)}}.

4. You can use \code{repSampleSizeRR} and \code{repSampleSizeRR2} to determine the sample size of the replication study. 

\code{repSampleSizeRR(RR, n, MUhat,SE,zalpha2,zalphaR2,idx=TRUE)}

\code{repSampleSizeRR2(RR,CCR2, MUhat,SE,fU,fA,zalpha2,zalphaR2, idx=TRUE)}

Details about these functions can be seen using \code{\link[RRate:repSampleSizeRR]{help(repSampleSizeRR)}} and \code{\link[RRate:repSampleSizeRR2]{help(repSampleSizeRR2)}}.

5. You can use \code{HLtest} to check the consistency between the results of the primary study and those of the replication study.  

\code{HLtest(x,p,g=10,null='all',boot=1000,info=T,dir='.')}

Details about the function can be seen using \code{\link[RRate:HLtest]{help(HLtest)}}
}
\author{
\packageAuthor{RRate}

Maintainer: \packageMaintainer{RRate}
}
\references{
Jiang, W., Xue, J-H, and Yu, W.  What is the probability of replicating a statistically significant association in genome-wide association studies?. \emph{Submitted}.
}

\keyword{ package }
\seealso{
\code{\link[RRate:repRateEst]{repRateEst}}, 
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
z1<-smryStats1$Z          #Z values in 1st study
n2.0<-2000                #Number of individuals in control group
n2.1<-2000                #Number of individuals in case group

SE2<-SEest(n2.0, n2.1, smryStats1$F_U, smryStats1$F_A) #SE in replication study
######  RR estimation  ######
RRresult<-repRateEst(log(smryStats1$OR),smryStats1$SE, SE2,zalpha2,zalphaR2, output=TRUE,dir='.')
RR<-RRresult$RR           #Estimated RR
}
