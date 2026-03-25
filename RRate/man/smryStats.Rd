\name{smryStats}
\alias{smryStats1}
\alias{smryStats2}
\alias{param}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Example summary statistics
}
\description{
Example summary statistics from the primary and replication studies in GWAS. These summary statistics are generated from a simulated study. The primary study (\code{smryStats1}) has 2000 cases and 2000 controls. The replication study (\code{smryStats2}) has 1000 cases and 1000 controls. The SNP number is 10,000. The disease pravalence is 1\%. Minor allele frequencies are from U(0.05, 0.5), and log-odds ratio are from 0.95N(0,1)+0.05N(0, 0.04). The ground-truth parameters for each SNP are in \code{param}.
}
\usage{
data(smryStats1)

data(smryStats2)

data(param)
}
\details{
The data sets are described as 

SNP: SNP ID

F_A: Allele frequency in case group

F_U: Allele frequency in control group

Z: Z value from log-odds ratio test

P: P value

OR: Odds ratio

SE: Standard error of log(OR)

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
\code{\link[RRate:repSampleSizeRR]{repSampleSizeRR}},
\code{\link[RRate:repSampleSizeRR2]{repSampleSizeRR2}},
\code{\link[RRate:HLtest]{HLtest}}
}
