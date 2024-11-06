\name{binom_one_alpha}
\alias{binom_one_alpha}
\docType{methods}
\title{Single arm, exact p-value calculator for single or multi-stage binomial trials.}
\description{
P-value (alpha) for single arm binomial clinical trials. This is done exactly accounting for all interim analysis prior to stopping the trial.
}
\usage{
binom_one_alpha(result.success, result.n, p0, failure, success, n)
}
\arguments{
\item{result.success}{total successes at the end of the trial}
\item{result.n}{total patients at the end of the trial}
\item{p0}{Probability of success under H0}
\item{failure}{A vector of the number of failures required to stop for futility, if not able to stop NA or a character string should be provided}
\item{success}{A vector of the number of successes required to stop for efficacy, if not able to stop NA or a character string should be provided}
\item{n}{A vector of the total number of patients to recruit up to each stage of the trial}
}
\seealso{
\code{\link{binom_one_power}}, \code{\link{binom_one_assurance}}
}
\examples{
# Simon's two stage design
failure=c(0,3)
success=c(NA,4)
n=c(7,18)
p0=0.1

result.success=4
result.n=18

# without accounting for interim analysis when calculating 
# the p-value
1-pbinom(result.success-1,result.n,p0)
# account for interim analysis
binom_one_alpha(result.success,result.n,p0,failure,success,n)
}