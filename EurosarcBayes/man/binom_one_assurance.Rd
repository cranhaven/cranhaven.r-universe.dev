\name{binom_one_assurance}
\alias{binom_one_assurance}
\alias{plot_binomassurance}
\docType{methods}
\title{Single arm, assurance calculator for single or multi-stage binomial trials.}
\description{
Computes the assurance of a given trial design given a prior assurance distribution.
}
\usage{
binom_one_assurance(failure, success, n, ass.dist,
 type="continuous", lower=0, upper=1, ...)

plot_binomassurance(failure, success, n, ass.dist,type="continuous",
	ndivisions=1000, xlim=c(0,1), xaxs="i", yaxs="i", ylim=NULL,
	main="Assurance distribution", col="red", col.fill="green", lwd=2,
	xlab="Probability of successful treatment",
	ylab="Prior assurance probability" ,...)
}
\arguments{
\item{failure}{A vector of the number of failures required to stop for futility, if not able to stop NA or a character string should be provided}
\item{success}{A vector of the number of successes required to stop for efficacy, if not able to stop NA or a character string should be provided}
\item{n}{A vector of the total number of patients to recruit up to each stage of the trial}
\item{ass.dist}{Distribution of prior probability for assurance. May be different to prior information.}
\item{type}{Tells the program you are passing it a continuous distribution ("continuous") or a discrete distribution ("discrete") for the assurance distribution}
\item{ndivisions}{The number of points calculated for the plot}
\item{lower, upper}{Range of the distribution to use}
\item{col.fill}{Colour of the true positive results in the graph}
\item{xlim, xaxs, yaxs, ylim, main, col, lwd, xlab, ylab}{Different defaults for plotting parameters}
\item{...}{Additional plotting parameters to pass to plot function}
}
\seealso{
\code{\link{binom_one_power}}, \code{\link{binom_one_alpha}}
}
\examples{
# Simon's two stage design
failure=c(0,3)
success=c(NA,4)
n=c(7,18)
p0=0.1
p1=0.3

# continuous assurance distribution
ass.dist = function(p) dbeta(p,4,18)

# assurance
binom_one_assurance(failure,success,n,ass.dist)

# plot
plot_binomassurance(failure,success,n,ass.dist,ndivisions=1000)

# discrete assurance distribution
ass.dist = matrix(c(0.2,0.3,0.4,0.3,0.4,0.3),ncol=2)

# assurance
binom_one_assurance(failure,success,n,ass.dist,type="discrete")

# plot
plot_binomassurance(failure,success,n,ass.dist,type="discrete",
                    ndivisions=1000)
}