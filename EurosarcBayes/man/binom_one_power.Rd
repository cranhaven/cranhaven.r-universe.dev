\name{binom_one_power}
\alias{binom_one_power}
\alias{plot_binom_one_power}
\docType{methods}
\title{Single arm, power calculator for single or multi-stage binomial trials.}
\description{
Computes the power of a given trial design given the probability of success of treatment p.
}
\usage{
binom_one_power(p,failure,success,n)

plot_binom_one_power(failure, success, n, ndivisions=1000, 
	xlim=c(0,1), xaxs="i", yaxs="i", ylim=c(0,1.1),
	main="Power curve for a single arm binomial trial design",
	xlab="Probability of successful treatment",
	ylab="Probability of successful trial", 
	p=NULL, alpha=NULL, power=NULL, col.error="red", \dots)
}
\arguments{
\item{p}{Probability of success to compute power for}
\item{failure}{A vector of the number of failures required to stop for futility, if not able to stop NA or a character string should be provided}
\item{success}{A vector of the number of successes required to stop for efficacy, if not able to stop NA or a character string should be provided}
\item{n}{A vector of the total number of patients to recruit up to each stage of the trial}
\item{ndivisions}{The number of points calculated for the plot}
\item{col.error}{Colour of type II errors in the plot}
\item{alpha, power}{Plotted as lines if provided}
\item{xlim, ylim, xaxs, yaxs, main, xlab, ylab}{Different defaults for plotting parameters}
\item{\dots}{Additional plotting parameters to pass to plot function}
}
\seealso{
\code{\link{binom_one_alpha}}, \code{\link{binom_one_assurance}}
}
\examples{
# Simon's two stage design
failure=c(0,3)
success=c(NA,4)
n=c(7,18)
p0=0.1
p1=0.3

# power
binom_one_power(p1,failure,success,n)
# type 1 error (alpha)
binom_one_power(p0,failure,success,n)

# plot
plot_binom_one_power(failure,success,n,ndivisions=1000,p=c(p0,p1),
 alpha=0.1,power=0.8)
}