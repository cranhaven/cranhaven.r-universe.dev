\name{SelectMinED}
\alias{SelectMinED}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Select Minimum Energy Design samples from a candidate set
}
\description{
Select MinED samples from \code{candidates} by optimizing the generalized MinED criterion in Joseph et al. (2019).
}
\usage{
SelectMinED(candidates, candlf, n, gamma=1, s=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{candidates}{
  Candidate samples from the target distribution, which can be MC, QMC, or MCMC samples.
}
  \item{candlf}{
  The log-unnormalized density function values corresponding to the \code{candidates}.
}
  \item{n}{
  The required number of MinED samples.
}
  \item{gamma}{
  The parameter in the anealled version of density function. Optional, default is ``1''. 
}
  \item{s}{
  The parameter in generalized distance. Optional, default is ``2''.
}
}
\details{
This function select MinED samples from a given set of candidate samples. The function is used internally in the \code{mined} function repeatedly for K times, where K is the number of annealing steps in the algorithm. Refer to Joseph et al., (2018) for more details.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The value returned from the function is a list containing the following components:
  \item{points}{The MinED samples selected from the \code{candidates}.}
  \item{logf}{The log-unnormalized density function values of the \code{points}.}
}
\references{
%% ~put references to the literature/web site here ~
Joseph, V. R., Wang, D., Gu, L., Lv, S., and Tuo, R. (2019). "Deterministic Sampling of Expensive Posteriors Using Minimum Energy Designs", \emph{Technometrics}, 61, 297-308, arXiv:1712.08929, DOI:10.1080/00401706.2018.1552203.
}
\author{
Dianpeng Wang <wdp@bit.edu.cn> and V. Roshan Joseph <roshan@gatech.edu>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{mined}}
}
\examples{
cand <- matrix(runif(10000, min = -4, max = 4), ncol = 1)
candlf <- log(dnorm(cand))
res <- mined::SelectMinED(cand, as.vector(candlf), 150, 1.0, 2.0)
print(res)
par(mfrow=c(1,2))
hist(cand)
hist(res$points)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Experimental Design}% use one of  RShowDoc("KEYWORDS")
