\name{compliance.score}
\alias{compliance.score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimate compliance scores using covariates.
}
\description{
Estimate probability that units are compliers using generalized linear models for
probability of being a complier or always-taker and for being an always-taker conditional
on being a complier or always-taker. These compliance scores can be used in inverse
probability weighting to estimate average treatment effects. In the case of one-sided non-compliance,
this estimation is considerably simpler.
}
\usage{
compliance.score(D, Z, W, weights = NULL,
  link = qnorm, inv.link = pnorm, genoud = TRUE,
  num.iter = ifelse(genoud, 200, 10000),
  one.sided = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{
Binary treatment of interest.
}
  \item{Z}{
Binary instrument.
}
  \item{W}{
Matrix of covariates for compliance model.
}
  \item{weights}{
Observation weights.
}
  \item{link}{
Link function applied for linear models. Defaults to probit link function.
}
  \item{inv.link}{
Inverse link function (i.e., mean function) applied for linear models. Defaults to probit mean function.
}
  \item{genoud}{
Whether to use global optimization via genetic optimization from package \code{rgenoud}.
Default, and highly recommended because the likelihood need not be concave.
Otherwise, use the BOBYQA algorithm for constrained optimization from package \code{minqa}.
}
  \item{num.iter}{
Number of iterations of optimization routine.
}
  \item{one.sided}{
Whether non-compliance is one-sided (logical). When compliance is one-sided, the previous
four arguments are ignored, and the compliance scores are estimated with probit regression.
}
}
\details{
A unit \eqn{i} is a complier if \eqn{D_{i1} > D_{i0}}, where \eqn{D_{i1}} and
\eqn{D_{i0}} are the potential treatments for unit \eqn{i} when \eqn{Z} is set to
1 and 0. This is a latent (unobserved) characteristic of individual units, since
each unit is only observed with one value of \eqn{Z}.

By default this function uses genetic optimization via \code{genoud} because
the loss function for the complier scores is not necessarily convex.

}
\value{
Vector of estimated probabilities of being a complier (i.e., compliance scores).
}
\references{
Peter M. Aronow and Allison Carnegie. (2013). Beyond LATE: Estimation of the average treatment effect with an instrumental variable. \emph{Political Analysis}.
}
\author{
Peter M. Aronow <peter.aronow@yale.edu>, Dean Eckles <icsw@deaneckles.com>, Kyle Peyton <kyle.peyton@yale.edu>
}
\note{
Requires \code{rgenoud} package if \code{genoud} = TRUE. Requires \code{minqa} package if \code{genoud} = FALSE. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Used by \code{\link{icsw.tsls}}. 
}
\examples{

# Load example dataset, see help(FoxDebate) for details
data(FoxDebate)

# Matrix of covariates
covmat <- with(FoxDebate, cbind(partyid, pnintst, watchnat, educad, readnews, 
                                gender, income, white))

# Estimate compliance scores with covariates, assuming (default) 
#   case of two-sided non-compliance
cscoreout <- with(FoxDebate, compliance.score(D = watchpro, Z = conditn,
                                              W = covmat))

# Extract vector of estimated compliance scores
cscore <- cscoreout$C.score
summary(cscore)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
