\name{tsls.wfit}
\alias{tsls.wfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit instrumental variables model via two-stage least squares, with weights.
}
\description{
Fits linear first- and second-stage models using weighted linear regression.
}
\usage{
tsls.wfit(X, Y, Z, weights, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
Matrix of covariates and treatment, including constant if intercept is desired.
}
  \item{Y}{
Vector outcome.
}
  \item{Z}{
Matrix of covariates and instrument, including constant if intercept is desired.
}
  \item{weights}{
Observation weights.
}
  \item{\dots}{
Additional arguments to \code{lm.wfit}.
}
}
\value{
A list with the second stage model fit,
as returned by \code{lm.wfit}.
}
\references{
Peter M. Aronow and Allison Carnegie. (2013). Beyond LATE: Estimation of the average treatment effect with an instrumental variable. \emph{Political Analysis}.
}
\author{
Peter Aronow <peter.aronow@yale.edu>, Dean Eckles <dean@deaneckles.com>, Kyle Peyton <kyle.peyton@yale.edu>
}

\seealso{
\code{\link{lm.wfit}}.

\code{\link{compliance.score}} for calculating compliance scores used in example. 
}
\examples{
# Load example dataset, see help(FoxDebate) for details
data(FoxDebate)

# Estimate compliance scores with covariates, assuming (default) 
#   case of two-sided non-compliance.
covmat <- with(FoxDebate, cbind(partyid, pnintst, watchnat, educad, readnews, gender, 
                                income, white))

cscoreout <- with(FoxDebate, compliance.score(D = watchpro, Z = conditn,
                              W = covmat))

# Extract vector of estimated compliance scores
cscore <- cscoreout$C.score

# Winsorising as described in Aronow and Carnegie (2013)
N <- length(FoxDebate[, "infopro"])
alpha <- 0.275
qcscore <- quantile(cscore, 1 / (N^alpha))
cscore[cscore < qcscore] <- qcscore

# IPW reweighting step Aronow and Carnegie (2013) use for missing data
Ymis <- is.na(FoxDebate[,"infopro"])

IPWweight <- 1 / (1 - predict(glm(Ymis ~ covmat, family = binomial(link = "probit")), 
                              type = "response"))
IPWweight[Ymis] <- 0

# Compute LATE via 2SLS with IPW weights. This replicates 
#   Table 2 Column 1 in Aronow and Carnegie (2013)
outputTSLS <- with(FoxDebate, tsls.wfit(X = cbind(1, covmat, watchpro), Y = infopro, 
                        Z = cbind(1, covmat, conditn), weights = IPWweight))
round(outputTSLS$coefficients, 2)

# Compute ATE via 2SLS with (IPW weights / compliance scores). This
#   replicates Table 2 Column 2 in Aronow and Carnegie (2013)
outputICSW <- with(FoxDebate, tsls.wfit(cbind(1, watchpro, covmat), infopro, 
                        cbind(1, conditn, covmat), w = IPWweight / cscore))
round(outputICSW$coefficients, 2) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line