\name{icsw.tsls}
\alias{icsw.tsls}
\alias{icsw.tsls.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Two-stage least squares with inverse complier score weighting
}
\description{
Estimate average treatment effects using two-stage least squares
with a binary instrument and binary treatment and weighting with
inverse complier scores (probabilities of compliance). Optionally,
bootstrap the entire estimation process for the purpose of hypothesis
testing and constructing confidence intervals.
}
\usage{
icsw.tsls(D, X, Y, Z, W, weights = NULL,
  R = 0, estimand = c("ATE", "ATT"),
  min.prob.quantile = NULL,
  min.prob = NULL, ...)

icsw.tsls.fit(D, X, Y, Z, W, weights,
  estimand = c("ATE", "ATT"),
  min.prob.quantile = NULL,
  min.prob = NULL, ...)
}
\arguments{
  \item{D}{
Binary treatment of interest.
}
  \item{X}{
Matrix of covariates for two-stage least squares. Add a constant if desired (see examples). 
}
  \item{Y}{
Outcome.
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
  \item{R}{
Number of bootstrap replicates.
}
  \item{estimand}{
Whether to estimate average treatment effect (default) or
average treatment effect on the treated.
}
  \item{min.prob.quantile}{
Compliance scores are truncated to this quantile of positive compliance scores.
}
  \item{min.prob}{
Compliance scores are truncated to this value.
}
  \item{\dots}{
Additional arguments to \code{compliance.score}.
}
}

\value{
If R = 0 or icsw.tsls.fit is called directly, a model fit,
as described in \code{\link{lm.wfit}}.

If R > 0, a list with elements
\item{fitted.model}{
  A model fit, as returned by \code{\link{lm.wfit}}.
}
\item{coefs.boot}{
  p by R matrix of model coefficients for each of R bootstrap replicates.
}
\item{coefs.se.boot}{
  Vector of standard deviations of coefficients under bootstrap resampling
  (i.e., bootstrap standard errors).
}
}
\references{
Peter M. Aronow and Allison Carnegie. (2013). Beyond LATE: Estimation of the average treatment effect with an instrumental variable. \emph{Political Analysis}.
}
\author{
Peter M. Aronow <peter.aronow@yale.edu>; Dean Eckles <icsw@deaneckles.com>; Kyle Peyton <kyle.peyton@yale.edu>
}

\seealso{
\code{\link{compliance.score}} for calculating compliance scores used in example. 

\code{\link{tsls.wfit}} for regression via 2SLS with weights. 

}
\examples{

# Load example dataset, see help(FoxDebate) for details
data(FoxDebate)

# Ipw reweighting step Aronow and Carnegie (2013) use for missing data
covmat <- with(FoxDebate, cbind(partyid, pnintst, watchnat, educad, readnews, gender, 
                                income, white))

# IPW reweighting step Aronow and Carnegie (2013) use for missing data
Ymis <- is.na(FoxDebate[, "infopro"])

IPWweight <- 1 / (1 - predict(glm(Ymis ~ covmat, family = binomial(link = "probit")),
                                  type = "response"))
IPWweight[Ymis] <- 0

N <- length(FoxDebate[, "infopro"])
alpha <- 0.275

# Compute the ATE of watching the Fox Debate on knowledge . This replicates the 
#   ATE from column 2 of Table 1 in Aronow and Carnegie (2013) 
icsw.out <- with(FoxDebate, icsw.tsls(D = watchpro, X = cbind(1, covmat), 
                                          Y = infopro, Z = conditn, W = covmat,
                                          min.prob.quantile = 1 / (N^alpha),
                          weights = IPWweight))
round(icsw.out$coefficients["D"], 2)

# Example with bootstrap (this takes awhile!)
\donttest{icsw.out <- with(FoxDebate, icsw.tsls(D = watchpro, X = cbind(1, covmat), 
                                      Y = infopro, Z = conditn, W = covmat,
                                      min.prob.quantile = 1 / (N^alpha), 
                                      weights = IPWweight, R = 1000))

# Display vector of coefficients
icsw.out$coefficients

# Display vector of (bootstrapped) SEs
icsw.out$coefs.se.boot
}
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
