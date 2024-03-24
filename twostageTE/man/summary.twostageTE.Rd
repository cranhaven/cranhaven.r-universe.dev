\name{summary.twostageTE}
\alias{summary.twostageTE}
\alias{summary}
\title{
summary method for object twostageTE
}
\description{
summary method for object twostageTE
}
\usage{
\method{summary}{twostageTE}(object, ...)
}
\arguments{
  \item{object}{twostageTE object}
  \item{\dots}{ignored}
}
\value{
prints confidence interval, point estimate, and auxiliary estimates
}
\author{
Shawn Mankad
}

\examples{
X=runif(25, 0,1)
Y=X^2+rnorm(n=length(X), sd=0.1)
oneStage_IR=stageOneAnalysis(X, Y, 0.25, type="IR-wald", 0.99)
summary(oneStage_IR)

}

