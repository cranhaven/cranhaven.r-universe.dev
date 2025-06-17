\name{cisngl}
\alias{cisngl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence interval for a linear parametric function in a linear model
}
\description{
Computes point estimate and confidence interval for a single linear parametric function in a linear model.
}
\usage{
cisngl(y, X, p, alpha, type, tol=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Responese vector in linear model.
}
  \item{X}{
Design/model matrix or matrix containing values of explanatory variables (generally including intercept).
}
  \item{p}{
Coefficient vector of linear parametric function for which confidence interval is needed.
}
  \item{alpha}{
Non-coverage probability of confidence interval.
}
  \item{type}{
Type of confidence interval ("lower", "upper", "both").
}
 \item{tol}{
A relative tolerance to detect zero singular values while computing generalized inverse, in case X is rank deficient (default = sqrt(.Machine$double.eps)).
 }
}

\details{
Normal distribution of response (given explanatory variables and/or factors) is assumed.
}

\value{
Returns a list of two objects:
\item{estimate}{Point estimate.}
\item{ci}{Confidence interval.}
}

\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
library(MASS)
data(birthwt)
attach(birthwt)
X <- cbind(1, smoke, binaries(race))
p <- c(0,1,0,0,0)
cisngl(bwt, X, p, 0.05, type = "upper", tol = 1e-10)
cisngl(bwt, X, p, 0.05, type = "both", tol = 1e-10) 
detach(birthwt)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ model }
\keyword{ design }
\keyword{ regression }
