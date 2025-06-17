\name{confelps}
\alias{confelps}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confidence ellipsiod for multiple parameters in a linear model.
}
\description{
Computes confidence ellipsiod for a vector of estimable functions in a linear model.
}
\usage{
confelps(y, X, A, alpha, tol=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Responese vector in linear model.
}
  \item{X}{
Design/model matrix or matrix containing values of explanatory variables (generally including intercept).
}
  \item{A}{
Coefficient matrix (A.beta is the vector for which confidence interval is needed).
}
  \item{alpha}{
The non-coverage probability of confidence ellipsoid.
}
 \item{tol}{
A relative tolerance to detect zero singular values while computing generalized inverse, in case X is rank deficient (default = sqrt(.Machine$double.eps)).
}
}

\details{
Normal distribution of response (given explanatory variables and/or factors) is assumed.
}

\value{
Returns a list of three objects:
\item{CenterOfEllipse}{Center of ellipsoid.}
\item{MatrixOfEllipse}{Matrix of ellipsoid, for describing quadratic form in terms of the vector of deviations from center of ellipsoid.}
\item{threshold}{Upper limit of quadratic form that completes specification of ellipsoid.}
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(denim)
attach(denim)
X <- cbind(1,binaries(Denim),binaries(Laundry))
A <- rbind(c(0,1,0,-1,0,0,0),c(0,0,1,-1,0,0,0))
confelps(Abrasion, X, A, 0.05,tol=1e-12)
detach(denim)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ model }
\keyword{ design }
\keyword{ regression }
