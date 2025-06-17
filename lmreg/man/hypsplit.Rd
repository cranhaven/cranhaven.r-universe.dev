\name{hypsplit}
\alias{hypsplit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Testable and untestable hypotheses in linear model
}
\description{
Reduces a general hypothesis in a linear model into a pair of completely testable and completely untestable hypotheses.
}
\usage{
hypsplit(X, A, xi, tol=sqrt(.Machine$double.eps))
}

\arguments{
  \item{X}{
Design/model matrix or matrix containing values of explanatory variables (generally including intercept).
}
  \item{A}{
Coefficient matrix (A.beta = xi is the null hypothesis to be split).
}
  \item{xi}{
A vector (A.beta = xi is the null hypothesis to be tested).
}
 \item{tol}{
A relative tolerance to detect zero singular values while computing generalized inverse, in case X is rank deficient (default = sqrt(.Machine$double.eps)).
 }
}

\value{
A list of two objects:
\item{testable }{Coefficient matrix and constant vector for testable part of hypotheses.}
 \item{untestable }{Coefficient matrix and constant vector for untestable part of hypotheses.}
}
\references{
Sengupta and Jammalamadaka (2019), Linear Models and Regression with R: An Integrated Approach.
}
\author{
Debasis Sengupta <shairiksengupta@gmail.com>, Jinwen Qiu <qjwsnow_ctw@hotmail.com>
}

\examples{
data(denim)
attach(denim)
X <- cbind(1, binaries(Denim), binaries(Laundry))
A <- rbind(c(0,1,0,0,0,0,0), c(0,0,1,0,0,0,0), c(0,0,0,1,0,0,0))
xi <- c(0,0,0)
hypotheses <- hypsplit(X, A, xi, tol=1e-13)
hypotheses[[1]]  # testable
hypotheses[[2]]  # untestable
detach(denim)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ model }
\keyword{ design }
\keyword{ regression }

