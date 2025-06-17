\name{ganova}
\alias{ganova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ANOVA table for linear hypothesis in a linear model
}
\description{
Prepares Analysis of Variance table for testing a general linear hypothesis in a linear model
}
\usage{
ganova(y, X, A, xi, tol=sqrt(.Machine$double.eps))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
Responese vector in linear model.
}
  \item{X}{
Design matrix or matrix containing values of explanatory variables (generally including intercept).
}
  \item{A}{
Coefficient matrix (A.beta = xi is the null hypothesis to be tested).
}
  \item{xi}{
A vector (A.beta = xi is the null hypothesis to be tested).
}
\item{tol}{
A relative tolerance to detect zero singular values while computing generalized inverse, in case the model matrix is rank deficient (default = sqrt(.Machine$double.eps)).
 }
}

\value{
Returns analysis of variance table for testing A.beta = xi in the linear model with response vector y and matrix of explanatory variables/factors X.
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
X <- cbind(1,binaries(Denim), binaries(Laundry))
A <- rbind(c(0,1,-1,0,0,0,0), c(0,1,0,-1,0,0,0))
xi <- c(0, 0)
ganova(Abrasion, X, A, xi)
detach(denim)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ model }
\keyword{ design }
\keyword{ regression }

