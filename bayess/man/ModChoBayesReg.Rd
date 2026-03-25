\name{ModChoBayesReg}
\alias{ModChoBayesReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian model choice procedure for the linear model%%  ~~function to do ... ~~
}
\description{
This function computes the posterior probabilities of all
(for less than 15 covariates) or the most probable (for more than 15 covariates)
submodels obtained by eliminating some covariates.
}
\usage{
ModChoBayesReg(y, X, g = length(y), betatilde = rep(0, dim(X)[2]), 
niter = 1e+05, prt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
response variable%%     ~~Describe \code{y} here~~
}
  \item{X}{
covariate matrix%%     ~~Describe \code{X} here~~
}
  \item{g}{
constant in the \eqn{g}{G} prior%%     ~~Describe \code{g} here~~
}
  \item{betatilde}{
prior expectation of the regression coefficient \eqn{\beta}{beta}
}
  \item{niter}{
number of Gibbs iterations in the case there are more than 15 covariates%%     ~~Describe \code{niter} here~~
}
  \item{prt}{
boolean variable for printing the standard output%%     ~~Describe \code{prt} here~~
}
}
\details{
When using a conjugate prior for the linear model such as the \eqn{G}{G} prior,
the marginal likelihood and hence the evidence are available in closed form. If the number
of explanatory variables is less than 15, the exact
derivation of the posterior probabilities for all submodels can be undertaken.
Indeed, \eqn{2^{15}=32768}{2^15=32768} means that the problem remains tractable. 
When the number of explanatory variables gets larger, a random exploration of the collection
of submodels becomes necessary, as explained in the book (Chapter 3). The proposal to change
one variable indicator is made at random and accepting this move follows from a Metropolis--Hastings
step.
}
\value{
%%  ~Describe the value returned
    \item{top10models }{models with the ten largest posterior probabilities}
    \item{postprobtop10 }{posterior probabilities of those ten most likely models}
}
\examples{
data(caterpillar)
y=log(caterpillar$y)
X=as.matrix(caterpillar[,1:8])
res2=ModChoBayesReg(y,X)
}
\keyword{linear regression}
\keyword{model choice}
\keyword{random walk}
