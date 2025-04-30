\name{getBestModel}
\alias{getBestModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the best model with nvar variables, or by AIC, BIC or knee-point.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns a binary vector of variables for the best model, as defined by either the AIC, BIC, or knee-point, or alternatively the best for a given number of variables.
}
\usage{
getBestModel(mogavs, nvar, method = c("AIC", "BIC", "knee", "mse", NULL))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mogavs}{
%%     ~~Describe \code{mogavs} here~~
A model of the class mogavs.
}
  \item{nvar}{
%%     ~~Describe \code{nvar} here~~
Number of variables for the best model. Only used if method is mse or NULL. Can be omitted if method is named and is AIC, BIC or knee.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
The desired metric for defining the best model. If nvar is omitted, method must be named.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The methods AIC, BIC and knee look at the whole set of tried models, whereas mse or NULL means that the function looks for the best model with nvar variables and the lowest mean square error.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A binary vector of the variables in the best model.
}
\author{
%%  ~~who you are~~
Tommi Pajala <tommi.pajala@aalto.fi>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{getBestModelVars}}
}
\examples{
data(sampleData)
mod<-mogavs(y~.,data=sampleData,maxGenerations=20)
getBestModel(mod,15,"mse")
getBestModel(mod,method="BIC")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
