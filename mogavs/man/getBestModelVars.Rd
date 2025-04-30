\name{getBestModelVars}
\alias{getBestModelVars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get variable names of the best model with nvar variables, or defined by lowest MSE, AIC, BIC or knee-point.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns a vector of variable names for the best model, as defined by either the AIC, BIC, or knee-point, or alternatively the best for a given number of variables.
}
\usage{
getBestModelVars(mogavs, nvars, data, method=c("AIC","BIC","mse",NULL))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mogavs}{
%%     ~~Describe \code{mogavs} here~~
A model of the class mogavs.
}
  \item{nvars}{
%%     ~~Describe \code{nvar} here~~
Number of variables for the best model. Only used if method is NULL or MSE.
}

  \item{data}{
The used data set.  
}

  \item{method}{
%%     ~~Describe \code{method} here~~
The desired metric for defining the best model. If nvar is omitted, method must be named.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The methods AIC, BIC and knee look at the whole set of tried models, whereas NULL means that the function looks for the best model with $nvar$ variables and the lowest mean square error.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a character vector of the variable names of the best model.
}

\author{
%%  ~~who you are~~
Tommi Pajala <tommi.pajala@aalto.fi>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{getBestModel}},\code{\link{plotVarUsage}}
}
\examples{
data(sampleData)
mod<-mogavs(y~.,data=sampleData,maxGenerations=20)
getBestModelVars(mod,nvars=15,sampleData,NULL)
getBestModelVars(mod,nvars=0,data=sampleData,method="BIC")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
