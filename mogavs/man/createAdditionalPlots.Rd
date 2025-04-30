\name{createAdditionalPlots}
\alias{createAdditionalPlots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for plotting boundaries of the archive set.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A plotting function for plotting the set of all tried models, and highlighting either all models within \code{epsilonBand} MSE of the efficient frontier, or the \code{kBest} best models for each number of variables.
}
\usage{
createAdditionalPlots(mogavs, epsilonBand = 0, kBest = 1, method = c("MSE", "kBest"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mogavs}{
%%     ~~Describe \code{mogavs} here~~
A model of class mogavs.
}
  \item{epsilonBand}{
%%     ~~Describe \code{epsilonBand} here~~
The value of epsilonBand, ie. the mean square error inside which models are highlighted.
}
  \item{kBest}{
%%     ~~Describe \code{kBest} here~~
The number of models that will be highlighted for each number of variables.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
Either \code{MSE} or \code{kBest} (case-insensitive). \code{MSE} plots the set of all tried models, with models inside the \code{epsilonBand} highlighted. \code{method="kBest"} plots the set of all tried models, with the \code{kBest} best models for each number of variables highlighted.
}
}
\author{
%%  ~~who you are~~
Tommi Pajala <tommi.pajala@aalto.fi>
}
\seealso{
\code{\link{mogavs}}
}
\examples{
data(sampleData)
mod<-mogavs(y~.,data=sampleData,maxGenerations=20)
createAdditionalPlots(mod,epsilonBand=0,kBest=15,"kbest")
createAdditionalPlots(mod,epsilonBand=0.001,method="mse")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
