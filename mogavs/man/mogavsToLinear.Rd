\name{mogavsToLinear}
\alias{mogavsToLinear}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform a mogavs model into a linear model.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Takes in a mogavs model and a number of variables, and transforms that into linear model as in \code{lm}.
}
\usage{
mogavsToLinear(bestModel, y_ind, data, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bestModel}{
  A binary vector, representing the variables in one model for a given number of variables.
%%     ~~Describe \code{bestModel} here~~
}
  \item{y_ind}{
  Column number for the y values in data.
%%     ~~Describe \code{y_ind} here~~
}
  \item{data}{
  The used data set.
%%     ~~Describe \code{data} here~~
}
  \item{\dots}{
  Additional arguments.
%%     ~~Describe \code{\dots} here~~
}
}

\value{
\item{lm}{
A linear model of class \code{lm}.
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Tommi Pajala <tommi.pajala@aalto.fi>
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{getBestModel}},\code{\link{getBestModelVars}}
}
\examples{
data(sampleData)
mod<-mogavs(y~.,sampleData,maxGenerations=20)

#get the best model with 15 variables
bm<-getBestModel(mod,15,method=NULL)

#transform best model into a linear model
mogavsToLinear(bm,1,sampleData)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
