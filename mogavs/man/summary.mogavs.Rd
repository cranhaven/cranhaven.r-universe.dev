\name{summary.mogavs}
\alias{summary.mogavs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary function for mogavs
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
S3 summary method for the mogavs class, producing output about the run and the models on the efficient frontier.
}
\usage{
\method{summary}{mogavs}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
  A model of class mogavs.
  }
  \item{\dots}{
  Additional arguments for summary, only here to achieve S3 consistency, ie. they are ignored.
  }
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list with the following details:
\item{maxGenerations}{The number of generations run for the model.}
\item{boundary}{The efficient frontier, summarized as a two-column matrix with the number of variables and MSE.}
\item{modelsTried}{The number of models tried during the run.}
}
\author{
%%  ~~who you are~~
Tommi Pajala <tommi.pajala@aalto.fi>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{mogavs}}
}
\examples{
data(sampleData)
mod<-mogavs(y~.,data=sampleData,maxGenerations=20)
summary(mod)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
