\name{plotVarUsage}
\alias{plotVarUsage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Produce a visual summary of how many times each variable appears on the efficient frontier.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Visualizes how models on the efficient frontier use different variables. May be useful for finding out which variables seem to be most useful for explanation.
}
\usage{
plotVarUsage(mogavs, method = c("hist", "plot", "table"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mogavs}{
%%     ~~Describe \code{mogavs} here~~
A model of the class mogavs.
}
  \item{method}{
%%     ~~Describe \code{method} here~~
The chosen method for visualizing variable usage, \code{hist} for a histogram, \code{plot} for a plot, and \code{table} for just a table.
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
In the case of \code{method="hist"} or \code{method="plot"} doesn't return anything, if \code{method="table"} returns a table.
}
\author{
%%  ~~who you are~~
Tommi Pajala <tommi.pajala@aalto.fi>
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getBestModel}},\code{\link{getBestModelVars}}
}
\examples{
data(sampleData)
mod<-mogavs(y~.,data=sampleData,maxGenerations=20)
plotVarUsage(mod,"table")
plotVarUsage(mod,"hist")
plotVarUsage(mod,"plot")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
