\name{combined}
\alias{combined}
\title{
Convert the list into a symmetric matrix
}
\description{
This function converts the list of vectors into a symmetric matrix}
\usage{
combined(dat,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
 An input matrix. The columns represent variables and the rows indicate observations.
}
\item{y}{
 List data that includes estimates from nodewise regression. 
}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
\value{
\item{symMat}{
 Symmetric matrix that represents the inferred network
}
}
%%\references{
%% ~put references to the literature/web site here ~
%%}
%%\author{
%%  ~~who you are~~
%%}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
%%\examples{
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.

%%## The function is currently defined as
%%function (x) 
%%{
%%  }
%%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
