\name{test.nm}
\alias{test.nm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Graphical tool to check the Markov assumption}
\description{
It constructs a PP-plot which compares the transition probabilities reported by the non-Markovian and Aalen-Johansen estimators. Under the Markov assumption the PP-plot should fit the straight line y=x. When the Markov assumption holds the Aalen-Johansen is preferred since it provides a smaller standard error. If the Markov assumption is violated, the Aalen-Johansen may be inconsistent and therefore the non-Markovian method is recommended.
}
\usage{
test.nm(data, s, t = "last")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
     A data.frame including at least four columns named \code{time1}, \code{event1}, \code{Stime} and \code{event}, which correspond to disease free survival time, disease free survival indicator, time to death or censoring, and death indicator, respectively.
}
  \item{s}{
     The current time for the transition probabilities to be computed.
}
  \item{t}{
     The future time for the transition probabilities to be computed. Default is \dQuote{last} which means the largest time among the uncensored entry times for the intermediate state and the final absorbing state.
}
}
\details{
  It constructs a PP-plot which compares the transition probabilities reported by the non-Markovian and Aalen-Johansen estimators. Under the Markov assumption the PP-plot should fit the straight line y=x. When the Markov assumption holds the Aalen-Johansen is preferred since it provides a smaller standard error. If the Markov assumption is violated, the Aalen-Johansen may be inconsistent and therefore the non-Markovian method is recommended. The PP-plot excludes \eqn{P_{11}}{P_{11}}(s,t) since both estimators agree in this case. Also, the user-supplied \code{s} must be strictly positive, because the Markov assumption is not relevant for the estimation of occupation probabilities (s=0).
}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
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
\examples{
data(colonTP)

test.nm(colonTP, s = 0) 
# nothig is displayed since the Markov condition is not relevant 
# for the case s=0 (occupation probabilities) 

test.nm(colonTP, s = 365, t = 1095)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
