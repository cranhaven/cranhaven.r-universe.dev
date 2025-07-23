\name{visualiseGroupsetweights}
\alias{visualiseGroupsetweights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Visualise estimated group set weights
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot group set weights from multiple cross-validation folds.
}
\usage{
visualiseGroupsetweights(dfGrps, GroupsetNames, hist = FALSE, boxplot = TRUE, 
                          jitter = TRUE, ps = 1.5, width = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dfGrps}{
%%     ~~Describe \code{dfGrps} here~~
Data frame containing the following variables;
  'Groupset': factor with group set names;
  'Groupset.weight': group set weight of each group set;
  'Fold': number indicating which fold in the cross-validation is used.
}
  \item{GroupsetNames}{
%%     ~~Describe \code{GroupsetNames} here~~
Vector with names of the group sets.
}
  \item{hist}{
%%     ~~Describe \code{hist} here~~
Should histogram be plotted?
}
  \item{boxplot}{
%%     ~~Describe \code{boxplot} here~~
Should boxplot be used or points?
}
  \item{jitter}{
%%     ~~Describe \code{jitter} here~~
Should group set weights be jittered?
}
  \item{ps}{
%%     ~~Describe \code{ps} here~~
Point size.
}
  \item{width}{
%%     ~~Describe \code{width} here~~
Width of jitter.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Plot in ggplot object.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{visualiseGroupset}} to visualise group sets and \code{\link{visualiseGroupweights}} to plot estimated group weights.
}
\examples{
dfGrps <- data.frame(Groupset=rep(c(1,2),each=10),
                     Groupset.weight=c(rnorm(10,0,0.01),rnorm(10,1,0.05)),
                     Fold=rep(1:10,2))
GroupsetNames <- c("Groupset1","Groupset2")
visualiseGroupsetweights(dfGrps, GroupsetNames, hist = FALSE, boxplot = TRUE,jitter=TRUE)
}

