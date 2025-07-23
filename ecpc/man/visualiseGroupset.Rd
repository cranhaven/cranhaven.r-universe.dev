\name{visualiseGroupset}
\alias{visualiseGroupset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Visualise a group set
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Visualises a group set in a graph, with directed edges indicating the hierarchy.
}
\usage{
visualiseGroupset(Groupset, groupweights, groupset.grouplvl, nodeSize = 10, ls = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Groupset}{
%%     ~~Describe \code{Groupset} here~~
List of G groups of covariates.
}
  \item{groupweights}{
%%     ~~Describe \code{groupweights} here~~
(optional) vector with G group weights; if given, group weights are visualised too.
}
  \item{groupset.grouplvl}{
%%     ~~Describe \code{groupset.grouplvl} here~~
List of G_2 groups defining a hierarchy.
}
  \item{nodeSize}{
%%     ~~Describe \code{nodeSize} here~~
Size of the nodes in the visualisation; scalar.
}
  \item{ls}{
%%     ~~Describe \code{ls} here~~
Line size; scalar.
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A ggplot object.

}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{visualiseGroupsetweights}} to plot estimated group set weights. and \code{\link{visualiseGroupweights}} to plot estimated group weights.
}
\examples{
#groups without hierarchical constraints
groupset <- list("Group1"=c(1:20),"Group2"=c(15,30))
visualiseGroupset(groupset,c(0.5,2))

#hierarchical groups
cont.codata <- seq(0,1,length.out=20) #continuous co-data
#only split at lower continous co-data group
hierarchicalgroupset <- splitMedian(values=cont.codata,split="lower",minGroupSize=5)
#obtain groups on group level defining the hierarchy
groupset.grouplvl <- obtainHierarchy(hierarchicalgroupset)
visualiseGroupset(hierarchicalgroupset, groupset.grouplvl=groupset.grouplvl)

}
