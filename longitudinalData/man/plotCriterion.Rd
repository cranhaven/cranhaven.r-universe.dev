\name{plotCriterion}
\Rdversion{1.1}
\alias{plotCriterion}
\alias{plotCriterion-method}
\alias{plotCriterion,ListPartition}
\alias{plotCriterion,ListPartition-method}
\title{
~ Function: plotCriterion ~
}
\description{
  This function graphically displays the quality criterion of all the
  \code{\linkS4class{Partition}} of a \code{\linkS4class{ListPartition}} object.
}
\usage{
plotCriterion(x, criterion=x["criterionActif"],nbCriterion=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterLongData]: object whose quality criterion should be
    displayed.}
  \item{criterion}{[character]: name of the criterion(s) to plot. It can either display all the
    value for a single specific criterion or display several criterion,
    only the best value for each clusters number and for each criterion.}
  \item{nbCriterion}{[numeric]: if there is a big number of
    \code{Partition}, the graphical display of all of them can be
    slow. \code{nbCriterion} lets the user limit the number of criteria
    that will be taken in account.}
}

\details{
  This function display graphically the quality criterion (probably to
  decide the best clusters' number). It can either display all the
  criterion ; this is useful to see the consistency of the result : is
  the best clusterization obtain several time or only one ? It can also
  display only the best result for each clusters number : this helps to
  find the local maximum, which is classically used to chose the
  "correct" clusters' number.
}

\value{
  No value are return. A graph is printed.
}


\examples{
###############
### Data generation
data(artificialLongData)
traj <- as.matrix(artificialLongData[,-1])

### Some clustering
listPart <- listPartition()
listPart["add"] <- partition(rep(c("A","B"),time=100),traj)
listPart["add"] <- partition(rep(c("A","B","B","B"),time=50),traj)
listPart["add"] <- partition(rep(c("A","B","C","A"),time=50),traj)
listPart["add"] <- partition(rep(c("A","B","C","D"),time=50),traj)
ordered(listPart)

################
### graphical display
plotCriterion(listPart)
plotAllCriterion(listPart,criterion=CRITERION_NAMES[1:5],TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
