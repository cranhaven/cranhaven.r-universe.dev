\name{plotAllCriterion}
\Rdversion{1.1}
\alias{plotAllCriterion}
\alias{plotAllCriterion-method}
\alias{plotAllCriterion,ListPartition}
\alias{plotAllCriterion,ListPartition-method}
\title{
~ Function: plotAllCriterion ~
}
\description{
  This function graphically displays the quality criterion of all the
  \code{\linkS4class{Partition}} of a \code{\linkS4class{ListPartition}} object.
}
\usage{
plotAllCriterion(x, criterion=CRITERION_NAMES[1:5],standardized = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{[ClusterLongData]: object whose quality criterion should be
    displayed.}
  \item{criterion}{[character]: name of the criterion(s) to plot. It can either display all the
    value for a single specific criterion or display several criterion,
    only the best value for each clusters number and for each criterion.}
  \item{standardized}{[logical]: If \code{standardized=TRUE}, all the
    criterion will be mapped into [0,1]. This makes them more easely comparable.}
}

\details{
  This function display graphically several quality criterion, probably to
  decide the best clusters' number.
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
plotAllCriterion(listPart)
plotAllCriterion(listPart,criterion=CRITERION_NAMES[1:5],TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
