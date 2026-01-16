\name{ordered(ListPartition)}
\Rdversion{1.1}
\alias{ordered}
\alias{ordered,ListPartition}
\alias{ordered,ListPartition-method}
\title{
  ~ Function: ordered(ListPartition) ~
}
\description{
  Sort the \code{\linkS4class{Partition}} of a
  \code{\linkS4class{ListPartition}} according to a quality criterion.
%  (the function \code{ordered} for \code{Partition} is describe
 % \link[=ordered,Partition-method]{here}).
}

\usage{
ordered(x,...)
}

\arguments{
  \item{x}{[ListPartition]: Object whose \code{Partition} should be sort.}
  \item{\dots}{Note used, for S4 compatibility only.}
}

\details{
  Sort the \code{Partition} of a \code{ListPartition} for each
  list (sort the 'c2' list, the 'c3' list,...) according to a quality criterion.
  The criterion used to sort is the one in the field
  \code{criterionActif}.
}
\value{
  This function change internaly the order of the fields \code{c2},
  \code{c3}, ... \code{c26} of an object. In addition, it return the
  permutation matrix (the matrix use to re-ordered the \code{ci}).
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011
}



\examples{
##############
### Preparing data
data(artificialLongData)
traj <- as.matrix(artificialLongData[,-1])

### Some clustering
part2 <- partition(rep(c("A","B"),time=100),traj)
part3 <- partition(rep(c("A","B","C","A"),time=50),traj)
part3b <- partition(rep(c("A","B","C","B"),time=50),traj)
part4 <- partition(rep(c("A","B","C","D"),time=50),traj)


################
### ListPartition
listPart <- listPartition()
listPart['criterionActif'] <-"Davies.Bouldin"
plotCriterion(listPart)

listPart["add"] <- part2
listPart["add"] <- part3
listPart["add"] <- part3b
listPart["add"] <- part4
listPart["add"] <- part4
listPart["add"] <- part3
listPart["add"] <- part3b

plotCriterion(listPart)
ordered(listPart)
plotCriterion(listPart)

listPart['criterionActif'] <-"Calinski.Harabatz"
plotCriterion(listPart)
ordered(listPart)
plotCriterion(listPart)


}
\keyword{methods}
