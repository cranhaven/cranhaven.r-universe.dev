\name{ListPartition-class}
\docType{class}
\alias{ListPartition}
\alias{ListPartition-class}
\alias{[,ListPartition-method}
\alias{[,ListPartition-methods}
\alias{[,ListPartition-ANY,ANY,ANY}
\alias{[,ListPartition-ANY,ANY,ANY-method}
\alias{[,ListPartition-ANY,ANY,ANY-methods}
\alias{[<-,ListPartition-method}
\alias{[<-,ListPartition-ANY,ANY,ANY}
\alias{ListPartition_show}
\alias{show,ListPartition-method}
\alias{listPartition}
\alias{listPartition-method}

\title{~ Class: ListPartition ~}
\description{
  An object of class \code{ListPartition} contain several liste of
  \code{Partition} sorted by cluster numbers.
}

\section{Objects from the Class}{
  Objects are mainly design to store the numerous \code{Partition} found
  by \code{kml} or \code{kml3d}.
}

\section{Slots}{
  \describe{
    \item{\code{criterionActif}}{[character]: Store the criterion name
      that will be used by fonctions that need a single criterion
      (like \link{plotCriterion} or \link{ordered}).}
    \item{\code{initializationMethod}}{[vector(chararcter)]: list all
      the initialization method that has allready been used to find some
      \code{Partition}
      (usefull to not run several time a deterministic method).}
    \item{\code{sorted}}{\code{[logical]}: are the \code{Partition}
      curently hold in the object sorted in decreasing (or increasing, according to
      \code{criterionActif}) order ?}
    \item{\code{c1}}{[list(Partition)]: list of \code{Partition} with 1 clusters.}
    \item{\code{c2}}{[list(Partition)]: list of \code{Partition} with 2 clusters.}
    \item{\code{c3}}{[list(Partition)]: list of \code{Partition} with 3 clusters.}
    \item{\code{c4}}{[list(Partition)]: list of \code{Partition} with 4 clusters.}
    \item{\code{c5}}{[list(Partition)]: list of \code{Partition} with 5 clusters.}
    \item{\code{c6}}{[list(Partition)]: list of \code{Partition} with 6 clusters.}
    \item{\code{c7}}{[list(Partition)]: list of \code{Partition} with 7 clusters.}
    \item{\code{c8}}{[list(Partition)]: list of \code{Partition} with 8 clusters.}
    \item{\code{c9}}{[list(Partition)]: list of \code{Partition} with 9 clusters.}
    \item{\code{c10}}{[list(Partition)]: list of \code{Partition} with 10 clusters.}
    \item{\code{c11}}{[list(Partition)]: list of \code{Partition} with 11 clusters.}
    \item{\code{c12}}{[list(Partition)]: list of \code{Partition} with 12 clusters.}
    \item{\code{c13}}{[list(Partition)]: list of \code{Partition} with 13 clusters.}
    \item{\code{c14}}{[list(Partition)]: list of \code{Partition} with 14 clusters.}
    \item{\code{c15}}{[list(Partition)]: list of \code{Partition} with 15 clusters.}
    \item{\code{c16}}{[list(Partition)]: list of \code{Partition} with 16 clusters.}
    \item{\code{c17}}{[list(Partition)]: list of \code{Partition} with 17 clusters.}
    \item{\code{c18}}{[list(Partition)]: list of \code{Partition} with 18 clusters.}
    \item{\code{c19}}{[list(Partition)]: list of \code{Partition} with 19 clusters.}
    \item{\code{c20}}{[list(Partition)]: list of \code{Partition} with 20 clusters.}
    \item{\code{c21}}{[list(Partition)]: list of \code{Partition} with 21 clusters.}
    \item{\code{c22}}{[list(Partition)]: list of \code{Partition} with 22 clusters.}
    \item{\code{c23}}{[list(Partition)]: list of \code{Partition} with 23 clusters.}
    \item{\code{c24}}{[list(Partition)]: list of \code{Partition} with 24 clusters.}
    \item{\code{c25}}{[list(Partition)]: list of \code{Partition} with 25 clusters.}
    \item{\code{c26}}{[list(Partition)]: list of \code{Partition} with 26 clusters.}
  }
}


\section{Construction}{
  Class \code{ListPartition} objects are mainly constructed by
  \code{kml}.
  Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{listPartition}} that does
  create an empty object.
}


\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{If 'xxx' is 'cX',
  'initializationMethod', 'sorted'
      or 'criterionActif', get the value of the field
      \code{xxx}.}
    \item{\code{object['criterionValues',j]}}{Give the values of the
      criterion 'j' for all the Partitions. The result is return as a
      list. If 'j' is missing, the criterion actif is used.}
    \item{\code{object['criterionValuesAsMatrix',j]}}{Give the values of the
      criterion 'j' for all the Partitions. The result is return as a
      matrix. If 'j' is missing, the criterion actif is used.}
    \item{\code{object['xxx']}}{If 'xxx' is a criterion, this is
      equivalent to object['criterionValuesAsMatrix','xxx']}

    \item{\code{object['initializationMethod']<-value}}{Set the field to
      \code{value}}
    \item{\code{object['criterionActif']<-value}}{If 'value' is one of
      CRITERION_NAMES, it sets the field to the criterion 'value'.}
    \item{\code{object['add']<-value}}{If 'value' is an object of class
      'Partition', then value is added to
      the Partition already hold in the field 'cX'. Note that a Partition with
      'X' clusters is automatiquely added to the correct list 'cX' according to its number of
      clusters.}
    \item{\code{object['clear']<-'cX'}}{Clear the list 'cX'.}
    \item{\code{listPartition}}{Constructor. Build an empty object.}
    \item{\code{ordered}}{Order the Partition according to the criterion
      actif.}
    \item{\code{regroup}}{Order then merge identical Partition (usefull
      to reduce the size of the \code{ListPartition})}
  }
}


\section{Author}{
Christophe Genolini^\{1,2\}\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] Christophe M. Genolini and Bruno Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011
}


\seealso{
  Classes: \code{\linkS4class{LongData}} \cr
  Methods: \code{\linkS4class{Partition}}\cr
}



\examples{
##############
### Preparing data
data(artificialLongData)
traj <- as.matrix(artificialLongData[,-1])

### Some clustering
part2 <- partition(rep(c("A","B"),time=100),traj)
part3 <- partition(rep(c("A","B","C","A"),time=50),traj)
part3b <- partition(rep(c("A","B","C","B","C"),time=40),traj)
part4 <- partition(rep(c("A","B","A","C","D"),time=40),traj)


################
### ListPartition
listPart <- listPartition()
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
regroup(listPart)
plotCriterion(listPart)
plotAllCriterion(listPart)
}


\keyword{classes}
\keyword{cluster}  % Partition
\keyword{ts}       % Time Series
