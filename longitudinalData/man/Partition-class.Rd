\name{Partition-class}
\docType{class}
\alias{Partition-class}
\alias{[,Partition-method}
\alias{[<-,Partition-method}
\alias{show,Partition-method}
\alias{is.na,Partition-method}

\title{~ Class: Partition ~}
\description{
  An object of class \code{Partition} is a partition of a population
  into subgroups. The object also contains some information like the
  percentage of trajectories in each group or some qualities criterion.
}

\section{Objects from the Class}{
  Objects are mainly intend to be created by some clustering methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...)
}

\section{Slots}{
  \describe{
    \item{\code{nbClusters}}{[numeric]: number of groups, between 1 and 26}

    \item{\code{clusters}}{[vector(factor)]: vector containing the
      groups of each individual. Groups are in upper-case
      letters.}

    \item{\code{percentEachCluster}}{[vector(numeric)]: percentage of
      trajectories contained in each group.}

    \item{\code{postProba}}{[matrix(numeric)]: assuming that in
      each clusters C and for each time T, variable follow a normal law
      (mean and standard deviation of the variable at time T restricted
      to clusters C), then it is possible to compute the postterior
      probabilities of each individual (that is the probabilities that
      an individual has to belong to each clusters). These probabilities
      are hold in postProba.}

    \item{\code{postProbaEachCluster}}{[vector(numeric)]: for each
      clusters C, mean of the post probabilities to belong to C of the
      individual that effectively belong to C. A high percent means that
      the individual that are in this cluter realy meant to be here.}

    \item{\code{criterionValues}}{[vector(numeric)]: Value of the quality
      criterions used to evaluate the quality of the
      \code{Clustering}. See \code{\link{qualityCriterion}} for details.}

    \item{\code{details}}{[vector(character)]: hold different optionnal informations like
      the algorithm (if any) used to find the partition, the convergence
      time, the imputation methods, the starting condition.
      Examples:
      \code{details=c(algorithm="kmeans",convergenceTime="3")}.}
  }
}

\section{validation rules}{
  A class \code{Partition} object must follow some rules to be valid:
  \itemize{
    \item Slots should be either all empty, or all non empty.
    \item \code{nbClusters} has to be lower or equal to 26.
    \item \code{clusters} is a factor in \code{LETTERS[1:nbCluster]}.
  }
}


\section{Construction}{
  Class \code{Partition} objects are mainly constructed by some clustering methods
  (like k-means, fuzzy k-means, mixture modeling, latent class
  analysis,...). Neverdeless, it is also possible to construct them from
  scratch using the fonction \code{\link{partition}}.
}


\section{Get [}{
  \describe{
    \item{Object["nbClusters"]}{[numeric]: Gets the number of clusters
      (the value of the slot \code{nbClusters})}
    \item{Object["clusters"]}{[vector(factor)]: Gets the cluster of each
      individual (the value of the slot \code{clusters})}
    \item{Object["clustersAsInteger"]}{\code{[vector(integer)]}: Gets the
      cluster of each individual and turn them into integer}
    \item{Object["percentEachClusters"]}{\code{[vector(numeric)]}: Get the
      percent of individual
      in each clusters (the value of the slot \code{nbClusters})}
    \item{Object["postProbaEachClusters"]}{\code{[vector(numeric)]}: Get the
      post probabilities for each clusters.}
    \item{Object["postProba"]}{\code{[matrix(numeric)]}: Get the
      post probabilities for each individual and each clusters.}
    \item{Object["criterionValues"]}{\code{[vector(numeric)]}: gives the values
      of all the
      criterion values (the value of the slot \code{criterionValues})}
    \item{Object["details"]}{\code{[vector(character)]}: Get the values
      of the slot \code{details}.}
    \item{Object["XcriterionX"]}{\code{[numeric]}: Get the value of the criterion
      \code{XcriterionX}. It can be one of \code{Calinski.Harabatz},
      \code{Krzysztof.Calinski}, \code{Genolini.Calinski}, \code{Ray.Turi},
      \code{Davies.Bouldin}, \code{BIC}, \code{AIC}, \code{AICc} or \code{random}.}
    \item{Object["XspecialX"]}{\code{[character]}: Get the value named
      \code{XspecialX} in
      the slot \code{details} (probably one of \code{multiplicity},
      \code{convergenceTime}, \code{imputationMethod} or
      \code{algorithm}.)}
  }
}

\section{Setteur [<-}{
  \describe{
    \item{Object["multiplicity"]<-value}{[numeric]: In the slot
      \code{details}, sets the values names \code{multiplicity} to \code{value}.}
    \item{Object["convergenceTime"]<-value}{[numeric]: In the slot
      \code{details}, sets the values names \code{convergenceTime} to \code{value}.}
  }
  The others slot can not be change after the object creation.
}



\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}


\seealso{
  Overview: \code{\link{longitudinalData-package}} \cr
  Classes: \code{\linkS4class{LongData}} \cr
  Methods: \code{\link{partition}}\cr
}



\examples{
############
### Building Partition

### number
part <- partition(rep(c(1,2,1,3),time=3))

### LETTERS
part <- partition(rep(c("A","B","D"),time=4),details=c(convergenceTime="3",multiplicity="1"))

### Others don't work
try(partition(rep(c("A","Bb","C"),time=3)))

#############
### Setteur and Getteur

### '['
part["clusters"]
part["clustersAsInteger"]
part["nbClusters"]

### '[<-'
part["multiplicity"] <- 2
(part)
}


\keyword{classes}
\keyword{cluster}  % Clustering
