\name{divideUp}
\alias{divideUp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Partition data into mutiple nearly equal subsets}
\description{
Randomly partition data into nearly equal subsets. If
\code{balanced=TRUE} the requirement is imposed that the subsets
should as far as possible be balanced with respect to a classifying
factor.  The multiple sets are suitable for use for determining the
folds in a cross-validation.
}
\usage{
divideUp(cl, nset = 2, seed = NULL, balanced=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cl}{classifying factor}
  \item{nset}{number of subsets into which to partition data}
  \item{seed}{set the seed, if required, in order to obtain
reproducible results}
  \item{balanced}{logical: should subsets be as far as possible balanced
with respect to the classifying factor?}
}
\value{
a set of indices that identify the \code{nset} subsets
}
\author{John Maindonald}
\examples{
foldid <- divideUp(cl=rep(1:3, c(17,14,8)), nset=10)
table(rep(1:3, c(17,14,8)), foldid)
foldid <- divideUp(cl=rep(1:3, c(17,14,8)), nset=10,
       	    balanced=FALSE)
table(rep(1:3, c(17,14,8)), foldid)


## The function is currently defined as
function(cl = rep(1:3, c(7, 4, 8)), nset=2, seed=NULL, balanced=TRUE){
    if(!is.null(seed))set.seed(seed)
    if(balanced){
      ord <- order(cl)
      ordcl <- cl[ord]
      gp0 <- rep(sample(1:nset), length.out=length(cl))
      gp <- unlist(split(gp0,ordcl), function(x)sample(x))
      gp[ord] <- gp
    } else
    gp <- sample(rep(1:nset, length.out=length(cl)))
    as.vector(gp)
  }
}
\keyword{arith}% at least one, from doc/KEYWORDS

