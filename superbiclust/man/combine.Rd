\name{combine}
\alias{combine}
\title{
Combine two Biclust objects into one
}
\description{
Combine two Biclust objects into one
}
\usage{
combine(x,y)
}

\arguments{
  \item{x}{
1st Biclust object containing bicluster results
}
  \item{y}{
2nd Biclust object containing bicluster results
}
}
\details{
If a biclust function returns empty set, joined result contains only results of non-empty object. 
Info and Parameters slots of a "Biclust" object contain information about both biclustering runs.
}
\value{
object of a class Biclust
}
\author{
Tatsiana Khamiakova
}
\seealso{
\code{\linkS4class{BiclustSet}} 
}
\examples{
#combine output of two biclust objects
 test <- matrix(rnorm(5000), 100, 50)
 test[11:20,11:20] <- rnorm(100, 3, 0.1)
 test[17:26,21:30] <- rnorm(100, 3, 0.1)
 set.seed(1)
 PlaidRes1 <- biclust(x=test, method=BCPlaid())
 set.seed(2)
 PlaidRes2 <- biclust(x=test, method=BCPlaid())
 combinedRes <- combine(PlaidRes1,PlaidRes2)
 summary(combinedRes)
}