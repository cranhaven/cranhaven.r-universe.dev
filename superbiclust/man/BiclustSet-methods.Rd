\name{BiclustSet-methods}
\docType{methods}
\alias{BiclustSet-methods}
\alias{BiclustSet,ANY-method}
\alias{BiclustSet,Biclust-method}
\alias{BiclustSet,Factorization-method}
\alias{BiclustSet,list-method}
\title{Constructor of BiclustSet object}
\description{
 The method extract relevant information from a variety of biclustering input and constructs a BiclustSet object
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
}

\item{\code{signature(x = "Biclust")}}{
Converts Biclust objects into BiclustSet object
}

\item{\code{signature(x = "Factorization")}}{
Converts FABIA Factorization object into BiClustSet
}

\item{\code{signature(x = "list")}}{
Converts  a list with biclustering results into BiClustSet
}
}}
\seealso{
\code{\linkS4class{BiclustSet}}
}
\examples{

 test <- matrix(rnorm(5000), 100, 50)
 test[11:20,11:20] <- rnorm(100, 3, 0.1)
 test[17:26,21:30] <- rnorm(100, 3, 0.1)
#Run FABIA
 set.seed(1)
 FabiaRes1 <- fabia(test)
#construct BiclustSet object from FABIA output
 FabiabiclustSet <- BiclustSet(FabiaRes1)
 FabiabiclustSet 
}