\name{weight}
\alias{weight}
\title{
Caculation of Weights for signature genes
}
\description{
Weights for signature genes are calculated from the results obtained by
performing Cox proportional hazard regression on the training survival datasets.
}
\usage{
weight(results,signature)
}

\arguments{
  \item{results}{
a list of multiple results obtained by performing survival analysis on the
training datasets.
}
\item{signature}{
a vector or matrix of genes screened by a method from survival data and
expression data.

}
}
\details{
The results file is a list of multiple results obtained by performing Cox
proportional hazard survival analysis based on univariate models in multiple
datasets. User can create this list by using results<-list(result1,result2,...,
resultn). Signature may be a matrix with two columns and n rows. One column may
be gene id(such as probe id or Ensembl id or NCBI Entrez id) and the other is
gene symbol or gene name. Signature may be a vector(a set of genes or gene ids).
}
\value{
Output a matrix where gene and/or gene id and weight in column and gene names
and/or gene id values and weight values.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}
}
\note{
Sum of weights may not be equal to 1 if some genes in results are lost.
}

\examples{
data(TS_signature)
data(results)
res<-weight(results = results,signature = TS_signature)
}

\keyword{ weight }
\keyword{ signature }
