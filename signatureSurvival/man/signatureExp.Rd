\name{signatureExp}
\alias{signatureExp}

\title{
Signature expression or signature score
}
\description{
Function signatureExp.R is used to give signature expression or score across all
patients in a cohort data by using weight vector of a signature.
}
\usage{
signatureExp(svdata,weight)
}
\arguments{
  \item{svdata}{
a survival dataset containing clinic data such as death or relapse status,
month, covariates age, sex and/smoking, stages, therapy etc. and a set of genes
containing subset of genes in the signature in column.
}
\item{weight}{
a matrix that contains gene and/or gene_id and
weight columns.
}
}
\details{
a matrix with \eqn{g \times (2 or 3)}. If weight contains two columns, then
first column must be gene or gene_id and the second column must be weight. If
weight has three columns, then the first two columns are gene and gene_id but
the third column must be weight. Gene name or gene_id in weigth must be matched
with gene name or gene_id in the survival data.
}
\value{
a survival dataset with signature scores of patients.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}
}

\seealso{
\code{\link{weight}}
}
\examples{
data(TCGA_survivalData)
data(signature_weight)
res1<-signatureExp(svdata=TCGA_survivalData,weight=signature_weight)
}

\keyword{ signature }
\keyword{ weight }

