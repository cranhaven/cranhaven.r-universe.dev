\name{leukemia}
\alias{leukemia}
\docType{data}
\title{
Leukemia cancer gene expression data
}
\description{
 Gene expression data for two classes: Acute lymphoblastic leukemia (ALL) and Acute myeloid leukemia (AML), over n=72 observations, and d=7128 genes. 
}
\usage{data("leukemia")}
\format{
  A list containing the following items:
  \describe{
    \item{\code{class}:}{a vector of class labels}
    \item{\code{X}    :}{72 by 7128 matrix, gene expressions for each observation}
  }
}

\source{
http://statweb.stanford.edu/~ckirby/brad/LSI/datasets-and-programs/datasets.html
}

\examples{
data(leukemia)
}

\keyword{datasets}
