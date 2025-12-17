\name{riboflavinv100}
\alias{riboflavinv100}
\docType{data}
\title{Riboflavin Production Data (Top 100 Genes)}
\description{
This dataset is a subset of the riboflavin production data by Bacillus subtilis, containing \eqn{n = 71} observations. It includes the response variable (log-transformed riboflavin production rate) and the 100 genes with the largest empirical variances from the original dataset.
}
\usage{
data(riboflavinv100)
}
\format{
\describe{
\item{y}{Log-transformed riboflavin production rate (original name: \code{q_RIBFLV}). This is a continuous variable indicating the efficiency of riboflavin production by the bacterial strain.
}
\item{x}{A matrix of dimension \eqn{71 \times 100} containing the logarithm of the expression levels of the 100 genes with the largest empirical variances.
}
}
}
\examples{
# Load the riboflavinv100 dataset
data(riboflavinv100)

# Display the dimensions of the dataset
print(dim(riboflavinv100$x))
print(length(riboflavinv100$y))

}
\keyword{datasets}
