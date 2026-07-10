\name{riboflavin}
\alias{riboflavin}
\docType{data}
\title{Riboflavin Production Data}
\description{
This dataset contains measurements of riboflavin (vitamin B2) production by Bacillus subtilis, a Gram-positive bacterium commonly used in industrial fermentation processes. The dataset includes
\eqn{n = 71} observations with \eqn{p = 4088} predictors, representing the logarithm of the expression levels of 4088 genes. The response variable is the log-transformed riboflavin production rate.
}
\usage{
data(riboflavin)
}
\format{
\describe{
\item{y}{Log-transformed riboflavin production rate (original name: \code{q_RIBFLV}). This is a continuous variable indicating the efficiency of riboflavin production by the bacterial strain.
}
\item{x}{A matrix of dimension \eqn{71 \times 4088} containing the logarithm of the expression levels of 4088 genes. Each column corresponds to a gene, and each row corresponds to an observation (experimental condition or time point).
}
}
}
\examples{
# Load the riboflavin dataset
data(riboflavin)

# Display the dimensions of the dataset
print(dim(riboflavin$x))
print(length(riboflavin$y))

}
\keyword{datasets}
