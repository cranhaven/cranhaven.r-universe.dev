\name{Iris}
\alias{Iris}
\title{Iris Data}
\usage{data(Iris)}
\description{
The Iris dataset is a classic and widely-used dataset in the field of machine learning and statistics. It contains measurements of sepal length, sepal width, petal length, and petal width for three species of iris plants. The dataset is commonly used for classification tasks.
}
\format{
A data frame with 150 rows and 5 columns representing different features of iris plants.
\itemize{
\item \code{Sepal.Length}: Sepal length in centimeters (continuous).
\item \code{Sepal.Width}: Sepal width in centimeters (continuous).
\item \code{Petal.Length}: Petal length in centimeters (continuous).
\item \code{Petal.Width}: Petal width in centimeters (continuous).
\item \code{Species}: Species of iris plant (categorical): Iris Setosa, Iris Versicolor, or Iris Virginica.
}
}
\examples{
# Load the dataset
data(Iris)

# Print the first few rows of the dataset
print(head(Iris))
}
\keyword{datasets}
