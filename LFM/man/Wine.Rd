\name{Wine}
\alias{Wine}
\title{Wine Data}
\usage{data(Wine)}
\description{
The Wine dataset contains the results of a chemical analysis of wines grown in the same region in Italy but derived from three different cultivars. The analysis determined the quantities of 13 constituents found in each of the three types of wines. This dataset is commonly used for classification tasks to determine the origin of wines based on their chemical properties.
}
\format{
A data frame with 178 rows and 14 columns representing different features of wines.
\itemize{
\item \code{Class}: Categorical target variable indicating the type of wine (1, 2, or 3).
\item \code{Alcohol}: Continuous feature representing the alcohol content.
\item \code{Malic_acid}: Continuous feature representing the malic acid content.
\item \code{Ash}: Continuous feature representing the ash content.
\item \code{Alcalinity_of_ash}: Continuous feature representing the alcalinity of ash.
\item \code{Magnesium}: Integer feature representing the magnesium content.
\item \code{Total_phenols}: Continuous feature representing the total phenols content.
\item \code{Flavanoids}: Continuous feature representing the flavanoids content.
\item \code{Nonflavanoid_phenols}: Continuous feature representing the nonflavanoid phenols content.
\item \code{Proanthocyanins}: Continuous feature representing the proanthocyanins content.
\item \code{Color_intensity}: Continuous feature representing the color intensity.
\item \code{Hue}: Continuous feature representing the hue.
\item \code{OD280_OD315_of_diluted_wines}: Continuous feature representing the OD280/OD315 of diluted wines.
\item \code{Proline}: Continuous feature representing the proline content.
}
}
\examples{
# Load the dataset
data(Wine)

# Print the first few rows of the dataset
print(head(Wine))
}
\keyword{datasets}
