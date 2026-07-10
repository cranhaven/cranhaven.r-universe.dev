\name{ionosphere}
\alias{ionosphere}
\title{ionosphere Data}
\usage{data(ionosphere)}
\description{
This dataset contains radar returns from the ionosphere, collected by a system in Goose Bay, Labrador. The dataset is used for classifying radar returns as 'good' or 'bad' based on the presence of structure in the ionosphere.
}
\format{
A data frame with multiple rows and 35 columns representing different features related to radar returns.
\itemize{
\item \code{Attribute1}: Continuous feature.
\item \code{Attribute2}: Continuous feature.
\item \code{Attribute3}: Continuous feature.
\item \code{Attribute4}: Continuous feature.
\item \code{Attribute5}: Continuous feature.
\item \code{Attribute6}: Continuous feature.
\item \code{Attribute7}: Continuous feature.
\item \code{Attribute8}: Continuous feature.
\item \code{Attribute9}: Continuous feature.
\item \code{Attribute10}: Continuous feature.
\item \code{...}: Additional continuous features (up to Attribute34).
\item \code{Class}: Binary classification target ('good' or 'bad').
}
}
\examples{
# Load the dataset
data(ionosphere)

# Print the first few rows of the dataset
print(head(ionosphere))
}
\keyword{datasets}
