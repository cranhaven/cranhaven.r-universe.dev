\name{Sonar}
\alias{Sonar}
\title{Sonar}
\usage{data(Sonar)}
\description{
This dataset contains sonar signals bounced off a metal cylinder (mines) and a roughly cylindrical rock. The task is to classify whether the signal is from a mine or a rock based on the sonar signal patterns.
}
\format{
A data frame with 208 rows and 61 columns representing different features of sonar signals.
\itemize{
\item \code{Attribute1}: Continuous feature representing energy within a frequency band.
\item \code{Attribute2}: Continuous feature representing energy within a frequency band.
\item \code{Attribute3}: Continuous feature representing energy within a frequency band.
\item \code{...}: Additional continuous features (up to Attribute60).
\item \code{Class}: Categorical target variable ('M' for mine, 'R' for rock).
}
}
\examples{
# Load the dataset
data(Sonar)

# Print the first few rows of the dataset
print(head(Sonar))
}
\keyword{datasets}
