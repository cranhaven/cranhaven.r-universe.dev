\name{Breast}
\alias{Breast}
\title{Breast}
\usage{data(Breast)}
\description{
This dataset contains original clinical cases reported by Dr. Wolberg. The data are grouped chronologically, reflecting the time periods when the samples were collected. The dataset includes various attributes related to breast cancer diagnosis.
}
\format{
A data frame with 699 rows and several columns representing different features related to breast cancer diagnosis.
\itemize{
\item \code{Sample_code_number}: Identification number for the sample.
\item \code{Clump_Thickness}: 1-10
\item \code{Uniformity_of_Cell_Size}: 1-10
\item \code{Uniformity_of_Cell_Shape}: 1-10
\item \code{Marginal_Adhesion}: 1-10
\item \code{Single_Epithelial_Cell_Size}: 1-10
\item \code{Bare_Nuclei}: 1-10 (some values may be missing or revised)
\item \code{Bland_Chromatin}: 1-10
\item \code{Normal_Nucleoli}: 1-10
\item \code{Mitoses}: 1-10
\item \code{Class}: 2 (benign) or 4 (malignant)
}
}
\examples{
# Load the dataset
data(Breast)

# Print the first few rows of the dataset
print(head(Breast))
}
\keyword{datasets}
