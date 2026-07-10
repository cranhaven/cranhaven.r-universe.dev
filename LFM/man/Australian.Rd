\name{Australian}
\alias{Australian}
\title{Australian}
\usage{data(Australian)}
\description{
This dataset contains information about credit card applications. All attribute names and values have been changed to meaningless symbols to protect confidentiality. The dataset includes a mix of continuous and categorical attributes, with some missing values.
}
\format{
A data frame with 690 rows and 15 columns representing different features related to credit card applications.
\itemize{
\item \code{A1}: Categorical - 0, 1 (formerly: a, b)
\item \code{A2}: Continuous
\item \code{A3}: Continuous
\item \code{A4}: Categorical - 1, 2, 3 (formerly: p, g, gg)
\item \code{A5}: Categorical - 1 to 14 (formerly: ff, d, i, k, j, aa, m, c, w, e, q, r, cc, x)
\item \code{A6}: Categorical - 1 to 9 (formerly: ff, dd, j, bb, v, n, o, h, z)
\item \code{A7}: Continuous
\item \code{A8}: Categorical - 1, 0 (formerly: t, f)
\item \code{A9}: Categorical - 1, 0 (formerly: t, f)
\item \code{A10}: Continuous
\item \code{A11}: Categorical - 1, 0 (formerly: t, f)
\item \code{A12}: Categorical - 1, 2, 3 (formerly: s, g, p)
\item \code{A13}: Continuous
\item \code{A14}: Continuous
\item \code{A15}: Class attribute - 1, 2 (formerly: +, -)
}
}
\examples{
# Load the dataset
data(Australian)

# Print the first few rows of the dataset
print(head(Australian))
}
\keyword{datasets}
