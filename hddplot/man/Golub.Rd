\name{Golub}
\alias{Golub}
\docType{data}
\title{Golub data (7129 rows by 72 columns), after normalization}
\description{
These are a normalized version of the Golub leukemia data from the
\code{golubEsets} package, available from:

\url{https://www.bioconductor.org/packages/release/data/experiment/}
}
\usage{data(Golub)}
\format{
  Numeric matrix: 7129 rows by 72 columns.
}
\details{
Data have been normalized and are supplied, here, as a matrix.
}
\source{See the help page for the dataset \code{golubMerge}, in the
\code{golubEsets} package, for details of the source of the original
data.
}
\references{
Molecular Classification of Cancer: Class Discovery and Class Prediction by 
Gene Expression Monitoring, Science, 531-537, 1999, T. R. Golub and 
D. K. Slonim and P. Tamayo and C. Huard and M. Gaasenbeek and J. P. Mesirov 
and H. Coller and M.L. Loh and J. R. Downing and M. A. Caligiuri and 
C. D. Bloomfield and E. S. Lander
}
\examples{
data(Golub)
## Select 20 rows from the data; show boxplots of variation across chips
boxplot(data.frame(t(Golub[sample(1:7129, 20), ]))) 
}
\keyword{datasets}
