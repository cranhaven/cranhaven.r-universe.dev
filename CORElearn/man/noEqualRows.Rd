\name{noEqualRows}
\alias{noEqualRows}

\title{ Number of equal rows in two data sets }
\description{
Counts number of equal rows in two data sets. The two data sets shall have equal number of columns.
}
\usage{
 noEqualRows(data1, data2, tolerance=1e-5, countOnce=TRUE)
}

\arguments{
  \item{data1}{ The first data set.}
  \item{data2}{ The second data set.}
  \item{tolerance}{ Tolerated difference between two rows.}
  \item{countOnce}{ Shall each equal row in data1 be counted just once, or number of rows it is equal to in data2.}
}
\details{
Rows are compared using column-wise comparisons. The sum of differences up to a given tolerance are tolerated.
}
\value{
Integer value giving the count of equal instances.
}

\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{CORElearn}}.
}

\examples{
# uses two randomly generated data sets
set.seed(12345)
d1 <- classDataGen(100)
d2 <- classDataGen(100)
noEqualRows(d1, d2, tolerance=1e-4)
}


