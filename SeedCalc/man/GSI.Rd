\name{GSI}
\alias{GSI}
\title{Germination Speed Index}
\usage{
GSI(time,nger)
}

\arguments{
  \item{time}{ A \code{vector} object containing the time for germination. }
  \item{nger}{ A \code{vector} object containing the accumulated number of seeds germinated at each time. }
}


\description{
Calculates the Germination Speed Index (Maguire, 1982).
}

\references{
MAGUIRE, J. D. Speed of germination-aid selection and evaluation for seedling emergence and vigor. Crop Science, v. 2, p. 176–177, 1962.
}

\examples{
time <- c(1,2,3,4,5,6,7,8,9,10)
nger <- c(0,2,4,15,25,38,45,50,50,50)
GSI(time,nger)
}

