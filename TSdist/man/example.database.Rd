\name{example.database}
\alias{example.database}
\alias{example.database}
\alias{zoo.database}


\docType{data}
\title{Example databases.}
\description{
Example database saved both as a numeric matrix and as a \code{zoo} object.
}
\usage{
data(example.database);
data(zoo.database);
}

\format{
 \code{example.database} is saved in a numerical matrix.
 
  \code{zoo.database} is saved as a \code{zoo} object with a given temporal index.
 
}


\details{
 
  \code{example.database} is a numerical matrix conformed by six ARMA(3,2) series of coefficients AR=(1, -0.24, 0.1) and MA=(1, 1.2) and length 100 that are situated in a row-wise format. They are generated from innovation vectors obtained randomly from a normal distribution of mean 0 and standard deviation 1, but by setting different random seeds.
  
  
 \code{zoo.database} is a copy of \code{example.database} but saved in a \code{zoo} object with a specific time index. The series are set in a column-wise format.
}


\examples{

data(example.database);
data(zoo.database);


## In example.database the series are set in a row-wise format.

plot(example.database)[1,]

## In zoo.database the series are set in a column-wise format.

plot(zoo.database)[,1]

}
\keyword{datasets}
