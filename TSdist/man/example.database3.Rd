\name{example.database3}
\alias{example.database3}


\docType{data}
\title{Example synthetic database with series belonging to different classes.}
\description{
Example synthetic database with ARMA series belonging to 5 different classes.}

\usage{
data(example.database3);
}

\format{
 \code{example.database3} a list conformed of the following two elements:
 
 \itemize{
\item{data} {The 50 time series are stored in a numeric matrix, row-wise.}
\item{classes} {A numerical vector of length 50 that takes values in \{1,2,3,4,5\}. Each element in the vector represents the class of one of the series.}
}

}


\details{
 
  \code{example.database3} is a database conformed of 50 series of length 100 obtained from 5 different classes. Each class is obtained from a different initializations of an 
  ARMA(3,2) process of coefficients AR=(1,-0.24,0.1) and MA=(1,1.2). 
  
  Random noise is added to all the series by sampling values from a \eqn{N(0,1.7)} distribution. R is obtained from the following formula: 
    
  Finally, all the series in the database are shifted \eqn{sh} positions to the right or left, \eqn{sh} being a random integer value extracted from \eqn{{-15,...,15}} in each case.
}

\examples{

data(example.database3);


##  The "data" element of the list contains the time series, set in a row-wise format.

plot(example.database3$data)[1,]

##  The "classes" element in example.database3 contains the classes of the series: 

example.database3$classes
}
\keyword{datasets}
