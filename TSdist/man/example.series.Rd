\name{example.series}
\alias{example.series}
\alias{example.series1}
\alias{example.series2}
\alias{example.series3}
\alias{example.series4}
\alias{zoo.series1}
\alias{zoo.series2}

\docType{data}
\title{Example series.}
\description{
Example series saved as numeric vectors and as \code{zoo} objects.
}
\usage{
data(example.series1);
data(example.series2);
data(example.series3);
data(example.series4);
data(zoo.series1);
data(zoo.series2);
}

\format{
 \code{example.series1}, \code{example.series2}, \code{example.series3} and \code{example.series4} are saved in numerical vectors.
 
  \code{zoo.series1} and \code{zoo.series2} are saved as \code{zoo} objects with a given temporal index.
 
}


\details{
 \code{example.series1} and \code{example.series2} are generated based on the Two Patterns synthetic database introduced by Geurts (2002).
 
  \code{example.series3} and \code{example.series4} are two ARMA(3,2) series of coefficients AR=(1, -0.24, 0.1) and MA=(1, 1.2) and length 100 and 120 respectively. They are generated from a pair of innovation vectors obtained randomly from a normal distribution of mean 0 and standard deviation 1, but by setting different random seeds.
  
  
 \code{zoo.series1} and \code{zoo.series2} are copies of \code{example.series1} and \code{example.series2} but with a specific time index.
}

\references{
 Geurts, P. (2002). Contributions to decision tree induction: bias/variance tradeoff and time series classification. University of Liege, Belgium.
}
\examples{

data(example.series1);
data(example.series2);
data(example.series3);
data(example.series4);
data(zoo.series1);
data(zoo.series2);

## Plot series

plot(example.series1, type="l")
plot(example.series2, type="l")
plot(example.series3, type="l")
plot(example.series4, type="l")
plot(zoo.series1)
plot(zoo.series2)


}
\keyword{datasets}
