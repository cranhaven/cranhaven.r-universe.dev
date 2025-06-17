\name{component}
\alias{component}
\title{Component data}
\usage{
component
}
\description{
Component data is a complete dataset consists lifetimes of a component which is produced by four different suppliers. The lifetimes of the component distribute as the two-parameter exponential distribution.
}
\examples{
library(doex)
component$supplier;
component$lifetime;
}

\value{
  \item{lifetime}{A set of data on lifetimes of the components obtained from the different suppliers.}
  \item{supplier}{A set of suppliers produce the components.}
}
\author{Mustafa CAVUS}

