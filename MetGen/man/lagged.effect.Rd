\name{lagged.effect}
\alias{lagged.effect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
lag effect
}
\description{
Introduce lag effects for variables
}
\usage{
lagged.effect(var.mat, var.name, maxlag, nstat = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var.mat}{
A data frame containing a chron variable and different climatic variables for each time step 
}
  \item{var.name}{
Character object that forms the name of the climatic variable to be lagged
}
  \item{maxlag}{
Numeric vector that specifies the maximum amount of lag requested
}
  \item{nstat}{
Numeric vector that specifies the number of sites considered
}
}

\value{
lagged effect for the variable var.name, will be observed, according to the amount of maxlag defined in the data frame introduced 
}

\examples{
data_lagged4 <- lagged.effect(myclimatic_data, "temp",4, nstat=3)
data_lagged8 <- lagged.effect(myclimatic_data, "temp",8, nstat=3)
}
