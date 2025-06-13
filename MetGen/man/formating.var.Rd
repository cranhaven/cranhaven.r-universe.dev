\name{formating.var}
\alias{formating.var}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format variables
}
\description{
Extraction of a variable of interest for each site separetly
}
\usage{
formating.var(var.mat, var.name, nstat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var.mat}{
data frame containing chron variable, geographical information and different climatic variables for each time step and for each site
}
  \item{var.name}{
character object that forms the name of the climatic variable to be picked up from the data frame according to sites
}
  \item{nstat}{
numeric vector that contains the number of gauged sites included in the data frame
}
}

\value{
a new data frame is returned with time steps and temporal series of the variable of interest arranged in columns according to sites
}

\examples{
nstat <- 3
temp_mat <- formating.var(myclimatic_data,"temp", nstat)
}

