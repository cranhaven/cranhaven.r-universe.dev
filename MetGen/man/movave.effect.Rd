\name{movave.effect}
\alias{movave.effect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving average
}
\description{
Averaging previous time steps using different bandwidth to perform temporal auto-correlation 

}
\usage{
movave.effect(var.mat, var.name, bw, nstat = NULL, na.proc = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var.mat}{
Data frame containing chron variable and different climatic variables for each time step
}
  \item{var.name}{
character object to define the name of the climatic variable that we will perform the moving average
}
  \item{bw}{
Numeric vector that forms the bandwidth to be used for the moving average 
}
  \item{nstat}{
Numeric vector specifying the number of gauged stations used to obtain observation data
}
  \item{na.proc}{
A logical value indicating whether NA values should be stripped before the computation proceeds
}
}

\value{
An additional column will be added in the data frame introduced containing the average of the variable of interest over previous time step specified by the bandwith for each site
}

\examples{
temp_movave <- movave.effect(myclimatic_data, "temp", 48, nstat = 3, 
na.proc = TRUE) 
}
