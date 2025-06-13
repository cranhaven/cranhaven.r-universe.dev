\name{spatave.effect}
\alias{spatave.effect}

\title{
Spatial average
}
\description{
Performs average value of climatic variable over different sites 
}
\usage{
spatave.effect(var.mat, var.name, nstat = NULL, na.proc = FALSE)
}

\arguments{
  \item{var.mat}{
A data frame containing a variable named "dates" in chron format and different climatic variables for each time step and for each site
}
  \item{var.name}{
Character object specifying the name of the climatic variable for which we will perfom spatial average
}
  \item{nstat}{
Numeric vector to define the number of sites used to obtain climatic series
}
  \item{na.proc}{
A logical value indicating whether NA values should be stripped before the computation proceeds
}

}

\value{
An additional column will be added in the data frame introduced to contain the average of the variable of interest over the different sites for each time step
}

\examples{
temp_spatave <- spatave.effect(myclimatic_data, "temp", nstat = 3, na.proc = TRUE) 

}
