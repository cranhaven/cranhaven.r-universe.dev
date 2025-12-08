\name{KM}
\alias{KM}
\title{ plots a survival function with a Kaplan Meier variant}
\description{ plots the survival function of patients knowing censored data. Model : a variant of Kaplan-Meier's model    
}
\usage{
KM(fileName, toplot = TRUE, header = TRUE)
}
\arguments{
  \item{fileName}{ the file with the data use}
  \item{toplot}{ a logical variable to plot the result of this estimation }
  \item{header}{ a logical for : has the input file an header }
}

\value{
	No values returned, just plot the survival function and the theoretical distribution 
	of our model with exponential stays and a constant side effect probability
}


\author{ Christophe Dutang and Julie Barthes }

\examples{
#KM("data.rda",TRUE,TRUE)
}
\keyword{ htest }
