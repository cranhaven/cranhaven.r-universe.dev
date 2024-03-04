\name{extr_par}
\alias{extr_par}

\title{
Extracts the fitted distribution parameters to be introduced in other function
}
\description{
Auxiliar function
}
\usage{
extr_par(x, dist)
}

\arguments{
  \item{x}{
List of parameters obtained by the aplication of the Fit_dist_parameter function
}
  \item{dist}{
Name of the distribution we would like to stract the parameters
  ("norm",  "lnorm",  "geom", "exp",  "pois", "gamma",  
                     "cauchy",  "logis",  "weibull", "nbinom", "beta",
                     "chisq",  "t", "f")
}
}



\author{
F. Barrio-Parra
}

\examples{
a <- rnorm(n=100, mean =10, sd = 1) 

b <- Fit_dist_parameter(a)

extr_par(x = b, dist ="norm")

}

\keyword{ methods }