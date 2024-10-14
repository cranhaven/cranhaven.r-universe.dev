\name{dipper.data}
\alias{dipper.data}
\alias{dipper.males}
\alias{dipper.histories}

\docType{data}
\title{European Dipper data}
\description{
Example capture-recapture data from a study of European dippers. 
}
\usage{data(dipper.data)}
\format{
  A data frame containing 294 capture histories and the sex designation of 
  birds captured.  Capture indicators are either 0 = not captured, 1 = captured, or 
  2 = captured but died and not released back into the population. Columns in the data frame are: 
  \describe{
    \item{\code{h1}}{a numeric vector indicating capture at occasion 1}
    \item{\code{h2}}{a numeric vector indicating capture at occasion 2}
    \item{\code{h3}}{a numeric vector indicating capture at occasion 3}
    \item{\code{h4}}{a numeric vector indicating capture at occasion 4}
    \item{\code{h5}}{a numeric vector indicating capture at occasion 5}
    \item{\code{h6}}{a numeric vector indicating capture at occasion 6}
    \item{\code{h7}}{a numeric vector indicating capture at occasion 7}
    \item{\code{males}}{a numeric vector indicating males. 1 = males, 0 = females}
    \item{\code{females}}{a numeric vector indicating females. 0 = males, 1 = females}
  }
} 

\details{
  This is a popular capture-recapture example data set. It has been analyzed by Lebreton et al. (1992)
  Amstrup et al. (2005) and others.   

  \code{dipper.males} is a vector indicating male birds.  I.e., \code{dipper.males <- dipper.data\$males}

  \code{dipper.histories} is a matrix of just the capture history columns \code{h1 - h7}, extracted 
	from \code{dipper.data} and made into a matrix. This matrix can be fed directly into 
    one of the estimation routines, such as \code{F.cjs.estim}.
    
  \bold{To access}: After loading the MRA library (with \code{library(mra)}) you must
  execute \code{data(dipper.data)}, \code{data(dipper.data)}, or \code{data(dipper.males)}   
  to get access to these data frames.  They are not attached when the library is loaded. 
 
}
\source{
  Amstrup, S. C., McDonald, T. L., and Manly, B. F. J. 2005. Handbook of Capture-Recapture Analysis. 
	Princeton University Press. [Chapter 9 has several examples 
	that use this data.] 

}

\examples{
data(dipper.data)
}
\keyword{datasets}
