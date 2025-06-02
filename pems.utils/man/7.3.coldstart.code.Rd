\name{7.3.coldstart.code}
\alias{7.3.coldstart.code}
\alias{coldstart.code}
\alias{coldstart}
\alias{fitColdStart}
\alias{coldStartPlot}
\alias{panel.coldStartPlot1}
\alias{panel.coldStartPlot2}

\title{
  Cold Start Emissions related code
}

\description{
  Functions associated with Cold Start Emissions calculations.
}

\usage{

#calculations

fitColdStart(em, time, engine.on = NULL, 
             data = NULL, method = 2, ...,
             fun.name="fitColdStart")

#Cold Start Plots

coldStartPlot(time, em = NULL,  
             ..., data = NULL, engine.on = NULL,
             plot.type = 1, method = 2,
             fun.name="coldStartPlot")

panel.coldStartPlot1(..., loa.settings = FALSE)
panel.coldStartPlot2(..., loa.settings = FALSE)

}

\arguments{

  \item{em, time}{
  (Typically pems.element vectors) \code{em} is the emissions 
  data-series that the cold start contribution should be 
  estimated for; \code{time} is the associated time-series, 
  typically a local time measurement in seconds. 
} 
  \item{engine.on}{
  (Optimal, single Numeric) The time the emission source, e.g. 
  monitored vehicle engine, was started. If not supplied, this 
  is assumed to be start of the supplied \code{em} and 
  \code{time} data-series. See also Notes.
}

  \item{data}{
  (Optional, typically \code{pems}) The data source for 
  \code{em} and \code{time}.
} 

  \item{method}{
  (Optinal, Numeric) The method to use when fitting and 
  calculating the cold start contribution: method \code{1} 
  Single break point fit of accumulated emissions; 
  method \code{2} modified break-point. If not supplied, 
  method 2 is used by default.See also Notes and References.  
}  

  \item{\dots}{
  (Optional) Other arguments, currently passed on as supplied 
  to assoicated calculation or plotting function, or passed 
  back to pemsPlot. 
}

  \item{fun.name}{
  (Optional character) The name of the parent function, to be 
  used in error messaging.
}

  \item{plot.type}{
  (Optional numeric) For \code{coldStartPlot}, the type of cold 
  start plot to generate: 1 a conventional accumulation profile;
  or 2 an emission time-series. If not supplied, plot type 1 is 
  selected by default. 
}

  \item{loa.settings}{
  (Logical) For \code{coldStartPlot} panel functions, 
  a \code{loa} plot argument that can typically be 
  ignored by plot users. 
}

}


\details{

  \code{fitColdStart} fits a cold start model to the supplied 
  emissions and time-series data. 
  
  \code{coldStartPlot} generates a plot of the cold start model. 
  
  \code{panel.coldStartPlot1} and \code{panel.coldStartPlot1} 
  are plot panels used by coldStartPlot when generating 
  \code{plot.type}s 1 and 2, respectively.
  
}


\value{

  \code{fitColdStart} generates a cold start contribution 
  report as a \code{pems} dataset. 

  \code{coldStartPlot} generates a cold start contribution 
  report as a \code{lattice} plot. 

}

\note{
  
  Regarding \code{engine.on}: This is specifically the time 
  the engine is turned on rather than the row of data set 
  where this happens. In some cases, they are same, e.g. when 
  the data is logged at a regular 1-Hz and data capture is 
  complete. 
  
  Regarding \code{method}: Method 1 (break-point) and method 2 
  (modified break-point) are based on the identification of a
  change point in the accumulated emissions profile. 
  
  [Doc further] 
  
  (See References.) 

}

\seealso{

  See \code{\link{common.calculations}} (and 
  \code{\link{checkUnits}} and 
  \code{\link{convertUnits}}) for details of data management.
}

\author{
  Karl Ropkins
}

\references{
  
 \code{fitColdStart} uses methods described in:

 [Heeb]
 
 [Ropkins cold start]

}


\examples{

###########
##example 1 
###########

#basic usage

#to do/maybe not run... time to compile...

}

\keyword{ methods }
