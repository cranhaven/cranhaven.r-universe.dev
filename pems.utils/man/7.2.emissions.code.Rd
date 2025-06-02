\name{7.2.emissions.calculations}
\alias{7.2.emissions.calculations}
\alias{emissions.calculations}
\alias{emissions}
\alias{em}
\alias{calcEm}
\alias{calcEm_HoribaPitot}


\title{
  Emission calculations
}

\description{
  Functions associated with emissions calculations.
}

\usage{

calcEm(conc = NULL, calc.method = calcEm_HoribaPitot, 
          analyte = NULL, ..., data = NULL, fun.name = "calcEm", 
          force = FALSE, this.call = NULL)

calcEm_HoribaPitot(conc = NULL, time = local.time, exflow = exh.flow.rate, 
          extemp = exh.temp, express = exh.press, analyte = NULL, 
          delay = NULL, mm = NULL, ..., force = force, data = NULL, 
          fun.name = "calcEm_HoribaPitot", this.call = NULL)
}

\arguments{

  \item{conc}{
  (Data series, typically pems.element vector) Analyte/species concentrations, the main input for 
  calculating emissions. If \code{conc} is a concentration data series from a standard 
  \code{pems} source it should be named \code{conc.[analyte]} and \code{pems.utils} will 
  manage it accordingly. See below for further details.
}

  \item{time, exflow, extemp, express}{ 
  (Data series, typically pems.element vectors) Other inputs used when calculating emissions. The 
  combination depending on the calcuation method used (and set by \code{calc.method}).
} 

  \item{calc.method}{
  (Required function) The function to use to calculate emissions. (Default 
  \code{calcEm_HoribaPitot}). See below for further details.
}

  \item{analyte}{
  (Optional character vector) The analyte emissions are to be calculated for. If supplied,
  this is used as a reference when assigning molecule weight and other analyte properties 
  if these are not provided as part of calculate call. If not supplied, \code{pems.utils} 
  attempts to recover these from available sources, e.g. \code{data} if supplied as part 
  of the calculation call or package references such as \code{\link{ref.chem}}.   
}

  \item{\dots}{
  (Optional) Other arguments, currently passed on to function provided as 
  \code{calc.method} (default \code{calcEm_HoribaPitot}) and appropriate 
  \code{pems.utils} functions.
}

  \item{data}{
  (Optional \code{pems} object) The data source for inputs.
} 

  \item{fun.name, this.call, force}{
  (Various pems management functions) \code{fun.name} (character vector) the name of the 
  parent function, to be used in error messaging.  
  \code{this.call} the initial call (can generally be ignored). \code{force} 
  (Logical) Should \code{calcEm} and \code{calc.method} ignore any error checking, e.g. 
  units assignments, and do calculations anyway?
}

  \item{delay, mm}{
  (Optional numerics) Emissions calculation constants. \code{delay} is the time delay between 
  \code{conc} measurements and other timeseries. \code{mm} is the molecular mass of the 
  analyte. If supplied, these in-call values supercede any preset in e.g. package \code{look-up.tables}.
}

    
}


\details{

  \code{calcEm...} functions calculate emissions.

  \code{calcEm} is a wrapper function which is intended to provide a conventient front for 
  emissions calculation methods. It accepts an input \code{conc} which it checks and passes 
  on to \code{calc.method}, along with other supplied arguments. 

  \code{calcEm_HoribaPitot} calculates emissions using methods described in the Horiba OBS 
  Operators Manual. In addition to \code{conc}, the function requires the time, and exhaust 
  flow data series (measured by the OBS Pitot flow meter). By default, the function assumes 
  that these are default names that are generated for these when standard OBS files are 
  imported into \code{R} using the \code{pems.utils} import function \code{\link{importOBS2PEMS}}. 
  See References and Note below.  
}


\value{

  \code{calcEm_HoribaPitot} (and \code{calcEm} by default) use Horiba Manual methods to 
  calculate emissions (in g/s).
}

\references{
  
 \code{calcEm_HoribaPitot} uses methods described in:

 The Horiba Operators Manual.  

}
\author{
  Karl Ropkins
}
\note{

  \code{calcEm...} constants can be set/modified in the calculation call, e.g. 
  \code{calcEm(..., delay = [new.value])}. If not supplied, these are first checked for 
  in the associated \code{pems} object (if supplied), or set to default values. See 
  References. If analyte-related constants are to be added to a \code{pems} object, 
  these should be named in the format '[type].[analyte]', e.g. \code{delay.co} for 
  the \code{delay} constant to be used for the analyte CO.   
  
  Unit handling in \code{pems.utils} is via \code{\link{checkUnits}}, \code{\link{getUnits}}, 
  \code{\link{setUnits}} and \code{\link{convertUnits}}. See \code{\link{common.calculations}} 
  for details.
 
}


\seealso{
  See \code{\link{common.calculations}}.
}

\examples{

###########
##example 1 
###########

#basic usage

em.co <- calcEm(conc.co, data = pems.1)

#where the returned object, em.co, is a pems.element

}

\keyword{ methods }
