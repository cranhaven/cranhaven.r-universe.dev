\name{6.1.common.calculations}
\alias{6.1.common.calculations}
\alias{common.calculations}
\alias{calcDistance}
\alias{calcSpeed}
\alias{calcAccel}
\alias{calcAcceleration}
\alias{calcJerk}
\alias{calcChecks}
\alias{calcPack}

\title{
  Common calculations
}

\description{
  Various common calculations associated with PEMS data.
}

\usage{

calcDistance(speed = NULL, time = NULL, data = NULL, 
          ..., fun.name = "calcDistance")

calcSpeed(distance = NULL, time = NULL, data = NULL, 
          ..., fun.name = "calcSpeed")

calcAccel(speed = NULL, time = NULL, data = NULL, 
          ..., method = 2, fun.name = "calcAccel")

calcAcceleration(speed = NULL, time = NULL, data = NULL, 
          ..., method = 2, fun.name = "calcAccel")

calcJerk(accel = NULL, time = NULL, data = NULL, 
          ..., fun.name = "calcJerk")

#associated

calcChecks(fun.name = "calcChecks", ..., data = NULL,
          if.missing = c("stop", "warning", "return"), 
          output = c("input", "data.frame", "pems", "special"),
          unit.conversions = NULL, overwrite = FALSE)

calcPack(output = NULL, data = NULL, settings = NULL, 
          fun.name = "calcPack", this.call = NULL)

}




\arguments{

  \item{speed, time, distance, accel}{
  (Required data series typically vectors) The inputs to use when doing a calculation. These 
  can typically be vectors or elements in either a \code{data.frame} or \code{pems} object.
} 

  \item{data}{
  (Optional \code{data.frame} or \code{pems} object) The data source if either a \code{data.frame} 
  or \code{pems} object is being used.
} 

  \item{\dots}{
  (Optional) Other arguments, currently passed on to \code{calcChecks}.
}

  \item{fun.name}{
  (Optional character) The name of the parent function, to be used in error messaging.
}

  \item{if.missing, output, unit.conversions, overwrite, settings, this.call}{
  (Various) Along with \code{data} and \code{fun.name}, arguments used by \code{calcCheck} and 
  \code{calcPack} to manage error and unit handling and workhorse \code{calc...} operations. These 
  are typically passed to the appropriate \code{check...} or \code{...Units} function for evaluation. 
  See Details, Note and Examples below. 

}

  \item{method}{
  (Character) Currently for \code{calcAccel} only. The method to use if options exist. 
}
    
}


\details{

  With the exception of \code{calcChecks} and \code{calcPack}, \code{calc...} functions 
  are common calculations.

  \code{calcDistance} calculates distance (in m) using speed and time.    

  \code{calcSpeed} calculates speed (in m/s) using distance and time.

  \code{calcAccel} calculates acceleration (in m/s/s) using speed and time.

  \code{calcJerk} calculates jerk (rate of change of acceleration in m/s/s/s) using 
  acceleration and time.

  By default results are returned as \code{pems.elements}. Other options include 
  returning as the supplied data plus the results as either a \code{data.frame} or a 
  \code{pems} object. See Note below. 

  Unit management is by \code{\link{convertUnits}}. See Note below. 

  The extra functions \code{calcChecks} and \code{calcPack} are add-ins that anyone can use 
  to develop other similiar functions. They are added at the start and end of standard 
  \code{calc...} functions to provide an option to use with third-party code. See Note.  
 
}


\value{

  With the exception of \code{calcChecks} and \code{calcPack}, all \code{calc...} functions 
  return either a \code{pems.element} vector, \code{data.frame} or \code{pems} object, depending 
  on \code{output} setting and \code{data} supplied.

}

\references{
  References in preparation.
}
\author{
  Karl Ropkins
}
\note{

  Unit handling in \code{pems.utils} is via \code{\link{checkUnits}}, \code{\link{getUnits}}, 
  \code{\link{setUnits}} and \code{\link{convertUnits}}. Allowed unit conversion methods have 
  to be defined in \code{\link{ref.unit.conversions}} or a locally defined alternative supplied 
  by the user. See \code{\link{convertUnits}} for an example of how to locally work with unit 
  conversions.

  \code{calc.dist} and \code{calc.accel} are alternatives to \code{calcDistance} and \code{calcAccel}. 

  The functions \code{calcChecks} and \code{calcPack} are currently under revision and likely to be 
  replaced in future versions of \code{pems.utils}.
         
}

%\section{Warning }{
%  No warnings
%}

\seealso{
  \code{\link{calcVSP}} for VSP calculations. \code{\link{calcEm}} for emissions calculations.
}

\examples{

###########
##example 1 
###########

#basic usage

#calculated accel as pems.element

calcAccel(velocity, local.time, pems.1)

#answer returned as suppied pems + calculated accel

calcAccel(velocity, local.time, pems.1, output = "pems")

#or, if you would rather... 
\dontrun{
pems.1$accel <- calcAccel(velocity, local.time, pems.1)
}

###########
#example 2
###########

#making wrappers for routine data processing 

my.pems <- list(pems.1, pems.1)

sapply(my.pems, function(x) 
                  calcAccel(velocity, local.time, data=x))

#ans = accel data series for each pems in my.pems list

#             [,1]        [,2]
# [1,]          NA          NA
# [2,]  0.00000000  0.00000000
# [3,]  0.05555556  0.05555556
# [4,]  0.00000000  0.00000000
# [5,] -0.02777778 -0.02777778
# [6,]  0.05555556  0.05555556
#       ....

#note:
#sapply if you can/want to simiplify outputs
#lapply if you want to keep output as a list of answers

}

\keyword{ methods }
