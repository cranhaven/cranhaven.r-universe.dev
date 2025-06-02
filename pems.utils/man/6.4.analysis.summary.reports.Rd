\name{6.4.analysis.summary.reports}
\alias{6.4.analysis.summary.reports}
\alias{summary.reports}
\alias{summaryReport}


\title{
  Generating summary reports
}

\description{
  Various functions for generating summary reports for \code{pems} objects.
}

\usage{

summaryReport(speed = NULL, time = NULL, accel = NULL, 
          distance = NULL, data = NULL, ..., 
          lod.speed = 0.1, lod.accel = 0.1,
          fun.name = "summaryReport")

}




\arguments{

  \item{speed, accel, time, distance}{
  (Data series typically vectors) The inputs to use when doing a calculation. These 
  can typically be vectors or elements in either a \code{data.frame} or \code{pems} object if 
  supplied as \code{data}. See Details below regarding requirements.
} 

  \item{data}{
  (Optional \code{data.frame} or \code{pems} object) The data source if either a \code{data.frame} 
  or \code{pems} object is being used.
} 

  \item{\dots}{
  (Optional) Other arguments, currently passed on to \code{\link{calcChecks}} which in turn provides 
  access to \code{pems.utils} management arguments such as \code{if.missing} and unit handlers such 
  as \code{unit.conversions}.
}

  \item{lod.speed, lod.accel}{
  (numerics) The limits of detection for speed and accel measurements, respectively. [Note: if only one 
  value is given for accel, the accel limits are assumed to be c(-lod.accel, + lod.accel)].
}

  \item{fun.name}{
  (Optional character) The name of the parent function, to be used in error messaging.
}

    
}


\details{

  \code{summaryReport} does not strictly require all the arguments \code{speed}, \code{accel}, \code{time} 
  and \code{distance} as inputs. It calculates as many of the missing cases as it can using the 
  \code{\link{common.calculations}} before halting an analysis or warning the user of any problems.

  Unit management is by \code{\link{convertUnits}}. See Note below.  
 
}


\value{

  \code{summaryReport} returns a one-row \code{data.frame} with twelve elements:

   \code{distance.travelled.km} this total distance travelled (in km)

   \code{time.total.s} the total time taken (in s)

   \code{avg.speed.km.h} the mean speed as averaged across the total journey/dataset 
   (in km/h)  

   \code{avg.running.speed.km.h} the mean speed while the vehicle was in motion (in km/h), 
   assuming a 0.01 km/h accuracy for \code{speed} measurements.
 
   \code{time.idle.s} and \code{time.idle.pc}, the time the vehicle was idling (in s and as a 
   percentage, respectively), also assuming a 0.01 km/h cutoff for \code{speed} measurements. 

   \code{avg.accel.m.s.s} the mean (positive component of) acceleration (in m/s/s), 
   assuming a 0.1 m/s/s cutoff for \code{accel} measurements.

   \code{time.accel.s} and \code{time.accel.pc}, the time the vehicle was accelerating (in s and as a 
   percentage, respectively), also assuming a 0.1 m/s/s cutoff for \code{accel} measurements. 

   \code{avg.decel.m.s.s} the mean deceleration (negative component of acceleration in m/s/s), 
   assuming a -0.1 m/s/s cutoff for \code{accel} measurements.

   \code{time.decel.s} and \code{time.decel.pc}, the time the vehicle was decelerating (in s and as a 
   percentage, respectively), also assuming a -0.1 m/s/s cutoff for \code{accel} measurements. 




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
}

\section{Warning }{
  Currently, \code{summaryReport} outputs have units incorporated into their names because the 
  outputs themselves are unitless \code{data.frames}.  
}

\seealso{
 
  \code{\link{checkUnits}} and \code{\link{convertUnits}} for data management.
}
\examples{

###########
##example 1 
###########

#basic usage

summaryReport(velocity, local.time, data=pems.1)

#   distance.travelled.km time.total.s avg.speed.km.h avg.running.speed.km.h
# 1              6.186056         1000        22.2698               28.78538
#   time.idle.s time.idle.pc avg.accel.m.s.s time.accel.s time.accel.pc
# 1          40            4       0.7921279          271          27.1
#   avg.decel.m.s.s time.decel.s time.decel.pc
# 1      -0.9039449          238          23.8

#apply to multiple cases

my.pems <- list(pems.1, pems.1)

sapply(my.pems, function(x) 
                    summaryReport(velocity, local.time, data = x))


#                        [,1]       [,2]      
# distance.travelled.km  6.186056   6.186056  
# time.total.s           1000       1000      
# avg.speed.km.h         22.2698    22.2698   
# avg.running.speed.km.h 28.78538   28.78538  
# time.idle.s            40         40        
# time.idle.pc           4          4         
# avg.accel.m.s.s        0.7921279  0.7921279 
# time.accel.s           271        271       
# time.accel.pc          27.1       27.1      
# avg.decel.m.s.s        -0.9039449 -0.9039449
# time.decel.s           238        238       
# time.decel.pc          23.8       23.8  




}

\keyword{ methods }
