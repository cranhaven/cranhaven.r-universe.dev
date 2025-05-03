\name{PCICt}
\alias{PCICt-package}
\docType{package}
\title{PCICt, a POSIXct work-alike for 360- and 365-day calendars}
\description{
  This package implements a work-alike to R's POSIXct class which
  implements 360- and 365-day calendars in addition to the gregorian
  calendar.
}
\details{
  Many global climate models (GCMs) and regional climate models (RCMs) are
  run using an idealized and simplified calendar which only includes 365
  days or 360 days per year.  When trying to do seasonal or monthly
  analysis on a set of models which use different calendar types, analyses
  may not be comparable unless one can normalize the calendars and the
  times which are represented therin. Thing get even more difficult when
  trying to compare model output with observations data which is located
  in a particular time on the Gregorian calendar.
  
  The PCICt package attempts to solve this problem by creating a new time
  type, PCICt, which inherits from the POSIXt type.  All of the
  functionality provided by POSIXt is also provided by PCICt, however
  PCICt does the work of normalizing the calendars and making points in
  time on seperate calendars cross comparable.
  
  365-day calendars are implemented using a 365-day non-leap year from a
  Gregorian calendar.

  360-day calendars are not implemented as 12 equal months of 30
  days. They are implemented as 12 months of the following lengths, in
  days, with the first month being January: 30, 28, 31, 30, 30, 30, 30,
  31, 30, 30, 30, 30. This was a decision to ease implementation.

  To map a 365_day calendar to Gregorian, PCICt simply drops February 29
  from leap years. To map a 360_day calendar to Gregorian, PCICt
  attempts  to remap the days such that the five lost days are
  distributed as equally as possible across the seasons (winter loses
  two days while, spring/summer/fall each lose one).
  
  There are a few problems with this implementation. As noted above,
  360-day calendars do not use equal months, which may cause problems in
  certain situations.

  Another problem originates from within R itself. Many functions in R
  strip attributes from data. If this happens to a PCICt object, it
  cannot be coerced back into a PCICt, as it is lacking the calendar
  attribute. This causes problems with several internal functions, like
  mean. PCICt includes a wrapper for mean. You will probably run into
  these problems. When you do, please use the wrapper for mean as a
  template for your wrapper.

  This package may be modified substantially in future to solve these problems.
}
\references{
  \url{http://www.pacificclimate.org}
}
\seealso{
\code{\link{as.PCICt}}
}
\keyword{climate}
\keyword{ts}
\keyword{calendar}
\keyword{date}
\keyword{time}
\keyword{chron}
\keyword{utilities}