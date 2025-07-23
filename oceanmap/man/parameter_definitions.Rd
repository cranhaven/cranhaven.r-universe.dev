\name{parameter_definitions}
\alias{parameter_definitions}
\docType{data}
\title{
parameter definitions dataframe
}
\description{
a dataframe containing definitions of parameters to plot or to save by \link{v}, \link{readbin} and \link{writebin}.
}
\usage{data(parameter_definitions)}
\format{
  data.frame
}
\value{
a dataframe with the following header, containing definitions of parameters to plot or to save by \link{v}, \link{readbin} and \link{writebin}:

\code{param a b c log name1 unit pal1 minv maxv min max invalid_data_dc coast_dc land_dc no_data_dc}

\item{param}{
character string indicating the keyword of a parameter.
}

\item{a,b,c}{
value for parameter parameter data conversion from/to byte data. (See \link{param_convert} and \link{param_unconvert})
}

\item{log}{
whether a logarithmic formula should be applied for data conversion (0 for \code{FALSE} and 1 for \code{TRUE}; See \link{param_convert} and \link{param_unconvert}).
}

\item{name}{
character string indicating the long name of a parameter.
}

\item{unit}{
character string or bgroup statement indicating the parameter unit.
}

\item{\code{pal1}}{
  default color map used by \code{v} calls on parameter related data.
}

\item{\code{minv, maxv}}{
  default minimum and maximum z-value used by \code{v} calls on parameter related data.
}

\item{\code{min, max}}{
  minimum and maximum byte-values to be considered when calculating absolute values.
}

\item{\code{invalid_data_dc, coast_dc, land_dc & no_data_dc}}{
 byte values used to mask invalid data, coast lines, land masses and missing data.
}
}


\keyword{parameter_definitions}

\author{
Robert K. Bauer
}

\seealso{\link{v}} 


\examples{
## Example
data(parameter_definitions)
head(parameter_definitions)

# selecting sea surface temperature parameter definition
parameter_definitions[parameter_definitions$param == "sst2",] 
}
