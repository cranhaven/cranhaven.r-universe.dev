\name{qs_factor}
\alias{qs_factor}
\docType{data}
\title{
Dataset: An example of general input for the daily water consumption factors in the study region.
}
\description{
This dataset is an example of general input for the daily water consumption (qs) factors in the study region (Haute-Sure catchment, Grand-Duchy of Luxembourg). 
}
\usage{data("qs_factor")}
\format{
  A data.frame with 12 observations of 2 variables:
  \describe{
    \item{\code{time}:}{character, time at which the factors are reported [HH:MM:SS];} 
    \item{\code{qs_factor}:}{numeric, the corresponding factors for water consumption [-];}
    }
  }

%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%\url{http://agrimeteo.lu/}
%}
%\references{
%%%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data("qs_factor")

qs_factor
}

\keyword{datasets}
