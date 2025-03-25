\name{rw}
\alias{rw}
\docType{data}
\title{
Dataset: An example of general input for rainwater characteristics in the study region.
}
\description{
This dataset is an example of general input for rainwater characteristics in the sewer system for the study region (Haute-Sure catchment, Grand-Duchy of Luxembourg). 
}
\usage{data("rw")}
\format{
  A list of 6 elements: 
  \describe{
    \item{\code{depth}:}{numeric, total rainfall amount of time series P1  [mm];} 
    \item{\code{pDur}:}{numeric, total rainfall duration  of time series P1 [min];} 
    \item{\code{CODr}:}{numeric, rainwater chemical oxygen demand (COD) concentration [mg/l];}
    \item{\code{NH4r}:}{numeric, rainwater ammonium (NH4) concentration [mg/l];} 
    \item{\code{stat}:}{character, raingauge station name for time series P1 [-];} 
    \item{\code{delta.t}:}{numeric, delta time for time series P1 [min];}
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
data("rw")

str(rw)
}

\keyword{datasets}
