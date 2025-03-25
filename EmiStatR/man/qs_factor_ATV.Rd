\name{qs_factor_ATV}
\alias{qs_factor_ATV}
\docType{data}
\title{
Dataset: An example of general input for the daily water consumption factors (ATV German guideline) in the study region.
}
\description{
This dataset is an example of general input for the daily water consumption (qs) factors according to the german guideline ATV-A134 applied in the study region (Haute-Sure catchment, Grand-Duchy of Luxembourg). 
}
\usage{data("qs_factor")}
\format{
  A data.frame with 12 observations of 2 variables:
  \describe{
    \item{\code{time}:}{character, time at which the factors are reported [HH:MM:SS];} 
    \item{\code{ATV.A134.Qf.m3_h}:}{numeric, the corresponding factors for water consumption according to the German guideline ATV-A134 [-];}
    }
  }

%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%\url{http://agrimeteo.lu/}
%}
%\references{
%
%}
\examples{
data("qs_factor_ATV")

qs_factor_ATV
}

\keyword{datasets}
