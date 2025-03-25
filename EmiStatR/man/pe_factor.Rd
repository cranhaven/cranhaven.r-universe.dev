\name{pe_factor}
\alias{pe_factor}
\docType{data}
\title{
Dataset: An example of general input for the daily population equivalent factors in the study region.
}
\description{
This dataset is an example of general input for the daily population equivalent (pe) factors in the study region (Haute-Sure catchment, Grand-Duchy of Luxembourg). 
}
\usage{data("pe_factor")}
\format{
  A data.frame with 12 observations of 2 variables:
  \describe{
    \item{\code{time}:}{character, time at which the factors are reported [HH:MM:SS];} 
    \item{\code{pe_factor}:}{numeric, the corresponding factors for population equivalents [-];}
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
data("pe_factor")

pe_factor
}

\keyword{datasets}
