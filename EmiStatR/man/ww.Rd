\name{ww}
\alias{ww}
\docType{data}
\title{
Dataset: An example of general input for wastewater characteristics in the study region.
}
\description{
This dataset is an example of general input for wastewater characteristics in the sewer system for the study region (Haute-Sure catchment, Grand-Duchy of Luxembourg). 
}
\usage{data("ww")}
\format{
  A list of 3 elements. 
  \describe{
    \item{\code{qs}:}{numeric, infiltration inflow [l/(s*ha)];} 
    \item{\code{CODs}:}{numeric, chemical oxygen demand (COD) sewage pollution per capita (PE) load per day [g/(PE*day)];}
    \item{\code{NH4s}:}{numeric, ammonium (NH4) sewage pollution per capita (PE) load per day  [g/(PE*day];} 
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
data("ww")

str(ww)
}

\keyword{datasets}
