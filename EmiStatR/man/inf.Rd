\name{inf}
\alias{inf}
\docType{data}
\title{
Dataset: An example of general input for infiltration characteristics in the study region.
}
\description{
This dataset is an example of general input for infiltration characteristics in the sewer system for the study region (Haute-Sure catchment, Grand-Duchy of Luxembourg). 
}
\usage{data("inf")}
\format{
  A list of 3 elements. 
  \describe{
    \item{\code{qf}:}{numeric, infiltration water inflow [l/(s*ha)];} 
    \item{\code{CODf}:}{numeric, chemical oxygen demand (COD) infiltration water pollution per capita (PE) load per day [g/(PE*day)];}
    \item{\code{NH4f}:}{numeric, ammonium (NH4) infiltration water pollution per capita (PE) load per day[g/(PE*day];} 
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
data("inf")

str(inf)
}

\keyword{datasets}
