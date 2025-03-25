\name{E2}
\alias{E2}
\docType{data}
\title{
Dataset: An example of combined sewer overflow (CSO) input for Kaundorf structure (CSO chamber, CSOC).
}
\description{
This dataset is an example of input data for the CSOC at Kaundorf, Grand-Duchy of Luxembourg. 
}
\usage{data("E2")}
\format{
  A list of 18 elements. This object contains the first 
    structure (CSOC) to simulate.
  \describe{
    \item{\code{id}:}{numeric, identification number [-];} 
    \item{\code{ns}:}{character, name of the structure [-];}
    \item{\code{nm}:}{character, name of the municipality [-];} 
    \item{\code{nc}:}{character, name of the catchment [-];}
    \item{\code{numc}:}{numeric, number of the catchment [-];}
    \item{\code{use}:}{character, use of the soil [-];}
    \item{\code{Atotal}:}{numeric, total area [ha];}
    \item{\code{Aimp}:}{numeric, impervious area [ha];}
    \item{\code{tfS}:}{numeric, time flow structure [time step];}
    \item{\code{pe}:}{numeric, population equivalent [PE];}
    \item{\code{V}:}{numeric, volume [m3];} 
    \item{\code{lev2vol}:}{list of 2, lev and vol, defining the curve lev (level [m]) 
    to vol (volume [m3]);}
    \item{\code{lev.ini}:}{numeric, initial level in the CSOC [m];}
    \item{\code{Qd}:}{numeric, maximum throttled outflow [l/s];} 
    \item{\code{Dd}:}{numeric, orifice diameter [m];}
    \item{\code{Cd}:}{numeric, orifice coefficient [-];}  
    \item{\code{Cimp}:}{numeric, coefficient for impervious area [-];}  
    \item{\code{Cper}:}{numeric, coefficient for pervious area [-].}
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
data("E2")

str(E2)
}

\keyword{datasets}
