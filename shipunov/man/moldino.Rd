\name{moldino}
\alias{moldino}
\alias{moldino_l}
\title{moldino}

\description{
Observations on island floras.
Islands are located in the freshwater Moldino lake, Middle Russia.
Data collected in 2013.

'moldino_l' contains squares and GPS locations.

'moldino' contains the actual abundance data.
}

\usage{moldino}

\format{
\describe{
\item{\code{columns}}{Island names, data is abundance of plant species, in 1543 scale
(0 -- absent; 1 -- one individual plant; 2 -- no more than 12 individual plants (rametes);
3 -- number of individuals is more than 12 but no more than 5\% of total number of plants on a plot;
4 -- number of individuals is more than 5\% but no more than 25\% of total number of plants on a plot;
5 -- number of individuals is more than 25\% but no more than 50\% of total number of plants on a plot;
6 -- number of individuals is more than 50\% but no more than 75\% of total number of plants on a plot;
7 -- number of individuals is more than 75\% of total number of plants on a plot.)}
\item{\code{rows}}{Names of plant species}
\item{\code{NAME}}{Island name}
\item{\code{SQUARE}}{Island square, m2}
\item{\code{LAT}}{Latitude}
\item{\code{LON}}{Longitude}
}
}

\source{
Abramova L., Volkova P., Eliseeva E., Troshina A., Shipunov A.
2005--inward. The checklist of flora from environs of village Polukarpovo
(Tver region). See\cr
"http://ashipunov.info/shipunov/moldino/nauka/molflora.pdf".

Shipunov A., Abramova L. 2014. Islands in lakes and the sea: how do they
differ? European Journal of Environmental Sciences. 4: 112--115.
}

% \examples{
% ...
% }

\keyword{datasets}
