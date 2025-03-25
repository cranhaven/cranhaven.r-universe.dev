\name{EmiStatR-package}
\alias{EmiStatR-package}
\docType{package}
\title{Emissions and Statistics in R for Wastewater and Pollutants in Combined Sewer Systems.}
\description{
    Provides a fast and parallelised calculator 
    to estimate combined wastewater emissions. It supports the planning and 
    design of urban drainage systems, without the requirement of extensive 
    simulation tools. The 'EmiStatR' package  
    implements modular R methods. This enables to add new 
    functionalities through the R framework. 
}
\details{

The DESCRIPTION file:
\tabular{ll}{
Package: \tab EmiStatR  \cr
Type:    \tab Package   \cr
Version: \tab 1.2.3.0   \cr
Date:    \tab 2021-09-13\cr
License: \tab GPL (>= 3)\cr
Depends: \tab R (>= 2.10), methods, shiny \cr
Imports: \tab utils, grDevices, graphics, stats, xts, zoo, foreach, parallel, lattice, doParallel \cr
%~~ An overview of how to use the package, including the most important functions ~~
}
}
\author{
    J.A. Torres-Matallana [aut, cre]
    K. Klepiszewski [aut, cre]
    U. Leopold [ctb]
    G. Schutz [ctb]
    G.B.M. Heuvelink [ctb]
    

Maintainer: J.A. Torres-Matallana
}
\references{
J. A. Torres-Matallana, K. Klepiszewski, U. Leopold, and G.B.M. Heuvelink. EmiStatR: a simplified and
scalable urban water quality model for simulation of combined sewer overflows. Water, 10(6)(782):1-24,
2018. \url{https://www.mdpi.com/2073-4441/10/6/782/htm}
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
See also the class the method \code{\link{EmiStatR}}}

%\examples{
%~~ simple examples of the most important functions ~~
%}
