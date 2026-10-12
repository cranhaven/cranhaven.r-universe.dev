\name{Backgrounddata}
\alias{Backgrounddata}
\docType{data}
\title{CSV file with background/pseudoabsence data}
\description{
Longitude and latitude
values, as well as values of four environmental variables (from the Bio-ORACLE dataset;
  Tyberghein et al., 2012) for each of 10,000
background points. The background points were selected randomly along shorelines of all continents in the northern hemisphere.}
%\usage{
%backgroundlocations <- system.file("extdata", "Backgrounddata.csv", package="MaxentVariableSelection")

% should be used as input argument to the 'VariableSelection' function.
%}
\format{
  A data frame that specifies environmental conditions and geographic
  locations of 10,000 background sites.
  \describe{
    \item{\code{species}}{The species name is here set to 'bg', which
      stands for background}
    \item{\code{longitude}}{longitudinal coordinate}
    \item{\code{latitude}}{latitudinal coordinate}
    \item{\code{calcite}}{Mean calcite concentration (mol/m3)}
    \item{\code{parmean}}{Mean photosynthetically active radiation (Einstein/m2/day)}
    \item{\code{salinity}}{Mean salinity (PSS)}
    \item{\code{sstmax}}{Maximum sea surface temperature (degree celsius)}    
  }
}
\examples{
backgroundlocations <- system.file("extdata",
                                   "Backgrounddata.csv",
                                   package="MaxentVariableSelection")
backgroundlocations <- read.csv(backgroundlocations,header=TRUE)
head(backgroundlocations)
}
\keyword{datasets}
\references{
Tyberghein L, Verbruggen H, Pauly K, Troupin C, Mineur F and {De Clerck},
O (2012)\cr
Bio-ORACLE: a global environmental dataset for marine species distribution modelling\cr
\emph{Global Ecology and Biogeography} \bold{21}:2 272--281.
}
