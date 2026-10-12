\name{Occurrencedata}
\alias{Occurrencedata}
\docType{data}
\title{CSV file with occurrence data}
\description{
  Longitude and latitude values, as well as values of four environmental variables (from the Bio-ORACLE dataset;
  Tyberghein et al., 2012) for each of 98 occurrence sites (locations
  where a species was recorded).
}
%\usage{
%occurrencelocations <- system.file("extdata", "Occurrencedata.csv", package="MaxentVariableSelection")
% should be used as input argument to the 'VariableSelection' function.
%}
\format{
  A data frame that specifies geographic
  locations and environmental conditions of 98 occurrence sites.
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
occurrencelocations <- system.file("extdata",
                                   "Occurrencedata.csv",
                                   package="MaxentVariableSelection")
occurrencelocations <- read.csv(occurrencelocations,header=TRUE)
head(occurrencelocations)
}
\keyword{datasets}
\references{
Tyberghein L, Verbruggen H, Pauly K, Troupin C, Mineur F and {De Clerck},
O (2012)\cr
Bio-ORACLE: a global environmental dataset for marine species distribution modelling\cr
\emph{Global Ecology and Biogeography} \bold{21}:2 272--281.
}