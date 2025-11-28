\name{VotesGR}

\alias{VotesGR}

\docType{data}

\title{New Democracy and Total Votes in Greece in 2012}

\description{New Democracy and Total Votes per prefecture in the double parliamentary elections in Greece in May and June 2012, respectively}

\usage{data(VotesGR)}

\format{
  A data frame with 51 observations on the following 8 variables.
  \describe{
    \item{\code{MapCode2}}{a numeric vector of codes for joining this data to a map}
    \item{\code{NAME_ENG}}{a alphanumeric vector of prefecture names in greeklish}
    \item{\code{X}}{a numeric vector of x coordinates}
    \item{\code{Y}}{a numeric vector of y coordinates}
    \item{\code{NDJune12}}{a numeric vector of votes for New Democracy in June 2012 parliamentary elections}
    \item{\code{NDMay12}}{a numeric vector of votes for New Democracy in May 2012 parliamentary elections}    
    \item{\code{AllJune12}}{a numeric vector of total valid votes in June 2012 parliamentary elections}
    \item{\code{AllMay12}}{a numeric vector of total valid votes in May 2012 parliamentary elections}
  }
}

\details{The X,Y coordinates refer to the geometric centroids of the 51 Prefectures in Greece in 2011. All electoral districts in the Attica Region have been merged to one. The two electoral regions in Thessaloniki have also been merged to a single region matching the NUTS II regions geography.}

\source{The shapefile of the corresponding polygons is available from the Public Open Data of the Greek Government at \url{https://geodata.gov.gr/en/dataset/oria-nomon-okkhe}. The election results are available from the Hellenic Ministry of Interior.}

\references{
Georganos, S., Kalogirou, S. (2014) Spatial analysis of voting patterns of national elections in Greece, 10th International Congress of the Hellenic Geographical Society,  Aristotle University of Thessaloniki, Thessaloniki 22-24 October 2014.
}
\examples{
  data(VotesGR)
  plot(VotesGR$NDJune12,VotesGR$NDMay12)
  abline(0,1)
}

\keyword{datasets}
