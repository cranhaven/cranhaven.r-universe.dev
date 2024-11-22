\name{surfaceDistance}

\alias{surfaceDistance}


\title{Earth surface distance between two points}

\description{
The surfaceDistance() function calculates the distance in kilometers between any two lat-lon pairs
using the Haversine equation.
}

\usage{
surfaceDistance(lat1_deg, lon1_deg, lat2_deg, lon2_deg)
}

\arguments{
  \item{lat1_deg}{latitude 1 (degrees)}
  \item{lon1_deg}{longitude 1 (degrees)}
  \item{lat2_deg}{latitude 2 (degrees)}
  \item{lon2_deg}{longitude 2 (degrees)}
}

%% \details{ }

\value{
Distance in kilometers
}

\references{
\url{https://en.wikipedia.org/wiki/Haversine_formula}
}

\author{
Jonathan Callahan \email{jonathan@mazamascience.com}
}

%% \note{ }

%% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{ }


