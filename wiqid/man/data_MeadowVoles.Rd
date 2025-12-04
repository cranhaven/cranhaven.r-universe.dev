\name{MeadowVoles}
\alias{MeadowVoles}
\docType{data}
\title{
Robust design mark-recapture data for meadow voles
}
\description{
Data for adult male meadow voles \emph{Microtus pennsylvanicus} from a live-trapping study at Patuxent Wildlife Research Center (Nichols et al 1984). Trapping was carried out for 5 consecutive nights each month for 6 months (June to December 1981).
}
\usage{data(MeadowVoles)}
\format{
  A data frame with 171 observations on the following 31 variables.
  \describe{
    \item{A1, A2, A3, A4, A5, B1, B2, B3, B4, B5, C1, C2, C3, C4, C5, D1, D2, D3, D4, D5, E1, E2, E3, E4, E5, F1, F2, F3, F4, F5}{a 1/0 array of capture data for voles for each of 5 occasions per month for 6 months. }
    \item{freq}{a column with -1/1, where -1 indicates that the animal was not released after the last recorded capture.}
  }
}
\details{
The data are provided as a data frame, such as would result from reading in data from a text file. Further formatting is needed before using these for analysis: see the examples. 
}
\source{
Williams, Nichols, Conroy (2002) \emph{Analysis and Management of Animal Populations: Modeling, Estimation, and Decision Making} Academic Press, San Diego CA
}
\references{
Nichols, Pollock, Hines (1984) The use of a robust capture-recapture design in small mammal population studies: A field example with \emph{Microtus pennsylvanicus.} \emph{Acta Theriologica} 29:357-365.
}
\examples{
data(MeadowVoles)

# Extract detection histories:
DH <- MeadowVoles[, 1:30]
freq <- MeadowVoles$freq

survRD(DH, freq=freq, occsPerSeason=5)

}
\keyword{datasets}
