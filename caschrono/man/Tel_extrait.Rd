\name{Tel_extrait}
\alias{Tel_extrait}
\docType{data}
\title{Telephone consumption in a firm}
\description{
The file "Tel_extrait.csv" has been created by an automatic telephone exchange system in a firm; the date includes the day, the month and the year ordered like this : D, M, Y.
}
\format{The series is first imported as a \code{data.frame} object and then transformed into a \code{ts} object}
\examples{
don.mois1 <- read.csv2(file = system.file("/import/Tel_extrait.csv", package = "caschrono"),
  col.names = c("Date.app", "Heur.deb.app", "Code Dest", "Dest Det","Dur app sec.",
  "Mont app EU"), skip = 0, stringsAsFactors = FALSE)
}
\keyword{datasets}
