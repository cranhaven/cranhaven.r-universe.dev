\name{hessen}
\alias{hessen}
\docType{data}
\title{
Extract from the SyHD Project on the syntax of the dialect of Hessen (Germany)
}
\description{
An example dataset of dialect data.
}
\usage{data(hessen)}
\format{
List of 3
\describe{
  \item{\code{boundary}}{An object of type \code{owin} describing a geographical boundary. This format is necessary for Voronoi diagrams.}
  \item{\code{villages}}{A dataframe with two variables "longitude" and "latitude" for the 157 villages on this there is data in this dataset.}
  \item{\code{data}}{Dataframe with 56 different characteristics of these 157 villages, distributed over 15 different variables (as indicated in the column names).}
}
}
\source{
Data from \url{https://www.syhd.info/startseite/index.html}
}
\references{
Jürg Fleischer, Simon Kasper & Alexandra N. Lenz (2012): Die Erhebung syntaktischer Phänomene durch die indirekte Methode: Ergebnisse und Erfahrungen aus dem Forschungsprojekt "Syntax hessischer Dialekte" (SyHD). In: Zeitschrift für Dialektologie und Linguistik 79/1, 2-42.
}
\examples{
data(hessen)

tessalation <- weightedMap(hessen$villages, window = hessen$boundary, crs = 2397)
plot(tessalation$weightedVoronoi)
}
\keyword{datasets}
