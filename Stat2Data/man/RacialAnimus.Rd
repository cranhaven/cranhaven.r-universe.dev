\name{RacialAnimus}
\alias{RacialAnimus}
\docType{data}
\title{Racial Animus and City Demgraphics}
\description{
Demographics and a measurement of racial animus in cities based on Google searches
}
\format{
  A data frame with 196 observations on the following 7 variables.
  \describe{
    \item{\code{MediaMarket}}{City (State)}
    \item{\code{Age65Plus}}{Percentage 65 and older}
    \item{\code{BachPlus}}{Percentage with a bachelor's degree}
    \item{\code{Black}}{Percentage of African-Americans}
    \item{\code{Hispanic}}{Percentage of Hispanics}
    \item{\code{ObamaKerry}}{Percentage of vote won by Obama in 2008 minus Kerry percentage in 2004}
    \item{\code{Animus}}{Measurement (0-250) of racial animus}
  }
}
\details{
Professor Seth Stephens-Davidowitz studies the level of racial animus across different areas in America by measuring the percent of Google search queries that include racially charged language. A measurement, Animus, is derived from his algorithm and is scaled to be between 0 (low racial animus) and 250 (high racial animus). The dataset includes those values along with demographic information about each media market.
}
\source{
Chae DH, Clouston S, Hatzenbuehler ML, Kramer MR, Cooper HLF, Wilson SM, et al. (2015) "Association between an Internet-Based Measure of Area Racism and Black Mortality, PLoS ONE 10(4): e0122963. doi:10.1371/journal.pone.0122963
}
\keyword{datasets}
