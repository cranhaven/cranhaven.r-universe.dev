\name{stratified}
\alias{stratified}
\docType{data}
\title{  Results of CBS News Survey of 1447 Adults in the United States   }
\description{
   Results of CBS News survey of 1447 adults in the United States, divided into 
   16 strata.  The sampling is assumed to be proportional, so that the population 
   proportions \eqn{N_j/N}, are approximately equal to the sampling proportions, 
\eqn{n_j/n}.
}
\usage{data(stratified)}
\format{
  A data frame with 16 observations on the following 5 variables.
  \describe{
    \item{\code{region}}{a character vector}
    \item{\code{bush}}{proportio declaring to vote for Bush}
    \item{\code{dukakis}}{proportion declaring to vote for Dukakis}
    \item{\code{other}}{proportion declaring to vote for other}
    \item{\code{proportion}}{sample proportion}
  }
}
\examples{
data(stratified)
str(stratified)

}
\keyword{datasets}
