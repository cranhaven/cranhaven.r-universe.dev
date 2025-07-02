\name{toydata}
\alias{toydata}
\docType{data}
\title{
A short example dataset
}
\description{
Dataset toydata contains repeated measurements made in 3 patients. It mimics randomized clinical trials data with two parallel arms with a repeated measurement of a time-varying binary covariate, which could be the time-varying confounder acting both on the survival and treatment censoring.
}
\usage{data("toydata")}
\format{
  A data frame with 3 observations on the following 12 variables.
  \describe{
    \item{\code{id}}{a numeric vector corresponding to the patient's identifier}
    \item{\code{randt}}{a vector containing the date of the randomization visit}
    \item{\code{lastdt}}{a vector containing the date of latest news}
    \item{\code{status}}{a numeric vector. The value equals to 1 if the patient dies at lastdt
        (and 0 otherwise)}
    \item{\code{age}}{a numeric vector containing patient’s age (in years) at randomization}
    \item{\code{ps1}}{a numeric vector containing the values (0 or 1) of a repeated measurement
        happening on date \code{randt}. Note that some of them could be missing}
    \item{\code{ps2}}{a numeric vector containing the values (0 or 1) of a repeated measurement
        happening on date \code{dt2}. Note that some of them could be missing}
    \item{\code{ps3}}{a numeric vector containing the values (0 or 1) of a repeated measurement
        happening on date \code{dt3}. Note that some of them could be missing}
    \item{\code{dt2}}{a vector containing the dates of measurement
        of \code{ps2}. Note that some of them could be missing}
    \item{\code{dt3}}{a vector containing the date of measurement
        \code{ps3}. Note that some of them could be missing}
    \item{\code{arm}}{a vector containing the patient’s randomized arm}
    \item{\code{swtrtdt}}{a vector containing the date when the patient initiates
        the other arm treatment (NA if does not happen)}
  }
}

\references{Graffeo, N., Latouche, A., Le Tourneau C., Chevret, S. (2019) "ipcwswitch: an R package for inverse probability of censoring weighting with an application to switches in clinical trials". Computers in biology and medicine, 111, 103339. doi : "10.1016/j.compbiomed.2019.103339"}


\examples{
data(toydata)
toydata
}
\keyword{datasets}
