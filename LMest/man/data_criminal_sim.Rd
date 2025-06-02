\name{data_criminal_sim}

\alias{data_criminal_sim}

\docType{data}

\title{Criminal dataset}

\description{Simulated dataset about crimes committed by a cohort of subjects.}

\usage{data(data_criminal_sim)}

\format{
  A data frame with 60000 observations on the following 13 variables.
  \describe{
    \item{\code{id}}{subject id}
    \item{\code{sex}}{gender of the subject}
    \item{\code{time}}{occasion of observation}
    \item{\code{y1}}{crime of type 1 (violence against the person)}
    \item{\code{y2}}{crime of type 2 (sexual offences)}
    \item{\code{y3}}{crime of type 3 (burglary)}
    \item{\code{y4}}{crime of type 4 (robbery)}
    \item{\code{y5}}{crime of type 5 (theft and handling stolen goods)}
    \item{\code{y6}}{crime of type 6 (fraud and forgery)}
    \item{\code{y7}}{crime of type 7 (criminal demage)}
    \item{\code{y8}}{crime of type 8 (drug offences)}
    \item{\code{y9}}{crime of type 9 (motoring offences)}
    \item{\code{y10}}{crime of type 10 (other offences)}
  }
}

\references{Bartolucci, F., Pennoni, F. and Francis, B. (2007), A latent Markov model for detecting patterns of criminal activity, \emph{Journal of the Royal Statistical Society, series A}, \bold{170}, pp. 115-132.}

\examples{data(data_criminal_sim)}

\keyword{datasets}
