\name{shark_data_raw}
\alias{shark_data_raw}
\docType{data}
\title{
Original shark data
}
\description{
Original shark data observations, unequally spaced in time
}
\usage{data(shark_data_raw)}
\format{
  A data frame with 68528 observations on the following 12 variables.
  \describe{
    \item{\code{tag}}{a factor with levels \code{GSH01} \code{GSH02} \code{GSH03} \code{GSH04} \code{GSH05} \code{GSH06} \code{GSH07} \code{GSH08} \code{GSH09} \code{GSH10} \code{GSH11} \code{GSH12} \code{GSH13} \code{GSH14} \code{GSH15} \code{GSH16} \code{GSH17} \code{GSH18} \code{GSH19} \code{GSH20} \code{GSH21} \code{GSH22}}
    \item{\code{X}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
    \item{\code{logvelocity}}{a numeric vector}
    \item{\code{bearing.to.east.tonext.rad}}{a numeric vector}
    \item{\code{turn.angle.rad}}{a numeric vector}
    \item{\code{state.guess2}}{a numeric vector}
    \item{\code{prev.guess2}}{a numeric vector}
    \item{\code{time_to_next}}{a numeric vector}
    \item{\code{dx_to_next}}{a numeric vector}
    \item{\code{dy_to_next}}{a numeric vector}
    \item{\code{date_as_sec}}{a numeric vector}
  }
}
\source{
Espinoza, Mario, Farrugia, Thomas J., and Christopher G. Lowe. Habitat use, movements
and site fidelity of the gray smooth-hound shark in a newly restored Southern
California estuary. Journal of Experimental Marine Biology and Ecology, 2011.
}
\examples{

##stored as separate integer and numeric variables for storage purposes

data(shark_data_raw, package="animalEKF")
shark_data <- do.call(cbind, shark_data_raw)
head(shark_data)

}
\keyword{datasets}
