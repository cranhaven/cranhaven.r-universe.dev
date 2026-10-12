\name{Eyes}
\alias{Eyes}
\docType{data}
\title{Pupil Dilation and Sexual Orientation}
\description{
Data from an experiment relating pupil dilation to sexual orientation.
}
\format{
  A data frame with 106 observations on the following 4 variables.
  \describe{
    \item{\code{DilateDiff}}{Difference in pupil dilation when looking at same-sex and opposite-sex nude photographs}
    \item{\code{Sex}}{\code{F}=female or \code{M}=male}
    \item{\code{Gay}}{\code{1}=gay or \code{0}=not, based on Kinsey scale score greater than 3}
    \item{\code{SexMale}}{{\code{0}=female or \code{1}=male}}
  }
}
\details{
DilateDiff is, essentially, the difference in pupil dilation when looking at (a) same-sex nudes and (b) opposite-sex nude photographs. More specifically, multiple measurements of pupil size were taken under each of the two conditions, together with a third condition that involved a neutral stimulus. Within-subject z-scores were then computed, which led to the DilateDiff numbers used here.
}
\source{
G. Rieger and R.C. Savin-Williams (2012),"The Eyes Have It: Sex and Sexual Orientation Differences in Pupil Dilation Patterns,"  in PLoS ONE. The full study included 325 students. Here we are analyzing a subset of the data that excludes White students.

}
\keyword{datasets}
