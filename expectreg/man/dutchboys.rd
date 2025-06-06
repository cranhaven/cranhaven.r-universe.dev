\name{dutchboys}
\Rdversion{1.1}
\alias{dutchboys}
\docType{data}
\title{
Data set about the growth of dutch children
}
\description{
Data from the fourth dutch growth study in 1997.
}
\usage{data(dutchboys)}
\format{
  A data frame with 6848 observations on the following 10 variables.
  \describe{
    \item{\code{defnr}}{identification number}
    \item{\code{age}}{age in decimal years}
    \item{\code{hgt}}{length/height in cm}
    \item{\code{wgt}}{weight in kg}
    \item{\code{hc}}{head circumference in cm}
    \item{\code{hgt.z}}{z-score length/height}
    \item{\code{wgt.z}}{z-score weight}
    \item{\code{hc.z}}{z-score head circumference}
    \item{\code{bmi.z}}{z-score body mass index}
    \item{\code{hfw.z}}{z-score height for weight}
  }
z-scores were calculated relative to the Dutch references.  
}
\details{
The Fourth Dutch Growth Study is a cross-sectional study that measures growth and
development of the Dutch population between ages 0 and 21 years. The study is a follow-up
to earlier studies performed in 1955, 1965 and 1980, and its primary goal is to
update the 1980 references.
}
\source{
van Buuren S and Fredriks A (2001)
\emph{Worm plot: A simple diagnostic device for modeling growth reference curves}
Statistics in Medicine, 20:1259-1277
}
\references{
Schnabel S and Eilers P (2009)
\emph{ Optimal expectile smoothing }
Computatational Statistics and Data Analysis, 53: 4168-4177 
}

\examples{
data(dutchboys)

expreg <- expectreg.ls(dutchboys[,3] ~ rb(dutchboys[,2],"pspline"),smooth="f",
                       estimate="restricted",expectiles=c(.05,.5,.95))
plot(expreg)
}
\keyword{datasets}
