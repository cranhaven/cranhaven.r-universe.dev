\name{cow}
\alias{cow}
\docType{data}
\title{ Data from an Experiment with Treatment Assignment Based on Covariates  }
\description{
  An experiment was conducted on 50 cows to estimate the effect of a
  feed additive (methionine hydroxy analog) on six outcomes related to 
  the amount of milk fat produced by each cow.  Four diets (treatments) were
  considered, corresponding to different levels of the additive, 
  and three
  variables were recorded before treatment assignment:  
  lactation number ( seasons
  of lactation), age, and initial weight of cow. 
  Multiple randomizations were calculated, and choosen that one with 
  `best balane', however that was defined.  
}
\usage{data(cow)}
\format{
  A data frame with 50 observations on the following 10 variables.
  \describe{
    \item{\code{level}}{diet, treatment}
    \item{\code{lactation}}{lactation number, pretreatment}
    \item{\code{age}}{age of cow, pretreatment variable}
    \item{\code{initial.weight}}{initial weight, pretreatment}
    \item{\code{dry}}{response}
    \item{\code{milk}}{response}
    \item{\code{fat}}{response}
    \item{\code{solids}}{response}
    \item{\code{final.weight}}{response}
    \item{\code{protein}}{response}
  }
}
\examples{
data(cow)
summary(cow)
names(cow)
# Investigating balance on pretreatment variables:
with(cow, tapply(lactation, level, mean))
with(cow, tapply(age, level, mean))
}
\keyword{datasets}
\concept{randomization}
