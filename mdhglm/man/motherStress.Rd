\name{motherStress}
\alias{motherStress}
\docType{data}
\title{Mother's Stress and Children's Morbidity Data}
\description{
Asar and Ilk (2014) considered the longitudinal data set from mother's stress and children's
morbidity study (MSCM). In MSCM study, 167 mothers and their preschool children were enrolled
for 28 days. Investigation of the serial dependence structures of the two
longitudinal responses suggested a weak correlation structure for the period
of days 1~16. Therefore, only the period of days 17~28 is considered
in this data set.
}
\usage{data("motherStress")}
\format{
  A data frame with 2004 observations on the following 14 variables.
  \describe{
    \item{\code{id}}{subjects identifier for 167 mothers}
    \item{\code{stress}}{mother's stress (0=absence, 1=presence)}
    \item{\code{illness}}{children's illness status (0=absence, 1=presence)}
    \item{\code{married}}{mother's marriage status (0=other, 1=married)}
    \item{\code{education}}{mother's highest education level (0=less than high school, 1=more than high school)}
    \item{\code{employed}}{mother's employment status (0=unemployed, 1=employed)}
    \item{\code{chlth}}{health status of children at baeline (0=very poor/poor, 1=fair, 2=good, 3=very good)}
    \item{\code{mhlth}}{health status of mother at baeline (0=very poor/poor, 1=fair, 2=good, 3=very good)}
    \item{\code{race}}{children's race (0=white, 1=non-white)}
    \item{\code{csex}}{children's gender (0=male, 1=female)}
    \item{\code{housize}}{household size (0=2-3 people, 1=more than 3 people)}
    \item{\code{bstress}}{the average mother's stress of the 1~16 days}
    \item{\code{billness}}{the average children's illnes of the 1~16 days}
    \item{\code{week}}{study time (=(day-22)/7)}
  }
}
\references{
Asar, O. and Ilk, O. (2013). mmm: An R package for analyzing multivariate
longitudinal data with multivariatemarginal models. Computer
Methods and Programs in Biomedicine, 112, 649--654.
}

