\name{budworm}
\alias{budworm}
\docType{data}
\title{ Mortality data of moth larvae due to increasing doses of
  insecticide.
}
\description{
  Batches of twenty larvae were exposed to increasing doses of
  insecticide, and the number of survivors and their sexes were noted.
  These data were reported by Collett(1991) and used by Venables
  and Ripley(1994 and later editions). They resulted from an experiment
  to study the toxicity of a pyrethroid insecticide to the tobacco
  budworm \emph{Heliothis virescens} of different doses of the insecticide.
}
\usage{data(budworm)}
\format{
  A data frame with 12 observations on the following 3 variables:
  \describe{
    \item{\code{ldose}}{the log of the dose of the insecticide}
    \item{\code{dead}}{the number of budworms that were dead a day
later}
    \item{\code{sex}}{a factor with two levels: ``F'' and ``M''
  }
}
}
\source{
  Collett, D. 1991. Modelling Binary Data. Chapman and Hall, London.
}
\references{
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
  
  Venables, W.N. and Ripley, B.D. 1994. Modern Applied Statistics with
  S-PLUS. Springer Verlag, New York.
}
\examples{
data(budworm)
fit <- glm(cbind(dead, 20 - dead) ~ ldose, data=budworm,
family=binomial)
summary(fit)
}

\keyword{datasets}
