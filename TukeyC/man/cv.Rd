\name{cv}
\alias{cv}

\title{
  Coefficient of the experiment variation
}

\description{
  It obtains the coefficient of variation of the experiment obtained by 
  models \code{lm}, \code{aov} and \code{aovlist}.
}

\usage{
cv(x, 
   round=2)
}

\arguments{
  \item{x}{A object of the class \code{lm}, \code{aov} or \code{aovlist}.}
  \item{round}{An integer value indicating the number of decimal places to be used.
    The default value is 2.}
}

\details{
sqrt(MSerror)*100/mean(x)
}

\value{
  \item{x}{named numeric vector}
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\examples{
library(TukeyC)

## Completely Randomized Design (CRD - aov) 
data(CRD1)
av1 <- with(CRD1,
            aov(y ~ x,
                data=dfm))
summary(av1)
cv(av1)

## Randomized Complete Block Design (RCBD - aov)
data(RCBD)
av2 <- with(RCBD,
            aov(y ~ blk + tra,
                data=dfm))
summary(av2)
cv(av2)

## Split-plot experiment (SPE - aovlist)
data(SPE)
av3 <- with(SPE,
            aov(y ~ blk + P*SP + Error(blk/P),
                data=dfm))
summary(av3)
cv(av3)

## Split-split-plot experiment (SSPE - aovlist)
data(SSPE)
av4 <- with(SSPE,
            aov(y ~  blk + P*SP*SSP + Error(blk/P/SP),
                data=dfm))
summary(av4)
cv(av4)

## storing
res <- cv(av4)
res[2:3]
}

\keyword{univar}
\keyword{aov}
\keyword{coefficient of variation}
