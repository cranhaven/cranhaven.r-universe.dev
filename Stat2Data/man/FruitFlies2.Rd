\name{FruitFlies2}
\alias{FruitFlies2}
\docType{data}
\title{Fruit Fly Sexual Activity and Male Competition}
\description{
Results from an experiment on male fruit flies with different levels of sexual activity and competition from other males
}
\format{
  A data frame with 201 observations on the following 7 variables.
  \describe{
    \item{\code{Mated}}{Was the fly allowed mating opportunities? (\code{n} or \code{y})}
    \item{\code{Alone}}{Did the fly live alone? (\code{y}=yes or \code{n}= no, lived near another male)}
    \item{\code{Mating}}{How many mating opportunities was the fly given?}
    \item{\code{Total}}{Total duration of mating time over all opportunities (in seconds)}
    \item{\code{Size}}{Size of the thorax (in mm)}
    \item{\code{Lifespan}}{Lifespan (in hours, starting at the 12th day)}
    \item{\code{Activity}}{Number of times a movement detector was tripped starting in the 12th day}
  }
}
\details{
Researchers randomly assigned virgin male fruit flies to one of two treatments:  live alone or live in an environment where they can sense one other male fly.  Flies were randomly allocated to either have mating opportunities with female flies or to not have such opportunities.  Those flies that were given mating opportunities were given 3, 4, or 5 opportunities to mate (Mating measures this number).  Researchers also measured size, lifespan and activity levels of the fruit flies.
}
\source{
The file we are using is the link called survival at \cr
http://rsbl.royalsocietypublishing.org/content/suppl/2013/02/25/rsbl.2012.1188.DC1.html

The article talking about the data is at \cr
http://rsbl.royalsocietypublishing.org/content/9/2/20121188.full
}
\keyword{datasets}
