\name{Wrinkle}
\alias{Wrinkle}
\docType{data}
\title{Moving Wet Objects with Wrinkled Fingers}
\description{
Results from an experiment to move wet/dry objects with wrinkled/dry fingers
}
\format{
  A data frame with 80 observations on the following 7 variables.
  \describe{
    \item{\code{Participant}}{Participant ID  (\code{p1} to \code{p20})}
    \item{\code{Time}}{Time to move objects (seconds)}
    \item{\code{Condition}}{\code{non-wrinkled/dry}, \code{non-wrinkled/wet}, \code{wrinkled/dry}, or \code{wrinkled/wet}}
    \item{\code{Fingers}}{Status of fingers (\code{non} or \code{wrinkled})}
    \item{\code{Objects}}{Status of objects (\code{dry} or \code{wet})}
    \item{\code{WrinkledThenNon}}{Wrinkled first? (\code{1}=yes or \code{1}=no)}
    \item{\code{DryThenWet}}{Dry first? (\code{1}=yes or \code{1}=no)}
  }
}
\details{
Each of 20 participants were measured doing a "transfer task" several times under each of four conditions. The transfer task was to pick up an item with the right hand thumb and index finger, pass the item through a small hole and grab it with the left hand, and then put the item into a box that had a hole in the lid. Sometimes the participant's fingers were wrinkled; sometimes the items were sitting in water.
}
\source{
Kareklas, Nettle, and Smulders (2013) "Water-induced finger wrinkles improve handling of wet objects", Biology Letters, http://dx.doi.org/10.1098/rsbl.2012.0999
}
\keyword{datasets}
