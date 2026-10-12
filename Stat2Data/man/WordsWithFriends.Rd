\name{WordsWithFriends}
\alias{WordsWithFriends}
\docType{data}
\title{Words with Friends Scores}
\description{
Results from the online game Words with Friends (solo play)
}
\format{
  A data frame with 444 observations on the following 11 variables.
  \describe{
    \item{\code{Points}}{Number of points scored by the author}
    \item{\code{OppPoints}}{Number of points scored by opponent ("solo")}
    \item{\code{WinMargin}}{Points minus OppPoints, so margin of victory (or loss)}
    \item{\code{Start}}{Did the author go first or pass? (\code{first} or \code{pass})}
    \item{\code{Ss}}{Number of S tiles (0 to 5)}
    \item{\code{BlanksNumber}}{Number of Blank tiles (0 to 2)}
    \item{\code{J}}{Did the author get the J tile? (\code{1}=yes, \code{0}=no)}
    \item{\code{Q}}{Did the author get the Q tile? (\code{1}=yes, \code{0}=no)}
    \item{\code{X}}{Did the author get the X tile? (\code{1}=yes, \code{0}=no)}
    \item{\code{Z}}{Did the author get the Z tile? (\code{1}=yes, \code{0}=no)}
    \item{\code{Blanks}}{Number of Blank tiles (\code{0blanks}, \code{1blank}, or \code{2blanks})}
  }
}
\details{
Results collected from one of the authors playing the "solo" mode of Words with Friends.
}
\source{
Author's iPhone
}
\keyword{datasets}
