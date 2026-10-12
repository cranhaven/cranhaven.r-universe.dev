\name{MusicTime}
\alias{MusicTime}
\docType{data}
\title{Estimating Time with Different Music Playing}
\description{
Estimates of 45 seconds with different music playing
}
\format{
  A data frame with 60 observations on the following 6 variables.
  \describe{
    \item{\code{MusicBg}}{Music playing in the background (\code{no} or \code{yes})}
    \item{\code{Subject}}{Code for each subject (\code{subj1} through \code{subj20})}
    \item{\code{Sex}}{Subject's sex (\code{f}=female or \code{m}=male)}
    \item{\code{TimeGuess}}{Subject's time estimating 45 seconds (in seconds)}
    \item{\code{Music}}{Type of music (\code{calm}, \code{control}, or \code{upbeat})}
    \item{\code{Accuracy}}{Absolute value of TimeGuess minus 45}
  }
}
\details{
Participants were asked to judge when 45 seconds had passed in silence (control), while listening to an upbeat song (Metropolis, by David Guetta and Nicky Romero), and while listening to a calm song (Bach's Das Wohltemperierte Klavier, Prelude in C Major). The order in which the three conditions were experienced was randomized for each participant.  Time until subject guessed 45 seconds had elapsed (TimeGuess) and the magnitude of the difference from 45 (Accuracy) were recorded.
}
\source{
Data collected by Ksenia Vlasov at Oberlin College.
}
\keyword{datasets}
