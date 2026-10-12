\name{HearingTest}
\alias{HearingTest}
\docType{data}
\title{Correctly Identified Words in a Hearing Test}
\description{
Percentaged of correctly identified words in a hearing test
}
\format{
  A data frame with 96 observations on the following 3 variables.
  \tabular{rl}{
    \code{Subj} \tab {Subject number (1 - 24)}\cr
    \code{List} \tab {List of words:  \code{L1} \code{L2} \code{L3}  \code{L4}}\cr
    \code{Percent} \tab {Percent (out of 50) of words correctly identified}\cr
  }
}
\details{
Audiologists use standard lists of 50 words to test hearing; the words are
calibrated, using subjects with normal hearing, to make all 50 words on the list equally hard
to hear. The goal of the study described here was to see how four such lists, denoted by L1-L4
in this dataset, compared when played at low volume with a noisy background. The response is the
percentage of words identified correctly.
}

\source{
Data downloaded from DASL at http://lib.stat.cmu.edu/DASL/Datafiles/Hearing.html.
}

\references{
Loven, F. (1981), "A Study of the Interlist Equivalency of the
CID W-22 Word List Presented in Quiet and in Noise." Unpublished MS Thesis,
University of Iowa. }

\keyword{datasets}
