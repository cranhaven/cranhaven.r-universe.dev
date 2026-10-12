\name{Handwriting}
\alias{Handwriting}
\docType{data}
\title{Guess Author's Sex from Handwriting?}
\description{
Survey data to see if subjects can guess author's sex from handwriting specimens
}
\format{
  A data frame with 204 observations on the following 8 variables.
  \describe{
    \item{\code{Individual}}{Survey Respondent Number }
    \item{\code{Gender}}{Gender of Respondent (\code{0} = male, \code{1} = female)}
    \item{\code{Survey1}}{Percent correct on Survey 1}
    \item{\code{Survey2}}{Percent correct on Survey 2}
    \item{\code{FemaleID}}{Percent correct in identifying female specimens on Survey 1}
    \item{\code{MaleID}}{Percent correct in identifying male specimens on Survey 1}
    \item{\code{Both}}{Percent correctly identified on Survey 1 AND Survey 2}
    \item{\code{DIFF}}{Survey1 - Survey2}
  }
}
\details{
Bradley and colleagues at Clarke University gave two identical surveys to a sample of 203 students (each student did the survey twice).  Each survey contains 25 writing specimens and students were asked to identify whether the author is male or female.  Of the 25 specimens, 12 are written by a female, 13 by a male.

An example of the survey form can be found at \cr
https://docs.google.com/forms/d/1sO6vlsozsORbqaCTsA7Ta0qZL7_6_MCEPJ7tYeKYyvI/viewform
}
\source{
Bradley, S., (2015), "Handwriting and Gender:  A Multi-use Dataset", JSE (Datasets and Stories). March 2015.

http://www.amstat.org/publications/jse/v23n1/bradley.pdf

}
\keyword{datasets}
