\name{Jurors}
\alias{Jurors}
\docType{data}
\title{Reporting Rates for Jurors}
\description{
Reporting rates for bi-weekly jury pools in Franklin County Court (Columbus, OH).
}
\format{
  A data frame with 52 observations on the following 4 variables.
  \tabular{rl}{
    \code{Period} \tab {Sequential 2-week periods ove the course of a year}\cr
    \code{PctReport} \tab {Percentage of selected jurors who report}\cr
    \code{Year} \tab {\code{1998} or \code{2000}}\cr
    \code{I2000} \tab {Indicator for data from the year 2000}\cr
  }
}
\details{
Tom Shields, jury commissioner for the Franklin County Municipal Court in Columbus, Ohio, is
responsible for making sure that the judges have enough potential jurors to conduct jury trials.
Jury duty for this court is two weeks long, so Tom must
bring together a new group of potential jurors twenty-six times a year. Random sampling methods
are used to obtain a sample of registered voters in Franklin County every two weeks, and these
individuals are sent a summons to appear for jury duty. One of the most difficult aspects of Tom's
job is to get those registered voters who receive a summons to actually appear at the courthouse for
jury duty. This dataset contains the 1998 and 2000 data for the percentages of individuals who reported
for jury duty after receiving a summons. The reporting dates
vary slightly from year to year, so they are coded sequentially from 1, the first group to report in
January, to 26, the last group to report in December. A variety of methods were used after 1998
to try to increase participation rates.
}
\source{
Franklin County Municipal Court
}

\keyword{datasets}
