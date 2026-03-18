\name{sumchart}
\alias{sumchart}
\title{Plotting routine for dataframes of looic values.}
\description{Produces a dotchart with error bars as summary of a
  dataframe with model names (`modnames'), LOOIC-values (`looic'),
  standard errors (`se'), lower values (`lwr'), and upper values (`upr') .}
\usage{
sumchart(df, rownames, groups, perc)
}
\arguments{
  \item{df}{
    data.frame name}
  \item{rownames}{
    model names}
  \item{groups}{
    row names}
  \item{perc}{
    the percentage of credibility desired}
}
\value{
  A plot is produced. }
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{dotchart}}
}



