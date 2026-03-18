\name{MedDiagn}
\alias{MedDiagn}
\title{
Computes the posterior probability of having a certain disease from
 prevalence, sensitivity, and specificity data.
}
\description{
  If experimental data on the sensitivity and the specificity
  of a diagnostic test are available, and the prevalence of the the
  condition is known with its raw data, then this function estimates
  the posterior probability of having the condition, with its 95\%
  credible interval.
}
\usage{
MedDiagn(x0, n0, x1, n1, x2, n2, N = 10000,
  alpha = 0.05, pdf = FALSE)
}
\arguments{
  \item{x0}{
prevalence raw data: number of people with a certain condition
}
  \item{n0}{
number of people examined for that condition
}
  \item{x1}{
sensitivity data: number of people with the disease for whom this test was positive
}
  \item{n1}{
total number of people in the sensitivity sample
}
  \item{x2}{
specificity raw data: number of people who did not have the disease who
  tested negative
}
  \item{n2}{
total number of people in the specificity sample
}
  \item{N}{
number of cases to be simulated (best left at 10000 or greater
}
  \item{alpha}{
credibility required (default 95\%)
}
  \item{pdf}{
set this to TRUE only if you want to keep a pdf-file of the posterior
  probability plot
}
}
\value{
none returned: a plot and printed information are produced
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\examples{
MedDiagn(105, 35000, 72, 80, 640, 800)
}

\keyword{ models }

