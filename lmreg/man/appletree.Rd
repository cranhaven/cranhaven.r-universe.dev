\name{appletree}
\alias{appletree}
\docType{data}
\title{
Apple yield with cropping under tree
}
\description{
Apple crop volume under various ground covers underneath tree (Pearce, 1983)
}
\usage{data(appletree)}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{Weight}}{Total weight (in pounds) of apple produced in a plot in four years, post-treatment}
    \item{\code{Treatment}}{Five types of permanent cropping under the apple tree
(coded as 1 to 5), or no cropping at all (0)}
    \item{\code{Block}}{Blocks coded as 1 to 4}
    \item{\code{Volume}}{Total crop volume (in bushels) in four years, pre-treatment}
  }
}
\source{
Pearce, S.C. (1983) The Agricultural Field Experiment, Wiley, Chechester, p.284.
}

\examples{
data(appletree)
head(appletree)
}
\keyword{datasets}
