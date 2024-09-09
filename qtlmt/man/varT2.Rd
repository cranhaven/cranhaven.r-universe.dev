\name{varT2}
\alias{varT2}
\title{
Hotelling's T^2 test statistic
}
\description{
Calculate the Hotelling's T^2 test statistic.
}
\usage{
varT2(x, group, equalVar=T)
}
\arguments{
  \item{x}{
a data matrix/frame. Columns are variables to select from.
}
  \item{group}{
a grouping indicator of observations.
}
  \item{equalVar}{
whether assume the same variance-covariance in two groups.
}
}

\value{
Hotelling's T^2 test statistic.
}

\examples{
data(etrait)
varT2(traits, mdat[,42])
}

