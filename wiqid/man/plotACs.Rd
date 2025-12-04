\name{plotACs}
\alias{plotACs}


\title{
Plot Activity Centres from \code{Bsecr0} output
}
\description{
Plot posterior distributions of Activity Centre (AC) locations using the output from \code{\link{Bsecr0}}.
}
\usage{
plotACs(object, which=NA, howMany=3000, showLabels=TRUE)

}
\arguments{
  \item{object}{
a \code{Bwiqid} object with ACs attribute}
  \item{which}{
a numeric vector indication which ACs to plot, default is to plot all.
}
  \item{howMany}{
the number of points to plot for each AC}
  \item{showLabels}{
if TRUE, point clusters for animals detected will be labelled with the animal ID.
}
}
\value{
Returns nothing, used for its plotting side effect.
}

\author{
Mike Meredith
}
