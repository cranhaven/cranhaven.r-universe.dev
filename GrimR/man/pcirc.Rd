\name{pcirc}
\alias{pcirc}

\title{Circle Plot}
\description{
  Add a circle to a plot, with cross-hairs
}
\usage{
pcirc(gcol = "black", border = "black", ndiv = 36)
}

\arguments{
  \item{gcol}{color of crosshairs}
  \item{border}{ border color }
  \item{ndiv}{ number of divisions for the circle }
}

\value{
no return values, used for side effects
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>}

\examples{
plot(c(-1,1),c(-1,1))
pcirc(gcol = "black", border = "black", ndiv = 36)
}

