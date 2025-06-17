\name{Wulffnet}
\alias{Wulffnet}
\title{Function Wulffnet
}
\usage{
Wulffnet(add = FALSE, col = gray(0.7), border = "black", lwd = 1)
}
\description{
Function Wulffnet
Plot a Wulffnet
modified from RFOC package; Wulff net rotated
}
\arguments{
  \item{add}{Logical, TRUE=add to existing plot}
  \item{col}{color}
  \item{border}{border color}
  \item{lwd}{line width }
}
\details{
 Plots equal-angle stereonet as opposed to equal-area.
 In comparison to the original Wnet function from RFOC package, Wulff net is rotated by 90 degrees so as to conform with custom in mineralogy.
}
\value{
  graphical side effects
}

\author{Jonathan M. Lees <jonathan.lees@unc.edu>, Florian Dufey <GrimR@gmx.de>}

\examples{
Wulffnet(add = FALSE, col = gray(0.7), border = "black", lwd = 1)
}

