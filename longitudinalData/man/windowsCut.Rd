\name{windowsCut}
\alias{windowsCut}

\title{
  ~ Function: windowsCut ~
}
\description{
\code{windowsCut} prepare an object \code{\linkS4class{ParWindows}}
  according to its arguments.
}
\usage{
windowsCut(x, addLegend = TRUE,closeScreen=TRUE)
}
\arguments{
  \item{x}{\code{[numeric]} or \code{[couple(numeric)]}: \code{x} is
    used to calculate the fields \code{nbCol} and \code{nbRow} of the
    object \code{ParWindows}. If \code{x} is a couple, then \code{x[1]} is \code{nbRow}
    and \code{x[2]} is \code{nbCol}. If \code{x} is a single number
    (the number of plot that should be display), \code{nbCol} and \code{nbRow} parameters are calculate
    consequently (see detail).
  }
  \item{addLegend}{\code{[logical]}: If \code{addLegend}is true, an
    extra space is reserved on the top of the screen to print the
    legend.
  }
  \item{closeScreen}{\code{[logical]}: Some function
    need to add details on a graph. This option let
    them call a plot function that will not call a \code{close.screen}
    on exit, so the graph will be modifiable.
  }
}

\details{
  If \code{x} is a number of variable, the column and row number are
  estimate according to the formula
  \code{nbCol <- ceiling(sqrt(x))} and \code{nbRow <- ceiling(x/nbCol)}.
}

\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSM, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}

\value{
  An object of class \code{\linkS4class{ParWindows}}.
}

\examples{
  ### Simple cut with no space for legent
  windowsCut(3,FALSE)
  windowsCut(4,FALSE)
  windowsCut(5,FALSE)

  ### Simple cut with legend
  windowsCut(5)
}

