\name{ParWindows-class}
\docType{class}
\alias{ParWindows}
\alias{ParWindows-class}
\alias{[,ParWindows-method}
\alias{[<-,ParWindows-method}

\title{~ Class: ParWindows ~}
\description{\code{ParWindows} is an objet containing graphical
  parameter used to set the screen display.
}

\section{Slots}{
  \describe{
    \item{\code{nbCol}}{\code{[numeric]}: Number of column of the screen
      matrix.}
    \item{\code{nbRow}}{\code{[numeric]}: Number of row of the screen
      matrix.}
    \item{\code{addLegend}}{\code{[logical]}: Shall a legend be added on
      the graph?}
    \item{\code{closeScreen}}{\code{[logical]}: On exit, high level plot
      function can either close the screen that they open and return
      nothing ; or not close it and return the list of the screen number.}
    \item{\code{screenMatrix}}{\code{[matrix(numeric)]}: Matrix with 4
      column defining the screen region, like the \code{figs} argument
      of the function \link[graphics]{screen}. The \code{screenMatrix} can be specified
      by the user (bad idea) or can be compute automaticaly according to
      \code{nbCol}, \code{nbRow} and \code{addLegend}. For that, use
      \link{windowsCut}.}
  }
}

\section{Construction}{
  Object \code{ParWindows} can be created by the constructor
  \code{\link{parWindows}} or by the function \code{\link{windowsCut}}.
}

\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field \code{xxx}.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to value.}
  }
}


\section{Author}{
Christophe Genolini\cr
1. UMR U1027, INSERM, Université Paul Sabatier / Toulouse III / France\cr
2. CeRSME, EA 2931, UFR STAPS, Université de Paris Ouest-Nanterre-La Défense / Nanterre / France
}

\references{
  [1] C. Genolini and B. Falissard\cr
  "KmL: k-means for longitudinal data"\cr
  Computational Statistics, vol 25(2), pp 317-328, 2010\cr

  [2] C. Genolini and B. Falissard\cr
  "KmL: A package to cluster longitudinal data"\cr
  Computer Methods and Programs in Biomedicine, 104, pp e112-121, 2011\cr
}


\examples{
### Building ParWindows
(paramWin <- parWindows(3,2,FALSE,TRUE))

### Get
figsScreen <- paramWin['screenMatrix']

### Usage
listScreen <- split.screen(figsScreen)
screen(listScreen[1])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[3])
plot(-5:5/10,2.5-(-5:5)^2/20,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20,ylim=c(0,6),type="l",lwd=3)

screen(listScreen[5])
plot(-5:5/10,(-5:5)^2/10,ylim=c(0,6),axes=FALSE,xlab="",ylab="",type="l",lwd=3)
lines(-5:5/10,(-5:5)^2/20+1.25,ylim=c(0,6),type="l",lwd=3)
close.screen(all.screens=TRUE)

### Sorry for that...
}
\keyword{classes}
\keyword{classif}  % Classification	['class' package]
\keyword{cluster}  % Clustering
\keyword{ts}       % Time Series
