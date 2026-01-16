\name{parWindows}
\alias{parWindows}

\title{
  ~ Function: parWindows ~
}
\description{
  \code{parWindows} is the constructor of object \code{\linkS4class{ParWindows}}.
}
\usage{
parWindows(nbRow, nbCol, addLegend,closeScreen)
}
\arguments{
  \item{nbRow}{\code{[numeric]}: Number of row of the screen matrix.}
  \item{nbCol}{\code{[numeric]}: Number of column of the screen matrix.}
  \item{addLegend}{\code{[logical]}: Shall a legend be added on the graph?}
  \item{closeScreen}{\code{[logical]}: Some function need to add details on a graph. This option let
    them call a plot function that will not call a \code{close.screen}
    on exit, so the graph will be modifiable.}
}

\details{
  \code{parWindows} is the constructor of object \code{\linkS4class{ParWindows}}.
   Given a number of rows and colonnes, it computes the \code{screenMatrix} that
   is use by \link[graphics]{split.screen} for plot object
  \code{\linkS4class{LongData}}. If \code{addLegend} is true, an
  extra space is added on the top of the graphes to print the legend.
}
\value{
An object of class \code{\linkS4class{ParWindows}}.
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

### :-)
}

