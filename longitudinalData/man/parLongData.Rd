\name{parLongData}
\alias{parLongData}
\alias{parTRAJ}
\alias{parMEAN}


\title{
 ~ Function: parLongData, parTraj and parMean~
}
\description{
\code{parLongData}, \code{parTraj} and \code{parMean} are constructors for the class
\code{\linkS4class{ParLongData}}.
}

\usage{
parLongData(type, col, pch, pchPeriod, cex, xlab, ylab)
parTRAJ(type = "l", col = "black", pch = "1",
   pchPeriod = 0, cex = 1, xlab = "Time", ylab = "")
parMEAN(type = "b", col = "clusters", pch = "letters",
   pchPeriod = 1, cex = 1.2, xlab = "Time", ylab = "")
}

\arguments{
  \item{type}{\code{[character]}: Set type of the plot should be
    drawn ('p' for point, 'l' for line, 'b' for both, 'c' line appart, 'o'
    for overplot, 'h' for histogram, 's' and 'S' for steps, 'n' for no
    ploting)}
  \item{col}{\code{[character]}: Set the plotting color. Vector of
    values are accepted. The special value 'clusters' can be use to color
    each trajectories according to its clusters (see details).}
  \item{pch}{\code{[numeric]} or \code{[character]}: Either an integer
    specifying a symbol or special values 'letters' or 'symbol' (see details).}
  \item{pchPeriod}{\code{[numeric]}: Fix the number of point that should be plot. Usefull to
    plot points on trajectories with a lot of mesurement (see examples
    in \code{\link{plotTrajMeans}} for \code{LongData} for details).}
  \item{cex}{\code{[numeric]}: Set the
    amount by which plotting text and symbols should be magnified relative
    to the default.}
  \item{xlab}{\code{[character]}: Title for the x axis.}
  \item{ylab}{\code{[character]}: Title for the y axis.}
}
\details{
\code{parLongData} is the basic constructor of the class
\code{\linkS4class{ParLongData}}.

\code{parTRAJ} create an object with default values for ploting
individual trajectories ;

\code{parMEAN} create an object with default values for ploting mean trajectories.

If col='clusters', pch='letters' or pch='symbol', the object can not be use directly, it
should first be prepared using the function \link{expandParLongData}.
}

\value{
  An object of class \code{\linkS4class{ParLongData}}
}


\section{Author(s)}{
  Christophe Genolini\cr
  PSIGIAM: Paris Sud Innovation Group in Adolescent Mental Health\cr
  INSERM U669 / Maison de Solenn / Paris\cr\cr

  Contact author : <genolini@u-paris10.fr>
}

\section{English translation}{
  Raphaël Ricaud\cr
  Laboratoire "Sport & Culture" / "Sports & Culture" Laboratory \cr
  University of Paris 10 / Nanterre
}

\examples{
##################
### Construction of LongData

time=c(1,2,3,4,8,12,16,20)
id2=1:120
f <- function(id,t)((id-1)\%\%3-1) * t
g <- function(id,t)(id\%\%2+1)*t
ld2 <- longData3d(
   array(cbind(outer(id2,time,f),outer(id2,time,g))+rnorm(120*8*2,0,3),
   dim=c(120,8,2)))

### Example with default value
plotTrajMeans3d(ld2)
plotTrajMeans3d(ld2,parTraj=parTRAJ())

### Example with default value except for the color
plotTrajMeans3d(ld2,parTraj=parTRAJ(col="blue"))
}
\keyword{methods}

