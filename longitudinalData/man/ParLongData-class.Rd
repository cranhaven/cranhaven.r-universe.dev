\name{ParLongData-class}
\docType{class}
\alias{ParLongData}
\alias{ParLongData-class}
\alias{[,ParLongData-method}
\alias{[<-,ParLongData-method}

\title{~ Class: ParLongData ~}
\description{\code{ParLongData} is an objet containing some graphical
  parameter used to plot \code{\linkS4class{LongData}} object and / or mean trajectories. They work as
  define in \code{par}.
}

\section{Slots}{
  \describe{
    \item{\code{type}}{\code{[character]}: Type of the plot that should be
      drawn ('p' for point, 'l' for line, 'b' for both, 'c' line appart, 'o'
      for overplot, 'h' for histogram, 's' and 'S' for steps, 'n' for no
      ploting)}
    \item{\code{col}}{\code{[character]}: A specification for the
      default plotting color. Can be either a single value or a vector.}
    \item{\code{pch}}{\code{[numeric]} or \code{[character]}: Either an integer
          specifying a symbol or a single character to be used as the default in plotting
          points.  See example in \link{points} for possible values and their
	  interpretation.}
    \item{\code{pchPeriod}}{\code{[numeric]}: Fix the number of point that should be plot. Usefull to
      plot points on trajectories with a lot of mesurement (see examples
      in \code{\link{plotTrajMeans}} for \code{LongData} for details).}
    \item{\code{cex}}{\code{[numeric]}: A numerical value giving the
      amount by which plotting text and symbols should be magnified relative
      to the default.}
    \item{\code{xlab}}{\code{[character]}: A title for the x axis.}
    \item{\code{ylab}}{\code{[character]}: A title for the y axis.}
  }
}

\section{Construction}{
  Object \code{ParLongData} can be created by three functions:
  \enumerate{
    \item \code{parLongData} create an object from scratch ;
    \item \code{parTraj} create an object containing default value to plot
    individutal trajectories;
    \item \code{parMean} create an object containing default value to
    plot mean trajectories.
  }
}

\section{Methods}{
  \describe{
    \item{\code{object['xxx']}}{Get the value of the field \code{xxx}.}
    \item{\code{object['xxx']<-value}}{Set the field \code{xxx} to \code{value}.}
  }
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
   ### Building ParLongData
   parMyData <- parLongData(type="n",col=3,pch="1",pchPeriod=20,cex=1,xlab="Time",ylab="Size")

   ### Get
   parMyData['col']

   ### Set
   parMyData['cex'] <- 3
   (parMyData)
}
\keyword{classes}
\keyword{cluster}  % Clustering
