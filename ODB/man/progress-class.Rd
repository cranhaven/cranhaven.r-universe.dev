\name{progress-class}
\Rdversion{1.1}
\docType{class}

\alias{progress}
\alias{progress-class}

\alias{progress.console-class}
\alias{initialize,progress.console-method}
\alias{set,progress.console-method}

\alias{progress.file-class}
\alias{initialize,progress.file-method}
\alias{set,progress.file-method}

\alias{set}

\title{Classes "progress", "progress.file" and "progress.console"}
\description{
  The two last classes inherit from the first one, and describe textual progression outputs.
}

\section{Objects from the Class}{
  \code{progress} is an abstract class that should not be instantiated.
  
  \code{progress.file} and \code{progress.console} objects are to be created by the \code{\link{new}} function, as \code{initialize} methods are implemented for each of them.
  
  See the Examples section.
}

\section{progress.file constructor}{
  Objects can be created by \code{\link{new}} with the following arguments :
  \describe{
    \item{\code{main}:}{Directly transfered in the appropriate slot}
    \item{\code{iMax}:}{Directly transfered in the appropriate slot}
    \item{\code{iCurrent}:}{Directly transfered in the appropriate slot}
	\item{\code{nSteps}:}{Approximative amount of steps, to pass to \code{\link{pretty}}}
  }
}

\section{progress.console constructor}{
  Objects can be created by \code{\link{new}} with the following arguments :
  \describe{
    \item{\code{main}:}{Directly transfered in the appropriate slot}
    \item{\code{iMax}:}{Directly transfered in the appropriate slot}
    \item{\code{iCurrent}:}{Directly transfered in the appropriate slot}
  }
}

\section{Common slots}{
  \describe{
    \item{\code{main}:}{Single character value, the title to print at beginning}
    \item{\code{iMax}:}{Single integer value, the maximum value for the iteration index}
    \item{\code{iCurrent}:}{Single integer value, the current value of the iteration index}
  }
}

\section{progress.console slots}{
  \describe{
    \item{\code{pTimes}:}{Float vector, \code{\link{proc.time}} returns used to compute the ETA}
    \item{\code{eraseLength}:}{Single integer value, \code{\link{nchar}} in the previous output}
  }
}

\section{progress.file slots}{
  \describe{
    \item{\code{steps}:}{Integer vector, iteration indexes for which print an output}
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{Constructors of the classes, see previous sections.}
    \item{set}{Updates a progression objects. Takes two arguments : \code{progress} (the object to update) and \code{iCurrent}, the new value for the iteration index. On each update, a \code{\link{message}} will be printed according to the class of the updated object.}
  }
}

\author{
  Sylvain Mareschal
}

\examples{
  # File oriented progression
  testFun = function() {
    obj <- new("progress.file", main="Iterating", iMax=20)
    for(i in 1:20) {
      obj = set(obj, i)
      Sys.sleep(0.1)
    }
  }
  testFun()
 
  # Console oriented progression
  testFun = function() {
    obj <- new("progress.console", main="Iterating", iMax=20)
    for(i in 1:20) {
      obj = set(obj, i)
      Sys.sleep(0.1)
    }
  }
  testFun()
}
