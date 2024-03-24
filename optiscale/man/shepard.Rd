\name{shepard}
\alias{shepard}
\alias{shep.plot}

\title{
   Shepard diagram for opscale
}
\description{
   Graph showing data (assumed quantitative) on vertical axis, optimally-scaled
   data on horizontal axis.
}
\usage{
shepard(x, \dots)

shep.plot(x.quant, os.data, main.title = "Shepard Diagram")
}

\arguments{
  \item{x}{
   An object of class \code{opscale}
}
  \item{x.quant}{
    Data vector, assumed to be quantitative.
}
  \item{os.data}{
   Optimally-scaled data.
}
  \item{main.title}{
   Main title for graph.
}
  \item{\dots}{
   Ignored
}
}

\value{
   \code{shepard()} and \code{shep.plot()} both
   produce an object of class \code{trellis}
}

\section{Warning }{If using \code{shep.plot()},
  the Shepard diagram should be created using "raw" optimally scaled values.
  That is, the OS values should NOT be rescaled to the mean and standard
  deviation of the original qualitative data.} 


\examples{
  ###   x1 is vector of qualitative data
  ###   x2 is vector of quantitative values
            x1 <- c(1,1,1,1,2,2,2,3,3,3,3,3,3)     
            x2 <- c(3,2,2,2,1,2,3,4,5,2,6,6,4)     
  ###   Optimal scaling, specifying that x1
  ###   is ordinal-discrete, optimally scaled
  ###   values are not rescaled
     op.scaled <- opscale(x.qual=x1, x.quant=x2,   
                  level=2, process=1,
                  rescale=FALSE)              
  ###   Create Shepard diagram
     shepard(op.scaled)
  ###   Same results are produced by:
     shep.plot(op.scaled$quant, op.scaled$os)       
}
