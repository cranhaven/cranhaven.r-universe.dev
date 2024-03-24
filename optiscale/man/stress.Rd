\name{stress}
\alias{stress}
\alias{calc.stress}

\title{
   Stress coefficients for \code{opscale}
}
\description{
   Calculates stress coefficients summarizing lack of fit between two vectors.
}
\usage{
stress(x, \dots)

calc.stress(quant, os, rescale = FALSE, 
   os.raw.mean = mean(os, na.rm = TRUE), 
   os.raw.sd = sd(os, na.rm = TRUE))
}

\arguments{
  \item{x}{
   Object of class \code{opscale}
}
  \item{quant}{
   Data vector.
}
  \item{os}{
   Vector of optimally-scaled data
}
  \item{rescale}{
   If TRUE, the optimally-scaled data have been rescaled to the mean and standard deviation of
   the original qualitative data vector that was used in the optimal scaling transformation.
}
  \item{os.raw.mean}{
   User-specified mean for optimally-scaled data, defaults to mean of \code{os}. 
      Only needed if \code{rescale = TRUE}.
}
  \item{os.raw.sd}{
   User-specified standard deviation for optimally-scaled data, defaults to
      standard deviation of \code{os}. 
      Only needed if \code{rescale = TRUE}.
}
  \item{\dots}{
   Ignored
}

}

\value{
   \code{stress()} and \code{calc.stress()} both
      produce a vector with three elements:
      \item{stress1 }{Kruskals Stress 1 coefficient}
      \item{stress2 }{Kruskals Stress 2 coefficient}
      \item{raw.stress}{Sum of squared residuals between \code{quant} and \code{os}}
}

\section{Warning }{
  If using \code{calc.stress()}, the
  stress coefficients must be created using "raw" optimally scaled values.
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
  ###  Calculate stress coefficients
    stress(op.scaled)
  ###   Same results can be obtained with:
    calc.stress(op.scaled$quant, op.scaled$os)                               
}
