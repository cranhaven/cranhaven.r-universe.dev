\name{Methods}
\alias{Methods}
\alias{plot.opscale}
\alias{print.opscale}
\alias{print.summary.opscale}
\alias{summary.opscale}

\title{
   S3 methods for opscale
}
\description{
   Plot, print, shepard, stress, and summary methods for objects of class \code{opscale}
}

\usage{
\method{plot}{opscale}(x, ...)
\method{print}{opscale}(x, ...)
\method{summary}{opscale}(object, ...)
}

\arguments{
   \item{object}{Object of class \code{opscale}}
   \item{x}{Object of class \code{opscale}}
   \item{...}{Ignored}
}

\details{
 Method \code{print} returns a listing of the data. 
 \code{summary} describes the optimal scale transformation. 
 \code{plot} calls \code{os.plot} and returns an object of class \code{trellis} that 
    graphs optimally-scaled values against the original (qualitative)
    data values.
}

\seealso{
   \code{\link{os.plot}}
}

\examples{
  ###   x1 is vector of qualitative data
  ###   x2 is vector of quantitative values
            x1 <- c(1,1,1,1,2,2,2,3,3,3,3,3,3)     
            x2 <- c(3,2,2,2,1,2,3,4,5,2,6,6,4)     
  ###   Optimal scaling, specifying that x1
  ###   is ordinal-discrete
     op.scaled <- opscale(x.qual=x1, x.quant=x2,   
                  level=2, process=1)              
     print(op.scaled)
     summary(op.scaled)
     plot(op.scaled)
}
