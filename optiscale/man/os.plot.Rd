\name{os.plot}
\alias{os.plot}

\title{
   Graph of optimal scaling transformation
}
\description{
   Line and point plot showing optimally-scaled values on the 
   vertical axis, original data values (assumed to be qualitative)
   on the horizontal axis.
}

\usage{
   os.plot(x.qual, os.data, 
      main.title = "Plot of optimal transformation")
}

\arguments{
  \item{x.qual}{Vector of data values, assumed to be qualitative.
   
}
  \item{os.data}{
   Vector of optimally-scaled data values.
}
  \item{main.title}{
   Main title for plot.
}
}

\value{
   Object of class \code{trellis}.
}

\seealso{
   \code{\link{plot.opscale}},
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
  ###   Plot of optimal scaling transformation
     os.plot(op.scaled$qual, op.scaled$os)         
}
