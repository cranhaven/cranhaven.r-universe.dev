\name{isotonic.regression}
\alias{isotonic.regression}
\title{
  Isotonic Regression
}
\description{
  Merges two sorted vectors, then fits the decreasing function that minimises squared error.
}
\usage{
isotonic.regression(x1,y1,x2=NULL,y2=NULL)
}
\arguments{
  \item{x1}{
    A vector of predictor values. This vector is assumed sorted.
  }
  \item{y1}{
    A vector of the corresponding response values. Assumed to be the same
    length as x1.
  }
  \item{x2}{
    A second vector of predictor values. This vector is assumed sorted.
  }
  \item{y2}{
    A vector of the corresponding values. Assumed to be the same
    length as x2.
  }  
}
\details{
  This function is designed to be used internally by the function
  select.cutoff. It is provided as it may provide a useful efficient
  implementation of isotonic regression, but it is not designed for
  general use.

  The function merges the sorted vectors x1 and x2 into a single vector,
  and merges y1 and y2 in the same way. It then fits the decreasing
  vector z that minimises the total squared error sum((z-y)^2) where y
  is the merged vector of y1 and y2. By setting x2=y2=NULL, this
  function can perform isotonic regression on a single data set.

  The solution to isotonic regression can be shown to be given by
  blockwise averages of the original data, where the blocks correspond
  to the convex hull of the cumulative sums.
}
\value{
  An object of class "isotonic" with the following components

  \describe{
    \item{x}{The sorted x1 and x2 values with Inf appended.} 
    \item{y}{The fitted isotonic regression values with 0 appended.}
    \item{data}{The original y1 and y2 values, corresponding to
    increasing order of the combined x1 and x2 vectors.}
  }  
}
\author{
Toby Kenney
}
\examples{
set.seed(1)
x1<-seq_len(1000)
y1<-rnorm(1000)+3*exp(-x1/500)
x2<-seq_len(100)*2.1001
y2<-rnorm(100)+3*exp(-x2/500)

ir<-isotonic.regression(x1,y1,x2,y2)
graphics::plot(ir$x,ir$y)
}
