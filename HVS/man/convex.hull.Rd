\name{convex.hull}
\alias{convex.hull}
\title{
  Convex hull of a sorted set of coordinates
}
\description{
  Finds the upper convex hull of a sorted set of 2-dimensional coordinates.
}
\usage{
convex.hull(x,y)
}
\arguments{
  \item{x}{
    A vector of x coordinates of points. This vector is assumed sorted.
  }
  \item{y}{
    A vector of the corresponding y coordinates. Assumed to be the same
    length as x.
  }
}
\details{
  Finds the convex hull of the points
  \{(x[1],-Inf),(x[1],y[1]),(x[2],y[2]),...,(x[n],y[n]),(x[n],-Inf)\}
  where n is the length of the vector x. This function is designed to be
  used internally for isotonic regression, which is in turn used
  internally for the select.cutoff function. It is designed to be very
  efficient for this case, but not for general use - it is not flexible
  and has no error checking.

  The method works by a divide-and-conquer approach. It divides the data
  in half, finds the convex hulls for both halves, then removes any
  middle points that are not in the combined convex hull. This results
  in a linear-time algorithm.   
}
\value{
  A vector of indices, indicating which points are in the convex hull.
}
\author{
Toby Kenney
}
\examples{
set.seed(1)
x<-seq_len(1000)
y<-runif(1000)*x*(1001-x)

hull<-convex.hull(x,y)
graphics::plot(x,y)
graphics::points(x[hull],y[hull],col="red",type='b')
}
