\name{\%over\%}
\alias{\%over\%}
\title{
  Division operator with divide by zero clobbering
}
\description{
  x \%over\% y = x/y when y!=0, equals 0 when y==0.
}
\usage{
  x \%over\% y
}
\arguments{
  \item{x,y}{
    Numeric or complex vectors or objects that can be coerced to such.
  }
}
\value{
  x/y when y!=0, otherwise 0.
}
\author{
  Grant Izmirlian izmirlig at mail dot nih dot gov
}
