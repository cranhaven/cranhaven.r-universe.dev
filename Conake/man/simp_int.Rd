\encoding{UTF-8}
\name{simp_int}
\alias{simp_int}

\title{
The Simpson method to compute integral
}
\description{
This function computes the Simpson method to calculate an integral.
}
\usage{
simp_int(x, fx, n.pts = 256, ret = FALSE)
}

\arguments{
  \item{x}{
The vector where the integral is computed
}
  \item{fx}{
The function to integrate
}
  \item{n.pts}{
The number of points used to compute the integral through the Simpson technique.
}
  \item{ret}{
A  boolean control parameter. Default value is FALSE.
}
}

\value{
Returns the value of the integral.
}

\author{
W. E. Wansouwé, F.G. Libengué and  C. C. Kokonendji 
}

\examples{
Vec=rgamma(100,1.5,2.6)
x=seq(min(Vec),max(Vec),length.out=100)
simp_int(x,dgamma(x,1.5,2.6))

}


