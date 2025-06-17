\name{jReturn}
\alias{jReturn}
\title{
Return Function
}
\description{
Compute returns from a price series of an asset.

Return is gain (or loss) rate from an investment to the asset in a time interval.

See the report: Value at Risk.<researchgate.net>
}
\usage{
jReturn(s)
}
\arguments{
  \item{s}{
A price series.
}
}
\value{
A return series.
}
\author{
Hung Vu
}
\note{
viet-hung.vu@jvn.edu.vn
}

\seealso{
https://www.researchgate.net/profile/Vu_Hung4
}
\examples{
y <- c(11, 12, 10, 13, 12, 14, 13, 15, 13, 14, 12)
s <- jReturn(y)
s
}
\keyword{ jVaR }
\keyword{ jPrice }