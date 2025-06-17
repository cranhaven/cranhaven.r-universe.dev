\name{jPrice}
\alias{jPrice}
\title{
Historical Price Function
}
\description{
Take out a price series from database.

See the report: Value at Risk.<researchgate.net>
}
\usage{
jPrice(name)
}
\arguments{
  \item{name}{
Name of a stock. See list of stocks using jStockList()
}
}
\value{
A price series.
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
jStockList()
jPrice('AAA')
}
\keyword{ jVaR }
\keyword{ jReturn }
\keyword{ jStockList }