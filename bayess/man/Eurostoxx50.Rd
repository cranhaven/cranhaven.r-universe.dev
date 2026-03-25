\name{Eurostoxx50}
\alias{Eurostoxx50}
\docType{data}
\title{
Eurostoxx50 exerpt dataset%%   ~~ data name/kind ... ~~
}
\description{
This dataset is a collection of four time series connected with the stock market.
Those are the stock values of the four companies ABN Amro, Aegon,
Ahold Kon., and Air Liquide, observed from January 1, 1998, to November 9, 2003.
}
\usage{data(Eurostoxx50)}
\format{
  A data frame with 1486 observations on the following 5 variables.
  \describe{
    \item{\code{date}}{six-digit date}
    \item{\code{Abn}}{value of the ABN Amro stock}
    \item{\code{Aeg}}{value of the Aegon stock}
    \item{\code{Aho}}{value of the Ahold Kon. stock} 
    \item{\code{AL}}{value of the Air Liquide stock}
  }
}
\details{
Those four companies are the first stocks (in alphabetical order) to appear in the financial index
Eurostoxx50.%%  ~~ If necessary, more details than the __description__ above ~~
}
\examples{
data(Eurostoxx50)
summary(Eurostoxx50)
}
\keyword{datasets}
\keyword{Eurostoxx50}
\keyword{ABN Amro}
\keyword{Aegon}
\keyword{Air Liquide}
\keyword{Ahold Kon}
