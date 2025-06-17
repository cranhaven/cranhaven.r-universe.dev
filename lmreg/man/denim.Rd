\name{denim}
\alias{denim}
\docType{data}
\title{
Abrasion of denim jeans
}
\description{
Effects of Laundering Cycles and denim treatment on edge abrasion of denim jeans (Card et al., 2006). Data simulated to match means/SDs.
}
\usage{data(denim)}
\format{
  A data frame with 90 observations on the following 3 variables.
  \describe{
    \item{\code{Laundry}}{Three levels of laundry cycles (1 = 0 cycle, 2 = 5 cycles, 3 = 25 cycles)}
    \item{\code{Denim}}{Three types
of denim treatments (1 = pre-washed, 2 = stone-washed, 3 = enzyme washed)}
    \item{\code{Abrasion}}{abrasion score (lower score means higher damage)}
  }
}

\source{
Card, A., Moore, M.A. and Ankeny, M. (2006) Garment washed jeans: Impact of launderings on physical properties. Int. J. Clothing Sc.
Tech., 18, pp.43-52.
}

\examples{
data(denim)
head(denim)
}
\keyword{datasets}
