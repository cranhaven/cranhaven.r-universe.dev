\name{getNamesWR}
\alias{getNamesWR}

\title{World countries names}

\description{Retrieves world country names, ids and iso.}

\usage{
getNamesWR(unit = c("all","country", "name_formal",
                    "name_wb", "iso2", "iso3",
                    "iso3_eh", "iso3_numeric", "iso3_un",
                    "iso2_wb", "iso3_wb"))
}

\arguments{
\item{unit}{the type of names}
}

\value{A character vector or a data frame with unit names and corresponding associated levels.}

\seealso{
\code{\link{getNamesIT}}, \code{\link{getNamesUS}}, \code{\link{getNamesEU}}, \code{\link{getNamesUK}}, \code{\link{getNamesDE}}
}

\examples{
getNamesWR()
getNamesWR("iso3")
}
