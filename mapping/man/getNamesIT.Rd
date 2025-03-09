\name{getNamesIT}
\alias{getNamesIT}

\title{Italian names}

\description{
Retrieves Italian statistical unit names.
}
\usage{
getNamesIT(year = c("2021","2020", "2019", "2018", "2017"),
           unit = c("ripartizione", "regione", "provincia", "comune"), all_levels = TRUE)
}

\arguments{
\item{year}{year of the analysis}
\item{unit}{the type of Italian statistical unit}
\item{all_levels}{a logical value indicating if all levels are returned or only the unit names}
}

\value{A character vector or a data frame with unit names and corresponding associated levels.}


\seealso{
\code{\link{getNamesEU}}, \code{\link{getNamesUS}}, \code{\link{getNamesWR}}, \code{\link{getNamesUK}}, \code{\link{getNamesDE}}
}

\examples{
getNamesIT()
getNamesIT(unit = "provincia")
getNamesIT(unit = "provincia", all_levels = FALSE)
}

