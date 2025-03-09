\name{getNamesEU}
\alias{getNamesEU}

\title{European names}

\description{
Retrieves European statistical unit names.
}

\usage{
getNamesEU(year = c("2021","2016", "2013", "2010", "2006", "2003"),
           unit = c("nuts0", "nuts1", "nuts2", "nuts3"), id = FALSE, all_levels = TRUE)
}

\arguments{
\item{year}{year of the analysis}
\item{unit}{the type of statistical unit}
  \item{id}{boolean value indicating whether the ids are returned instead of names}
\item{all_levels}{a logical value indicating if all levels are returned or only the unit names}
}

\value{A character vector or a data frame with unit names and corresponding associated levels.}


\seealso{
\code{\link{getNamesIT}}, \code{\link{getNamesUS}}, \code{\link{getNamesWR}}, \code{\link{getNamesUK}}, \code{\link{getNamesDE}}
}

\examples{

getNamesEU()

getNamesEU(unit = "nuts1")
getNamesEU(unit = "nuts1", all_levels = FALSE, id = FALSE)
getNamesEU(unit = "nuts1", all_levels = FALSE, id = TRUE)
}

