\name{getNamesUS}
\alias{getNamesUS}

\title{USA names}
\description{Retrieves USA statistical unit names.}

\usage{
getNamesUS(year = "2018",
           unit = c("region", "division","state", "county",
                    "district", "district_county", "urban_area"),
           id = FALSE, all_levels = TRUE)
}

\arguments{
\item{year}{year of the analysis}
\item{unit}{the type of statistical units}
  \item{id}{boolean value indicating whether the ids are returned instead of names}
\item{all_levels}{a logical value indicating if all levels are returned or only the unit names}
}


\value{A character vector or a data frame with unit names and corresponding associated levels}

\seealso{
\code{\link{getNamesIT}}, \code{\link{getNamesEU}}, \code{\link{getNamesWR}}, \code{\link{getNamesUK}}, \code{\link{getNamesDE}}}


\examples{

getNamesUS()

\donttest{
getNamesUS(unit = "state")
getNamesUS(unit = "state", all_levels = FALSE)

getNamesUS(unit = "county")
getNamesUS(unit = "county", all_levels = FALSE)
}

}
