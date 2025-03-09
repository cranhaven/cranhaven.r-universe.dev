\name{getNamesUK}
\alias{getNamesUK}

\title{United Kingdom names}
\description{Retrieves United Kingdom statistical unit names.}

\usage{
getNamesUK(year = c("2020", "2019"), 
           unit = c("country", "county"), 
           all_levels = TRUE)
}

\arguments{
\item{year}{year of the analysis}
\item{unit}{the type of statistical units}
\item{all_levels}{a logical value indicating if all levels are returned or only the unit names}
}


\value{A character vector or a data frame with unit names and corresponding associated levels}

\seealso{
\code{\link{getNamesIT}}, \code{\link{getNamesEU}}, \code{\link{getNamesWR}}, \code{\link{getNamesDE}}
}

\examples{

getNamesUK()

\donttest{
getNamesUS(unit = "county")
getNamesUK(unit = "county", all_levels = FALSE)

}

}
