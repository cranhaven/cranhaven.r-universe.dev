\name{getNamesFR}
\alias{getNamesFR}

\title{France names}
\description{Retrieves France statistical unit names.}

\usage{
getNamesFR(year = c("2021", "2020", "2019"), 
           unit = c("region"), all_levels = TRUE)
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

getNamesFR()

\donttest{
getNamesFR(all_levels = FALSE)

}

}