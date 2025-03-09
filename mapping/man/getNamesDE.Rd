\name{getNamesDE}
\alias{getNamesDE}

\title{Germany names}
\description{Retrieves Germany statistical unit names.}

\usage{getNamesDE(unit = c("state", "district", "municipal", "municipality"), 
       all_levels = TRUE)
}

\arguments{
  \item{unit}{the type of statistical units}
  \item{all_levels}{a logical value indicating if all levels are returned or only the unit names}

}

\value{A character vector or a data frame with unit names and corresponding associated levels}

\seealso{
\code{\link{getNamesIT}}, \code{\link{getNamesEU}}, \code{\link{getNamesWR}}, \code{\link{getNamesUK}}
}

\examples{

getNamesDE()

\donttest{
getNamesDE(unit = "district")
getNamesDE(unit = "district", all_levels = FALSE)

}

}