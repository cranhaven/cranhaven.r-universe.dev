\name{checkNamesIT}
\alias{checkNamesIT}

\title{Check Italian names}

\description{
Check the differences between the names (or codes) given in input and the corresponding names (or codes), as provided by ISTAT, of the selected Italian statistical unit.
}

\usage{
checkNamesIT(id,
             unit = c("ripartizione", "regione", "provincia","comune"),
             year = c("2021","2020", "2019", "2018", "2017"),
             matchWith = c("name", "code", "number"),
             return_logical = FALSE, print = TRUE, use_internet = TRUE)
}

\arguments{
  \item{id}{character vector with names or codes}
  \item{unit}{the type of Italian statistical unit to check}
  \item{year}{year of the analysis}
    \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"name"} \tab if unit names \cr
\code{"code"} \tab if unit code \cr
\code{"number"} \tab if unit number code}
}
  \item{return_logical}{a logical value indicating whether nomatched id are returned}
  \item{print}{a logical value indicating whether print the nomatched names}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.}
}
\details{
The function provides a check between id name or code in the dataset and the corresponding selected Italian statistical unit. \code{unit} starts from the largest aggregate, "ripartizione", to the smallest, "comune". Since unit can change over the years, the year of the data has to be provided.
}
\value{
Returns a string vector with nomatched names or a boolean vector indicating whether or not the id matched.
}

\author{
Alessio Serafini
}


\seealso{
\code{\link{checkNamesEU}}, \code{\link{checkNamesUS}}, \code{\link{checkNamesWR}}
}
\examples{
data("popIT")
ck <- checkNamesIT(popIT$ID, unit = "provincia")
str(ck)

\donttest{
ck <- checkNamesIT(popIT$ID, unit = "provincia", return_logical = TRUE)
str(ck)
}
}
