\name{checkNamesEU}
\alias{checkNamesEU}
\title{Check European names}

\description{
Check the differences between the names (or codes) given in input and the names (or codes), as provided by Eurostat, of the corresponding selected European statistical unit.
}
\usage{
checkNamesEU(id,
             unit = c("nuts0", "nuts1", "nuts2", "nuts3", "urau"),
             year = c("2021","2016", "2013", "2010", "2006", "2003"),
             matchWith = c("nuts", "id", "iso2", "iso3", "country_code"),
             scale = c("20", "60"), return_logical = FALSE,
             print = TRUE, use_internet = TRUE)
}

\arguments{
  \item{id}{character vector with names or codes}
  \item{unit}{the type of European statistical unit to check}
  \item{year}{year of the analysis}
  \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"nuts"} \tab if nuts names \cr
\code{"id"} \tab if nuts id \cr
\code{"iso2"} \tab if iso2 code \cr
\code{"iso3"} \tab if iso3 code \cr
\code{"country_code"} \tab if Eurostat code }
}
  \item{scale}{the scale of the map.}
  \item{return_logical}{a logical value indicating whether nomatched id are returned.}
  \item{print}{a logical value indicating whether print the nomatched names}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.}
}
\details{
The function provides a check between id name in the dataset and the European statistical unit. \code{unit} starts from the largest aggregate, "nuts0" (European country), to the smallest, "nuts3". Since unit can change over the years, the year of the data has to be provided.

The single unit can be coded in different ways, with names, id or iso standard.
}
\value{Returns a string vector with nomatched names or a boolean vector indicating whether or not the id matched.}

\author{
Alessio Serafini
}


\seealso{
\code{\link{checkNamesIT}}, \code{\link{checkNamesUS}}, \code{\link{checkNamesWR}}
}
\examples{
data("popEU")
data("popEUnuts2")

# Chech only the country
ck <- checkNamesEU(id = popEU$GEO,
                   unit = "nuts0", matchWith = "id")
ck1 <- checkNamesEU(id = popEU$GEO, unit = "nuts0",
                    matchWith = "id", return_logical = TRUE,
                    print = FALSE)

popEU[ck1,]

ck2 <- checkNamesEU(id = popEUnuts2$GEO,
                    unit = "nuts2",
                    matchWith = "id")
}

