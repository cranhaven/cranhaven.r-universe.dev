\name{checkNamesWR}
\alias{checkNamesWR}

\title{Check World country names}

\description{Check the differences between the names (or codes) given in input and the names (or codes) of the worldwide countries.}

\usage{
checkNamesWR(id,
             unit = c("country", "nato", "ocde",
                      "continent", "region", "subregion",
                      "region_wb", "type_income", "type_economy"),
             matchWith = c("country", "iso2", "iso3", "iso3_eh",
                           "iso3_numeric", "iso3_un", "iso2_wb",
                           "iso3_wb", "name_formal", "name_wb"),
             res = c("low", "hi"), return_logical = FALSE,
             print = TRUE, use_internet = TRUE)
}

\arguments{
  \item{id}{character vector with names or codes}
    \item{unit}{the type of world statistical unit}
    \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"country"} \tab if country names \cr
\code{"iso2"} \tab if iso2 code \cr
\code{"iso3"} \tab if iso3 code. \cr
\code{"iso3_eh"} \tab if iso3_eh code \cr
\code{"iso3_numeric"} \tab if iso3 numeric code \cr
\code{"iso3_un"} \tab if iso3 United Nations \cr
\code{"iso2_wb"} \tab if iso2 World Bank \cr
\code{"iso3_wb"} \tab if iso3 World Bank \cr
\code{"name_formal"} \tab if formal names \cr
\code{"name_wb"} \tab if World Bank names
}}
  \item{res}{map resolution}
  \item{return_logical}{a logical value indicating whether nomatched id are returned}
  \item{print}{a logical value indicating whether print the nomatched names}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.}
}
\details{
The function provides a check between id name in the dataset and the worldwide country names.
The single unit can be coded in different ways, with names, id or iso standards.}

\value{
Returns a string vector with no matched names or a boolean vector indicating whether or not the id matched.
}

\author{
Alessio Serafini
}

\seealso{
\code{\link{checkNamesIT}}, \code{\link{checkNamesEU}}, \code{\link{checkNamesUS}}
}
\examples{
data("popWR")

ck <- checkNamesWR(id = popWR$country, matchWith = "country")
ck
ck1 <- checkNamesWR(id = popWR$country_code, matchWith = "iso3", return_logical = TRUE)
ck1
}
