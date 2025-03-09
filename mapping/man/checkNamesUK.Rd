\name{checkNamesUK}
\alias{checkNamesUK}
\title{Check United Kingdom names}

\description{
Check the differences between the names (or codes) given in input and the names (or codes) of the corresponding selected United Kingdom statistical unit.
}


\usage{
checkNamesUK(id, unit = c("country", "county"), 
             year = c("2020", "2019"), 
             matchWith = c("name", "code"), 
             scale = c("500", "20"), return_logical = FALSE, 
             print = TRUE, use_internet = TRUE)
}

\arguments{

  \item{id}{character vector with names or codes}
  \item{unit}{the type of European statistical unit to check}
  \item{year}{year of the analysis}
  \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"name"} \tab if unit names \cr
\code{"code"} \tab if unit code}
}
  \item{scale}{the scale of the map.}
  \item{return_logical}{a logical value indicating whether nomatched id are returned.}
  \item{print}{a logical value indicating whether print the nomatched names}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.
  
}
}


\details{
The function provides a check between id name or code in the dataset and the corresponding selected United Kingdom statistical unit. \code{unit} starts from the largest aggregate, "country", to the smallest, "county". Since unit can change over the years, the year of the data has to be provided.
}
\value{
Returns a string vector with nomatched names or a boolean vector indicating whether or not the id matched.
}

\author{
Alessio Serafini
}


\seealso{
\code{\link{checkNamesEU}}, \code{\link{checkNamesUS}}, \code{\link{checkNamesWR}}, \code{\link{checkNamesDE}}
}



\examples{
data("popUK")
ck <- checkNamesUK(popUK$name, unit = "country")
str(ck)
}

