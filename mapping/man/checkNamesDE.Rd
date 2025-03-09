\name{checkNamesDE}
\alias{checkNamesDE}

\title{Check Germany names}

\description{
Check the differences between the names (or codes) given in input and the names (or codes), of the corresponding selected Germany statistical unit.
}

\usage{
checkNamesDE(id, unit = c("state", "district", "municipal", "municipality"), 
             matchWith = c("name", "code", "code_full"), return_logical = FALSE, 
             print = TRUE, use_internet = TRUE)
}

\arguments{

  \item{id}{character vector with names or codes}
  \item{unit}{the type of European statistical unit to check}
  \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"name"} \tab if unit names \cr
\code{"code"} \tab if unit code \cr
\code{"code_full"} \tab if unit complete code}
}
  \item{return_logical}{a logical value indicating whether nomatched id are returned.}
  \item{print}{a logical value indicating whether print the nomatched names}
  \item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.
  
}
}



\details{
The function provides a check between id name or code in the dataset and the corresponding selected Germany statistical unit. \code{unit} starts from the largest aggregate, "state", to the smallest, "municipality".
}
\value{
Returns a string vector with nomatched names or a boolean vector indicating whether or not the id matched.
}

\author{
Alessio Serafini
}


\seealso{
\code{\link{checkNamesEU}}, \code{\link{checkNamesUS}}, \code{\link{checkNamesWR}}, \code{\link{checkNamesUK}}
}

\examples{
data("popDE")
ck <- checkNamesDE(popDE$code_state, unit = "state", matchWith = "code_full")
str(ck)
}

