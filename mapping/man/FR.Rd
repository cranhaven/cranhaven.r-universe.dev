\name{FR}
\alias{FR}
\title{Object of class \code{FR}}

\description{
Creates an object with data and coordinates of class \code{FR} for France statistical units to use with \code{mapping} functions or available in other R "maps" packages.}


\usage{
FR(data, colID = NULL, unit = c("region"), 
   year = c("2021", "2020", "2019"), matchWith = c("name", "code"), 
   subset = NULL, add = NULL, new_var_names = NULL, 
   aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL, 
   facets = NULL, check.unit.names = TRUE, dir = NULL, use_cache = TRUE, 
   print = FALSE, use_internet = TRUE, crs = NULL)
}


\arguments{

\item{data}{a data.frame object with variables to display}
  \item{colID}{character value or columns number indicating the column with unit names or codes}
 \item{unit}{the type of Italian statistical unit}
  \item{year}{year of the analysis}
    \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"name"} \tab if unit names \cr
\code{"code"} \tab if unit code \cr
}}
  \item{subset}{a formula indicating the condition to subset the data, see the Details}
  \item{add}{a formula to add new transformed variables starting from the variables in the \code{data}}
  \item{new_var_names}{a character value or vector indicating the names of the new variables created in \code{add}}
    \item{aggregation_fun}{function to use when data are aggregated}
    \item{aggregation_unit}{variable name by which the unit are aggregated}
    \item{aggregation_var}{variable name with value to aggregate}
    \item{facets}{variable(s) name to split the data}

  \item{check.unit.names}{a logical value indicating if the \code{colID} names are checked with \code{unit} names}
  \item{dir}{local directory in which shape files are stored}
\item{use_cache}{a logical value indicating whether use the cache}
  \item{print}{a logical value indicating whether print the nomatched names}
\item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used}
    \item{crs}{coordinate reference system. Look at \code{\link{st_crs}}}

}


\details{
The function links (automatically) the id in the \code{data} and the coordinates for the given \code{unit}.

Since the names (or codes) provided in the \code{data} given in input must be checked with the \code{unit} names (or codes)  available in the package,  the \code{check.unit.names} provides a preliminary check.

\code{subset} provide an expression to subset the \code{data}, using a formula with the logical operators. For example, sub-samples of the data can be selected as follows: \code{~I("Variable 1" == "condition 1" & "Variable 2" != "condition 2")} or for example, \code{~I("Variable 1" > "condition 1" | "Variable 2" != "condition 2")}.

}

\value{
An object of class \code{FR}, with data and coordinates to use in functions which perform map.
}


\seealso{\code{\link{EU}}, \code{\link{WR}}, \code{\link{US}}, \code{\link{DE}}}




\examples{

data("popFR")

\donttest{

fr <- FR(data = popFR)

### Adding two varaibles


fr2 <- FR(data = popFR,
          add = ~I(population/1000) + I(population/100) )


### Adding to variables and names
fr3 <- FR(data = popFR,
          add = ~I(population/1000) + I(population/100),
          new_var_names = c("ratio1", "ratio2"))
}


}


