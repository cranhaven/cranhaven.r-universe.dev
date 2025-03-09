\name{EU}
\alias{EU}

\title{Object of class \code{EU}}

\description{
Creates an object with data and coordinates of class \code{EU} for European countries to use with \code{mapping} functions or available in other R "maps" packages.}

\usage{
EU(data, colID = NULL,
               unit = c("nuts0","nuts1", "nuts2", "nuts3", "urau"),
               year = c("2021","2016", "2013", "2010", "2006", "2003"),
               matchWith = c("nuts", "id", "iso2", "iso3", "country_code"),
               scale = c("20", "60"), show_eu = TRUE,
               subset = NULL, add = NULL, new_var_names = NULL,
               aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL,
               facets = NULL, check.unit.names = TRUE, dir = NULL,
               use_cache = TRUE, print = FALSE, use_internet = TRUE, crs = NULL)
}

\arguments{
\item{data}{a data.frame object with variables to display}
  \item{colID}{character value or columns number indicating the column with unit names}
  \item{unit}{the type of European statistical unit}
  \item{year}{year of the analysis}
 \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"nuts"} \tab if nuts names \cr
\code{"id"} \tab if nuts id \cr
\code{"iso2"} \tab if iso2 code \cr
\code{"iso3"} \tab if iso3 code \cr
\code{"country_code"} \tab if Eurostat code }
}
  \item{scale}{the scale of the map}
    \item{show_eu}{logical value set to TRUE indicating if the entire map is drawn or only the coordinates linked to the input data}

    \item{subset}{a formula indicating the condition to subset the data, see the Details }
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
\item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used.}
    \item{crs}{coordinate reference system. Look at \code{\link{st_crs}}}


}

\details{
The function links (automatically) the id in the \code{data} and the coordinates for the given \code{unit}.

Since the names (or codes) provided in the \code{data} given in input must be checked with the \code{unit} names (or codes)  available in the package (as provided by Eurostat),  the \code{check.unit.names} provides a preliminary check.

\code{subset} provide an expression to subset the \code{data}, using a formula with the logical operators. For example, sub-samples of the data can be selected as follows: \code{~I("Variable 1" == "condition 1" & "Variable 2" != "condition 2")} or for example, \code{~I("Variable 1" > "condition 1" | "Variable 2" != "condition 2")}.

}

\value{
An object of class \code{EU}, with data and coordinates to use in functions which perform map.
}

\seealso{\code{\link{WR}}, \code{\link{IT}}, \code{\link{US}}, \code{\link{DE}}, \code{\link{UK}}}

\examples{

data("popEU")

popEU <- popEU

euNuts2 <- EU(data = popEU, colID = "GEO",
              unit = "nuts2",matchWith = "id")
str(euNuts2)

\donttest{


euNuts2_1 <- EU(data = popEU, colID = "GEO",
                unit = "nuts2",matchWith = "id",
                add = ~I(male/total) + I(female/total))
str(euNuts2_1)

euNuts2_2 <- EU(data = popEU, colID = "GEO",
                unit = "nuts2",matchWith = "id",
                add = ~I(male/total) + I(female/total),
                new_var_names = c("Per_Male", "Per_Female"))
str(euNuts2_2)
}
}

