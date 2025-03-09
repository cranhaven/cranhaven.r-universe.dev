\name{WR}
\alias{WR}

\title{Object of class \code{WR}}

\description{
Creates an object with data and coordinate of class \code{WR} to use with \code{mapping} function or available in other R "maps" packages.}

\usage{
WR(data, colID = NULL,
   unit = c("country", "nato", "ocde", "continent",
            "region", "subregion", "region_wb",
            "type_income", "type_economy"),
   matchWith = c("country", "iso2", "iso3", "iso3_eh",
                 "iso3_numeric", "iso3_un", "iso2_wb",
                 "iso3_wb", "name_formal", "name_wb"),
   res = c("low", "hi"), show_wr = TRUE, subset = NULL,
   add = NULL, new_var_names = NULL,
   aggregation_fun = sum, aggregation_unit = NULL, aggregation_var = NULL,
   facets = NULL, check.unit.names = TRUE, dir = NULL, use_cache = TRUE,
   print = FALSE, use_internet = TRUE, crs = NULL)
}

\arguments{
\item{data}{a data.frame object with variables to display}
  \item{colID}{character value or columns number indicating the column with unit names}
  \item{unit}{the type of world statistical unit}
    \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"country"} \tab if country names \cr
\code{"iso2"} \tab if iso2 code \cr
\code{"iso3"} \tab if iso3 code \cr
\code{"iso3_eh"} \tab if iso3_eh code \cr
\code{"iso3_numeric"} \tab if iso3 numeric code \cr
\code{"iso3_un"} \tab if iso3 United Nations \cr
\code{"iso2_wb"} \tab if iso2 World Bank \cr
\code{"iso3_wb"} \tab if iso3 World Bank \cr
\code{"name_formal"} \tab if formal names \cr
\code{"name_wb"} \tab if World Bank names
}}
  \item{res}{map resolution}
    \item{show_wr}{logical value set to TRUE indicating if the entire map is drawn or only the coordinates linked to the input data}

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
\item{use_internet}{a logical value indicating wheter the coordinates are downloaded from \url{https://github.com/mappinguniverse/geospatial}. If \code{FALSE} the maps downloaded during package installation will be used}
    \item{crs}{coordinate reference system. Look at \code{\link{st_crs}}}

}


\details{
The function links (automatically) the id in the \code{data} and the coordinates for the given \code{unit}.

Since the names (or codes) provided in the \code{data} given in input must be checked with the \code{unit} names (or codes) available in the package,  the \code{check.unit.names} provides a preliminary check.


\code{subset} provide an expression to subset the \code{data}, using a formula with the logical operators. For example, sub-samples of the data can be selected as follows: \code{~I("Variable 1" == "condition 1" & "Variable 2" != "condition 2")} or for example, \code{~I("Variable 1" > "condition 1" | "Variable 2" != "condition 2")}.


}

\value{
An object of class \code{WR}, with data and coordinates to use in functions which perform map.
}

\seealso{\code{\link{EU}}, \code{\link{IT}}, \code{\link{US}}, \code{\link{DE}}, \code{\link{UK}}}


\examples{

data("popWR")

wr <- WR(data = popWR, colID = "country_code",
         matchWith = "iso3_eh", res = "low")
str(wr, 1)

wr1 <- WR(data = popWR, colID = "country_code",
          matchWith = "iso3_eh", res = "low",
          add = ~I(male/total) + I(female/total))
str(wr1)

wr2 <- WR(data = popWR, colID = "country_code",
          matchWith = "iso3_eh", res = "low",
          add = ~I(male/total) + I(female/total),
          new_var_names = c("Per_Male", "Per_Female"))
str(wr2)

}

