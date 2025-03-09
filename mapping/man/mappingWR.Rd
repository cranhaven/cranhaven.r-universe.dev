\name{mappingWR}
\alias{mappingWR}

\title{Static maps for World countries}

\description{Function to produce static maps for world countries.}

\usage{
mappingWR(data, var = NULL, colID = NULL,
          type = c("static", "interactive"),
          typeStatic = c("tmap", "choro.cart",
                         "typo", "bar"),
          unit = c("country", "nato", "ocde", "continent",
                   "region", "subregion", "region_wb",
                   "type_income", "type_economy"),
          matchWith = c("country", "iso2", "iso3",
                        "iso3_eh", "iso3_numeric",
                        "iso3_un", "iso2_wb", "iso3_wb",
                        "name_formal", "name_wb"),
          res = c("low", "hi"), dir = NULL, show_wr = TRUE,
          add_text = NULL, subset = NULL,
          facets = NULL, aggregation_fun = sum, aggregation_unit = NULL,
          options = mapping.options(legend.position = c("left", "bottom")))
}

\arguments{
  \item{data}{a data.frame object with variables to display or a \code{WR} object produced by \code{\link{WR}} function}
  \item{var}{character value(s) or columns number(s) indicating the variable to plot}
  \item{colID}{character value or columns number indicating the column with unit names}
  \item{type}{if generates static or interactive map}
  \item{typeStatic}{type of static map}
  \item{unit}{the type of world statistical unit}
    \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"country"} \tab if country names). \cr
\code{"iso2"} \tab if iso2 code. \cr
\code{"iso3"} \tab if iso3 code. \cr
\code{"iso3_eh"} \tab if iso3_eh code. \cr
\code{"iso3_numeric"} \tab if iso3 numeric code. \cr
\code{"iso3_un"} \tab if iso3 United Nations. \cr
\code{"iso2_wb"} \tab if iso2 World Bank. \cr
\code{"iso3_wb"} \tab if iso3 World Bank. \cr
\code{"name_formal"} \tab if formal names. \cr
\code{"name_wb"} \tab if World Bank names.
}}
  \item{res}{map resolution}
  \item{dir}{local directory in which shape files are stored}
  \item{show_wr}{logical value set to TRUE indicating if the map entire map is drawn or only the coordinates linked to the input data}
  \item{add_text}{character name indicating the column with text labels }
  \item{subset}{a formula indicating the condition to subset the data. See the details section }
  \item{facets}{variable(s) name to split the data}
  \item{aggregation_fun}{function to use when data are aggregated}
  \item{aggregation_unit}{variable name by which the unit are aggregated}
   \item{options}{a list with options using \code{mapping.options} function}

}
\details{
If \code{data} is a object of class "WR" generated using the \code{\link{WR}} function, the arguments \code{unit}, \code{year}, and \code{matchWith} are ignored, because the object already contains the coordinates.

The \code{aggregation_unit} provides an aggregation for a user specified variable in \code{data}, or for larger statistical unit, automatically provided when the function link the \code{data} with the coordinates.

\code{subset} provide an expression to subsetting the \code{data} using a formula, with the logical operators. For example data can be subsetting as follows: \code{~I("Variable 1" == "condition 1" & "Variable 2" != "condition 2")} or for example, \code{~I("Variable 1" > "condition 1" | "Variable 2" != "condition 2")}.
}
\value{
Return a map. For \code{tmap} type, the function also returns a \code{tmap} object.
}

\references{


  Giraud, T. and Lambert, N. (2016). cartography: Create and Integrate Maps in your R
  Workflow. JOSS, 1(4). doi: 10.21105/joss.00054.

    Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial Vector Data.
  The R Journal 10 (1), 439-446, https://doi.org/10.32614/RJ-2018-009

  Tennekes M (2018). “tmap: Thematic Maps in R.” _Journalstatisticaltical Software_, *84*(6),
1-39. doi: 10.18637/jss.v084.i06 (URL: https://doi.org/10.18637/jss.v084.i06).

  }


\seealso{
\code{\link{mappingEU}},\code{\link{mappingIT},\link{mappingUS}},\code{\link{mappingDE}},\code{\link{mappingUK}}
}

\examples{

data("popWR")
popWR <- popWR


wr <- WR(data = popWR, colID = "country_code",
         matchWith = "iso3_eh", check.unit.names = FALSE,
         res = "low")

###############
#   Statics   #
###############


mappingWR(data = wr, var = "total")

\donttest{
mappingWR(data = wr, var = c("male","female"))
mappingWR(data = wr, var = "total", subset = ~I(iso2 == "IT"))
mappingWR(data = wr, var = "total", subset = ~I(region == "Americas"))

mappingWR(data = wr, var = "total", facets = "continent")
mappingWR(data = wr, var = "total",
          subset = ~I(continent == "South America"),
          facets = "name_wb")
}

\donttest{
mappingWR(data = wr, var = "total", typeStatic = "choro.cart")


mappingWR(data = wr, var = "total", aggregation_unit = "continent",
          aggregation_fun = function(x) sum(x, na.rm = TRUE))
mappingWR(data = wr, var = "total", aggregation_unit = "subregion",
          aggregation_fun = function(x) sum(x, na.rm = TRUE))

## World countries

wr1 <- loadCoordWR()
mappingWR(data = wr1)


###############
# Interactive #
###############


mappingWR(data = wr, var = "total", type = "interactive")
mappingWR(data = wr, var = c("male","female"), type = "interactive")
mappingWR(data = wr, var = "total", subset = ~I(iso2 == "IT"), type = "interactive")
}

\donttest{
mappingWR(data = wr, var = "total",
          subset = ~I(region == "Americas"), type = "interactive")
mappingWR(data = wr, var = "total", type = "interactive",
          aggregation_unit = "continent",
          aggregation_fun = function(x) sum(x, na.rm = TRUE))
mappingWR(data = wr, var = "total", type = "interactive",
          aggregation_unit = "subregion",
          aggregation_fun = function(x) sum(x, na.rm = TRUE))
}

}

