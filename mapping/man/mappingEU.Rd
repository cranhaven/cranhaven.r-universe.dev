\name{mappingEU}
\alias{mappingEU}

\title{Static maps for Europe}
\description{Function to produce static maps for European statistical unit.}

\usage{
mappingEU(data, var = NULL, colID = NULL,
          type = c("static", "interactive"),
          typeStatic = c("tmap", "choro.cart", "typo", "bar"),
          unit = c("nuts0", "nuts1", "nuts2", "nuts3", "urau"),
          year = c("2021","2016", "2013", "2010", "2006", "2003"),
          matchWith = c("nuts", "id", "iso2", "iso3", "country_code"),
          scale = c("20", "60"), dir = NULL, show_eu = TRUE,
          add_text = NULL, subset = NULL, facets = NULL,
          aggregation_fun = sum, aggregation_unit = NULL,
          options = mapping.options())
}

\arguments{
  \item{data}{a data.frame object with variables to display or a \code{EU} object produced by \code{\link{EU}} function}
  \item{var}{character value(s) or columns number(s) indicating the variable to plot}
  \item{colID}{character value or columns number indicating the column with unit names}
  \item{type}{if generates static or interactive map}
  \item{typeStatic}{type of static map}
  \item{unit}{the type of European statistical unit to check}
  \item{year}{year of the unit}
 \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"nuts"} \tab if nuts names). \cr
\code{"id"} \tab if nuts id. \cr
\code{"iso2"} \tab if iso2 code. \cr
\code{"iso3"} \tab if iso3 code. \cr
\code{"country_code"} \tab if Eurostat code }
}
  \item{scale}{the scale of a map}
  \item{dir}{local directory in which shape files are stored}
    \item{show_eu}{logical value set to TRUE indicating if the map entire map is drawn or only the coordinates linked to the input data}
\item{add_text}{character name indicating the column with text labels }
  \item{subset}{a formula indicating the condition to subset the data. See the details}
  \item{facets}{variable(s) name to split the data}
  \item{aggregation_fun}{function to use when data are aggregated}
  \item{aggregation_unit}{variable name by which the unit are aggregated}
   \item{options}{a list with options using \code{mapping.options} function}

}

\details{

If \code{data} is a object of class "EU" generated using the \code{\link{EU}} function, the arguments \code{unit}, \code{year}, and \code{matchWith} are ignored, because the object already contains the coordinates.

The \code{aggregation_unit} provides an aggregation for a user specified variable in \code{data}, or for larger statistical unit, automatically provided when the function link the \code{data} with the coordinates. For example, if data are of type \code{nut2}, we will have variables for larger aggregate unit, that is \code{nuts1} and \code{nuts0} variables. Look at \code{\link{EU}} for more details.

\code{subset} provide an expression to subsetting the \code{data} using a formula, with the logical operators. For example data can be subsetting as follows: \code{~I("Variable 1" == "condition 1" & "Variable 2" != "condition 2")} or for example, \code{~I("Variable 1" > "condition 1" | "Variable 2" != "condition 2")}.

}

\value{
Returns a map. For \code{tmap} type, the function also returns a \code{tmap} object.
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
\code{\link{mappingWR}},\code{\link{mappingIT}},\code{\link{mappingUS}},\code{\link{mappingDE}},\code{\link{mappingUK}}}


\examples{


data("popEU")
popEU <- popEU
euNuts2 <- EU(data = popEU, colID = "GEO",unit = "nuts2",matchWith = "id")

###############
#   Statics   #
###############


mappingEU(data = euNuts2, var = "total")
mappingEU(data = euNuts2, var = c("male","female"))

mappingEU(data = euNuts2, var = "total", subset = ~I(nuts0_id == "IT"))
mappingEU(data = euNuts2, var = "total",
          subset = ~I(nuts0_id == "ES"), facets = "nuts2")

\donttest{
mappingEU(data = euNuts2, var = "total", typeStatic = "choro.cart")

mappingEU(data = euNuts2, var = "total", aggregation_unit = "nuts0", aggregation_fun = sum)
mappingEU(data = euNuts2, var = c("male","female"),
          aggregation_unit = "nuts0", aggregation_fun = sum)

### Europe

eu1 <- loadCoordEU()
mappingEU(data = eu1)
}


###############
# Interactive #
###############
\donttest{
mappingEU(data = euNuts2, var = "total", type = "interactive")
mappingEU(data = euNuts2, var = c("male","female"), type = "interactive")

mappingEU(data = euNuts2, type = "interactive",
          var = "total", subset = ~I(nuts0_id == "IT"))
mappingEU(data = euNuts2, var = "total", type = "interactive",
          subset = ~I(nuts0_id == "ES"))


mappingEU(data = euNuts2, var = "total", type = "interactive")

mappingEU(data = euNuts2, var = "total", type = "interactive",
          aggregation_unit = "nuts0",
          aggregation_fun = sum)
mappingEU(data = euNuts2, var = c("male","female"), type = "interactive",
          aggregation_unit = "nuts0", aggregation_fun = sum)

}

}

