\name{mappingUS}
\alias{mappingUS}

\title{Static maps for USA}

\description{
Function to produce static maps for USA unit.}

\usage{
mappingUS(data, var = NULL, colID = NULL,
          type = c("static", "interactive"),
          typeStatic = c("tmap", "choro.cart", "typo", "bar"),
          unit = c("country", "region", "division","state",
                   "county", "district", "district_county", "urban_area"),
          year = c("2018"), matchWith = c("name", "id", "number"),
          scale = c("20", "50", "500"), dir = NULL, show_us = TRUE,
          add_text = NULL, subset = NULL, facets = NULL,
          aggregation_fun = sum, aggregation_unit = NULL,
          options = mapping.options())
}

\arguments{
  \item{data}{a data.frame object with variables to display or a \code{US} object produced by \code{\link{US}} function}
  \item{var}{character value(s) or columns number(s) indicating the variable to plot}
\item{colID}{character value or columns number indicating the column with unit names}
    \item{type}{if generates static or interactive map}
  \item{typeStatic}{type of static map}
  \item{unit}{the type of European statistical unit to check.}
  \item{year}{year of the unit}
  \item{matchWith}{the type of id to check if unit is set to "states"}
 \item{scale}{the scale of a map}
 \item{dir}{local directory in which shape files are stored}
  \item{show_us}{logical value set to TRUE indicating if the map entire map is drawn or only the coordinates linked to the input data}
  \item{add_text}{character name indicating the column with text labels }
  \item{subset}{a formula indicating the condition to subset the data. See the details section}
  \item{facets}{variable(s) name to split the data}
  \item{aggregation_fun}{function to use when data are aggregated}
  \item{aggregation_unit}{variable name by which the unit are aggregated}
   \item{options}{a list with options using \code{mapping.options} function}
}

\details{

If \code{data} is a object of class "US" generated using the \code{\link{US}} function, the arguments \code{unit}, \code{year}, and \code{matchWith} are ignored, because the object already contains the coordinates.

The \code{aggregation_unit} provides an aggregation for a user specified variable in \code{data}, or for larger statistical unit, automatically provided when the function link the \code{data} with the coordinates. For example, if data are of type \code{county}, we will have variables for larger aggregate unit, that is \code{state} and \code{region} variables. Look at \code{\link{US}} for more details.

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
\code{\link{mappingWR}},\code{\link{mappingIT}},\code{\link{mappingEU}},\code{\link{mappingDE}},\code{\link{mappingUK}}
}

\examples{

data("popUS")

us <- US(data = popUS, unit = "state")

###############
#   Statics   #
###############


mappingUS(data = us, var = "population")
mappingUS(data = us, var = "population",
          subset = ~I(id == "california" | id == "texas"))
mappingUS(data = us, var = "population",
          subset = ~I(id == "california" | id == "texas"), facets = "id")

\donttest{
mappingUS(data = us, var = "population", typeStatic = "choro.cart")


###############
# Interactive #
###############


mappingUS(data = us, var = "population", type = "interactive")
mappingUS(data = us, var = "population", type = "interactive",
                      subset = ~I(id == "california" | id == "texas" | id == "new york" ))

}
}

