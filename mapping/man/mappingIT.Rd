\name{mappingIT}
\alias{mappingIT}

\title{Static maps for Italy}
\description{Function to produce static maps for Italian statistical unit.}

\usage{
mappingIT(data, var = NULL, colID = NULL,
          type = c("static", "interactive"),
          typeStatic = c("tmap", "choro.cart", "typo", "bar"),
          unit = c("none", "ripartizione", "regione", "provincia", "comune"),
          year = c("2021", "2020", "2019", "2018", "2017"),
          matchWith = c("name", "code", "number"), dir = NULL, show_it = TRUE,
          add_text = NULL, subset = NULL, facets = NULL,
          aggregation_fun = sum, aggregation_unit = NULL,
          options = mapping.options())
}

\arguments{
  \item{data}{a data.frame object with variables to display or a \code{IT} object produced by \code{\link{IT}} function. If object of class \code{IT},  arguments \code{unit}, \code{year}, and \code{matchWith} will be ignored}
  \item{var}{character value(s) or columns number(s) indicating the variable to plot}
  \item{colID}{character value or columns number indicating the column with unit names}
  \item{type}{if generates static or interactive map}
  \item{typeStatic}{type of static map}
  \item{unit}{the type of Italian statistical unit}
  \item{year}{year of the unit}
  \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"name"} \tab if unit names). \cr
\code{"code"} \tab if unit code }
}
\item{dir}{local directory in which shape files are stored}
  \item{show_it}{logical value set to TRUE indicating if the map entire map is drawn or only the coordinates linked to the input data}
\item{add_text}{character name indicating the column with text labels }
  \item{subset}{a formula indicating the condition to subset the data. See the details section}
  \item{facets}{variable(s) name to split the data}
  \item{aggregation_fun}{function to use when data are aggregated}
  \item{aggregation_unit}{variable name by which the unit are aggregated}
  \item{options}{a list with options using \code{mapping.options} function}

}

\details{
If \code{data} is a object of class "IT" generated using the \code{\link{IT}} function, the arguments \code{unit}, and \code{year} are ignored, because the object already contains the coordinates.

The \code{aggregation_unit} provides an aggregation for a user specified variable in \code{data}, or for larger statistical unit, automatically provided when the function link the \code{data} with the coordinates. For example, if data are of type \code{provicia}, we will have variables for larger aggregate unit, that is \code{regione} and \code{ripartizione} variables. Look at \code{\link{IT}} for more details.

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
\code{\link{mappingWR}},\code{\link{mappingEU}},\code{\link{mappingUS}},\code{\link{mappingDE}},\code{\link{mappingUK}}}


\examples{
data("popIT")

it <- IT(data = popIT, unit = "provincia", year = "2019", check.unit.names = FALSE)

###############
#   Statics   #
###############

mappingIT(data = it, var = "totale")

\donttest{
mappingIT(data = it, var = "totale", subset = ~I(regione == "Lazio"))


mappingIT(data = it, var = "totale", facets =  "ripartizione")

mappingIT(data = it, var = c("maschi","femmine"))
mappingIT(data = it, var = "totale", typeStatic = "choro.cart")


mappingIT(data = it, var = "totale",
          aggregation_unit = "ripartizione",
          aggregation_fun = function(x) sum(x, na.rm = TRUE))

### Italy

it1 <- loadCoordIT()
mappingIT(data = it1)
}


###############
# Interactive #
###############

mappingIT(data = it, var = "totale", type = "interactive")

\donttest{
mappingIT(data = it, var = c("maschi", "femmine"), type = "interactive")


mappingIT(data = it, var = "totale", subset = ~I(regione == "Lazio"), type = "interactive")

mappingIT(data = it, var = "totale", type = "interactive",
                      aggregation_unit = "ripartizione",
                      aggregation_fun = function(x) sum(x, na.rm = TRUE))
}

}
