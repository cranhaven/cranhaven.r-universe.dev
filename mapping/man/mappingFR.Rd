\name{mappingFR}
\alias{mappingFR}

\title{Static maps for France}
\description{Function to produce static maps for France statistical unit.}

\usage{
mappingFR(data, var = NULL, colID = NULL, 
          type = c("static", "interactive"), 
          typeStatic = c("tmap", "choro.cart", "typo", "bar"), 
          unit = c("region"), year = c("2021", "2020", "2019"), 
          matchWith = c("name", "code"), 
          dir = NULL, add_text = NULL, subset = NULL, facets = NULL, 
          aggregation_fun = sum, aggregation_unit = NULL, 
          options = mapping.options())
}


\arguments{

  \item{data}{a data.frame object with variables to display or a \code{UK} object produced by \code{\link{FR}} function. If object of class \code{FR},  arguments \code{unit}, \code{year}, and \code{matchWith} will be ignored}
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
\item{add_text}{character name indicating the column with text labels }
  \item{subset}{a formula indicating the condition to subset the data. See the details section}
  \item{facets}{variable(s) name to split the data}
  \item{aggregation_fun}{function to use when data are aggregated}
  \item{aggregation_unit}{variable name by which the unit are aggregated}
  \item{options}{a list with options using \code{mapping.options} function}


}

\details{
If \code{data} is a object of class "UK" generated using the \code{\link{UK}} function, the arguments \code{unit}, and \code{year} are ignored, because the object already contains the coordinates.


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
\code{\link{mappingWR}},\code{\link{mappingEU}}, \code{\link{mappingUS}}, \code{\link{mappingDE}}
}  


\examples{

data("popFR")

\donttest{

fr <- FR(data = popFR)

###############
#   Statics   #
###############

mappingFR(data = fr, var = "population", subset = ~I(region == "corse"))
  

}





}



