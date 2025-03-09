\name{mappingDE}
\alias{mappingDE}

\title{Static maps for Germany}
\description{Function to produce static maps for Germany statistical unit.}


\usage{
mappingDE(data, var = NULL, colID = NULL, 
          type = c("static", "interactive"), 
          typeStatic = c("tmap", "choro.cart", "typo", "bar"), 
          unit = c("state", "district", "municipal", "municipality"), 
          matchWith = c("name", "code", "code_full"), dir = NULL, 
          add_text = NULL, subset = NULL, facets = NULL, 
          aggregation_fun = sum, aggregation_unit = NULL, 
          options = mapping.options())
}

\arguments{

  \item{data}{a data.frame object with variables to display or a \code{DE} object produced by \code{\link{DE}} function. If object of class \code{DE},  arguments \code{unit}, \code{year}, and \code{matchWith} will be ignored}
  \item{var}{character value(s) or columns number(s) indicating the variable to plot}
  \item{colID}{character value or columns number indicating the column with unit names}
  \item{type}{if generates static or interactive map}
  \item{typeStatic}{type of static map}
  \item{unit}{the type of Italian statistical unit}
  \item{matchWith}{the type of id to check:
    \tabular{ll}{
\code{"name"} \tab if unit names \cr
\code{"code"} \tab if unit code \cr
\code{"code_full"} \tab if unit complete code}
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
If \code{data} is a object of class "DE" generated using the \code{\link{DE}} function, the argument \code{unit}, because the object already contains the coordinates.

The \code{aggregation_unit} provides an aggregation for a user specified variable in \code{data}, or for larger statistical unit, automatically provided when the function link the \code{data} with the coordinates. For example, if data are of type \code{municipal}, we will have variables for larger aggregate unit, that is \code{district} and \code{state} variables. Look at \code{\link{DE}} for more details.

\code{subset} provide an expression to subsetting the \code{data} using a formula, with the logical operators. For example data can be subsetting as follows: \code{~I("Variable 1" == "condition 1" & "Variable 2" != "condition 2")} or for example, \code{~I("Variable 1" > "condition 1" | "Variable 2" != "condition 2")}.

}
\value{
Return a map. For \code{tmap} type, the function also returns a \code{tmap} object.

}


\seealso{
\code{\link{mappingWR}},\code{\link{mappingEU}}, \code{\link{mappingUS}}, \code{\link{mappingUK}}
}

\examples{

data("popDE")

de <- DE(data = popDE, colID = "code_state", 
         unit = "state", matchWith = "code_full", 
         check.unit.names = FALSE)

\donttest{
###############
#   Statics   #
###############

mappingDE(data = de, var = "population_2020")


mappingDE(data = de, var = "population_2020", 
            subset = ~I(state == "bayern"))
  
}


###############
# Interactive #
###############

mappingDE(data = de, var = "population_2020", type = "interactive")

\donttest{

  
  mappingDE(data = de, var = "population_2020", 
            subset = ~I(state == "bayern"), 
            type = "interactive")

}

}

