\name{ctree-module}
\alias{ctree-module}
\alias{ctreeUI}
\alias{ctreeServer}

\title{
Shiny module to build and analyse conditional inference trees
}

\description{
The module builds a conditional inference trees according to several parameter inputs. Then it plots the tree and computes performance measures, variable importance, checks the stability and return the code to reproduce the analyses.
}

\usage{
ctreeUI(id)

ctreeServer(id, data, name)
}

\arguments{
\item{id}{Module id. See \code{\link[shiny:callModule]{shiny::callModule()}}.}
\item{data}{\code{\link[shiny:reactive]{shiny::reactive()}} function returning a \code{data.frame} to use for the analyses.}
\item{name}{\code{\link[shiny:reactive]{shiny::reactive()}} function returning a \code{character} string representing \code{data} name.}
}

\references{
Hothorn T, Hornik K, Van De Wiel MA, Zeileis A. "A lego system for conditional inference". \emph{The American Statistician}. 60:257–263, 2006.

Hothorn T, Hornik K, Zeileis A. "Unbiased Recursive Partitioning: A Conditional Inference Framework". \emph{Journal of Computational and Graphical Statistics}, 15(3):651-674, 2006.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{ictree}}
}

\examples{

library(shiny)
library(moreparty)

data(titanic)

ui <- fluidPage(
  titlePanel("Conditional inference trees"),
  ctreeUI(id = "ctree_app")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = titanic,
    name = deparse(substitute(titanic))
  )
  ctreeServer(id = "ctree_app", reactive(rv$data), reactive(rv$name))
}

if (interactive())
  shinyApp(ui, server)
}

\keyword{tree}
