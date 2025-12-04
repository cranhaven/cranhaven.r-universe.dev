\name{showShinyApp}
\alias{showShinyApp}
\title{
Display a 'shiny' application
}
\description{
Displays one of the built-in interactive 'shiny' applications in the browser. See Details for the apps available.
}
\usage{
showShinyApp(topic)
}

\arguments{
  \item{topic}{
The name of the shiny app to display. If missing, a list of available apps will be returned. Partial matching can be used.
}

}
\details{
Three apps are currently included in the \pkg{wiqid} package:

\emph{"Beta"} displays a beta distribution and sliders which allow you to change the parameters. You can also input binomial data and obtain the conjugate beta posterior distribution.

\emph{"Gamma"} displays a gamma distribution with variable parameters, and can produce the conjugate gamma posterior for Poisson-distributed count data.

\emph{"Quadratic"} plots a quadratic relationship with variable parameters, showing how the quadratic term can add a hump or hollow to a relationship.
}
\value{
If \code{topic} is missing, a list of available apps. Otherwise, nothing useful; the function is run for its side effect.
}

\author{
A much simplified version of code by Jason Bryer on GitHub at \url{https://github.com/jbryer/DATA606}, adapted by Mike Meredith.
}

\examples{
showShinyApp() # Shows a list of available apps

}