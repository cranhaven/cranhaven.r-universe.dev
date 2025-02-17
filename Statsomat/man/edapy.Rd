\name{edapy}
\alias{edapy}
\title{Exploratory Data Analysis with Python}
\usage{
edapy()
}
\examples{
\dontrun{
library(Statsomat)
edapy()
}
}
\value{
{Shiny app opens in viewer or browser.}
}
\description{
A Shiny app for automated Exploratory Data Analysis with Python, based on the R interface to Python \href{https://rstudio.github.io/reticulate/}{reticulate}. Run the app locally by calling the function or launch it directly in the web from \url{https://statsomat.shinyapps.io/edapy}. Follow the Instructions in the GUI of the app to generate a PDF report or Python code to reproduce numerical and graphical results. Check also the GitHub repository of the app for more details \url{https://github.com/Statsomat/edapy}. System Requirements: Python >=3. Imports \code{numpy, pandas, seaborn, matplotlib, scipy, statsmodels, tabulate, sys, warnings}.
}
