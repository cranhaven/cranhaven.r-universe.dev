\name{lar-package}
\alias{lar-package}
\alias{lar}
\docType{package}
\title{
Labour Relations
}
\description{
This package is intended for researchers studying historical labour relations (see http://www.historyoflabourrelations.org). The package allows for easy access of excel files in the standard defined by the Global Collaboratory on the History of Labour Relations. The package also allows for visualisation of labour relations according to the Collaboratory's format.
}
\details{
\tabular{ll}{
Package: \tab lar\cr
Type: \tab Package\cr
Version: \tab 0.1-2\cr
Date: \tab 2014-04-29\cr
License: \tab GPL-3\cr
  }
}
\author{
Richard L. Zijdeman <richard.zijdeman@iisg.nl>
}
\references{
Project website: http://historyoflabourrelations.org
}
\examples{
  \dontrun{
  ## First read in a labour relations formatted excel file:
  df <- read.lar("Spain_1900_(JMTL-Sept2011).xlsx")

  ## Next, draw a treemap of the labour relations:
  draw.lar(df)
  
  ## Treemap with example data set for Spain in 1900
  data(spain.1900)
  draw.lar(spain.1900)
  }
}
