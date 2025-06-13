\name{draw.lar}
\alias{draw.lar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Draw Labour Relations
}
\description{
This function draws a treemap of labour relations 
(http://historicallabourrelations.org). 
It is a convenience wrapper for the Treemap package by Martijn 
Tennekens. Before this function is used, data should be setup in a specific data
format (see read.lar()).
}
\usage{
draw.lar(data.frame, sex)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.frame}{A data.frame in labour relations format. Required. See read.lar for more information on the format.}
  \item{sex}{
  One of "total" (default), "male", "female", to indicate respectively: the 
  total population, the male population or the female population.
  }
}

\references{
Project website: http://historyoflabourrelations.org
}
\author{
Richard L. Zijdeman
}
\note{
The creation of this package was made by possible by the International Institute 
of Social History and research grants by the Gerda Henkel Stiftung and the 
Netherlands Organisation for Scientific Research (NWO).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
         \code{\link[lar]{read.lar}, \link{treemap}}.
       }

\examples{
  \dontrun{ 
  ## First read in a labour relations pre-defined excel file:
  df <- read.lar("Spain_1900_(JMTL-Sept2011).xlsx")

  ## Next, draw a treemap of the labour relations:
  draw.lar(df)
  
  ## Treemap with example data set for Spain in 1900
  data(spain.1900)
  draw.lar(spain.1900)
  
  ## Save treemap image as .png
  data(spain.1900)
  ppi <- 300  # define pixels: increase this number for even better quality
  png(file = "tm_spain_1900.png",
      , width=10*ppi, height=8*ppi, res=ppi)
  draw.lar(spain.1900)
  dev.off() 
  }
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ labour }
\keyword{ relations }
\keyword{ treemaps }% __ONLY ONE__ keyword per line
