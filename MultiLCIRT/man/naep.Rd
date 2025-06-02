\name{naep}
\alias{naep}
\docType{data}
\title{NAEP dataset}

\description{
This dataset contains the responses of a sample of 1510 examinees to 12 binary items on Mathematics. It has been
extrapolated from a larger dataset collected in 1996 by the Educational Testing Service within the National Assessment of 
Educational Progress (NAEP) project.}

\usage{data(naep)}
\format{
  A data frame with 1510 observations on the following 12 items:
  \describe{
    \item{\code{Item1}}{round to thousand place}
    \item{\code{Item2}}{write fraction that represents shaded region}
    \item{\code{Item3}}{multiply two negative integers}
    \item{\code{Item4}}{reason about sample space (number correct)}
    \item{\code{Item5}}{find amount of restaurant tip}
    \item{\code{Item6}}{identify representative sample}
    \item{\code{Item7}}{read dials on a meter}
    \item{\code{Item8}}{find (x, y) solution of linear equation}
    \item{\code{Item9}}{translate words to symbols}
    \item{\code{Item10}}{find number of diagonals in polygon from a vertex}
    \item{\code{Item11}}{find perimeter (quadrilateral)}
    \item{\code{Item12}}{reason about betweenness}
  }
}

%\details{
%%%  ~~ If necessary, more details than the __description__ above ~~
%}

%\source{
%%%  ~~ reference to a publication or URL from which the data were obtained ~~
%% Vedere in Bartolucci e Forcina o da altra parte la fonte originale dei dati Naep
%}

\references{
Bartolucci, F. and Forcina, A. (2005), Likelihood inference on the underlying structure of IRT models. \emph{Psychometrika},
\bold{70}, 31-43. 

Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering items. 
\emph{Psychometrika}, \bold{72}, 141-157.
}
\examples{
data(naep)
## maybe str(naep)
str(naep)
}
\keyword{datasets}
\keyword{education}