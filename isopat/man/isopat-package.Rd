\name{isopat-package}
\alias{isopat-package}
\alias{isopat}
\docType{package}
\title{
Calculation of isotopic pattern of a chemical formula. 
}
\description{
The function calculates the isotopic pattern (fine structures) for a given chemical formula. 
It returns a list of accurate masses, abundances and isotopic compositions. 
The elements used and their isotopic characteristics can be defined by the user.
}
\details{
\tabular{ll}{
Package: \tab isopat\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2011-04-05\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Starting from a monoisotopic peak of a given molecule, the isotopic compositions and the concomitant abundances of isotopologues are
changed iteratively towards less abundant isotopes over a number of generations 1,...,i,...,n.  
At each generation level i, a number i of isotopes contained in the monoisotopic peak have been exchanged for less abundant isotopes over 
all possible combinatorial changes of size i. 
Thus, generations of isotopic compositions are derived from an initial monoisotopic peak (i=0) while progressing to less abundant isotope 
compositions of a molecule. 
}
\author{
Martin Loos

Maintainer: Martin Loos <loosmart@eawag.ch>
}

\keyword{ package }
\seealso{
  \code{\link[isopat]{isopattern}}
  \code{\link[isopat]{iso_list}}

}
\examples{
## calculate isotopic peaks for coffein-D9 #####################################
data(iso_list);
isopattern(iso_list,"C8H1N4O2D9",1e-10);
}
