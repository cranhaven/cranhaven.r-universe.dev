\name{9.2.look-up.tables}
\alias{9.2.look-up.tables}
\alias{look-up.tables}
\alias{ref.unit.conversions}
\alias{ref.chem}
\alias{ref.petrol}
\alias{ref.diesel}
\alias{pems.scheme}

\docType{data}

\title{
   reference data for use with pems.utils
}

\description{
   Various reference and example datasets intended for use with functions in 
   \code{pems.utils}. 
}

\usage{

   ref.unit.conversions

   ref.chem

   ref.petrol

   ref.diesel

   pems.scheme

}


\format{
  \code{ref.unit.conversions}: Unit conversion methods stored as a list of lists. See Details.

  \code{ref.chem, ref.petrol, ref.diesel}: Common chemical and fuel constants stored as lists.  

  \code{pems.scheme}: Default scheme for \code{pems.utils} plots.

}

\details{
  \code{unit.conversions} is basically a 'look-up' for unit conversion methods. Each element of 
  the list is another list. These lists are each individual conversion methods comprising four 
  elements: \code{to} and \code{from}, character vectors given the unit ids and alias of the unit 
  types that can be converted using the method; \code{conversion}, a function for the associated 
  conversion method; and (possibly) \code{tag}, a more detailed description of the conversion 
  intended for use in documentation.      

  Other \code{ref...} are sets of constants or reference information stored as lists. \code{ref.chem} 
  contains atomic weights of some elements and molecular weights of some species. \code{ref.petrol} and 
  \code{ref.diesel} contain default properties for typical fuels.  

}

\note{
  \code{ref.unit.conversions} can be updated locally. See \code{\link{convertUnits}}, 
  \code{\link{addUnitConversion}}, etc.
}

%\source{
%  TO BE COMPLETED
%}

%\references{
%%  ~~ possibly secondary sources and usages ~~
%website link for .csv and database versions?
%}

\examples{

#basic structure
ref.unit.conversions[[1]]

}

\keyword{datasets}
