\name{daInsNam}

\alias{daInsNam}

\docType{data}

\title{Liability Insurance Coverage for Hunters and Anglers in Mississippi: Variable Definitions}
\description{This data set contains variable defintions for the a survey result about liability insurance purchase decision by hunters and anglers in Mississippi, as documented in \code{daIns}. It contains 14 observations and 2 columns.
\tabular{ll}{
\code{Variable   } \tab 14 variable abbrevations \cr
\code{Definition } \tab Detailed definitions for the 14 variables \cr
}
}

\usage{data(daInsNam)}

\format{A data frame with 2 columns and 14 rows.}

\details{The data set contains the definitions of 14 variables collected from a telephone survey as detailed in \code{daIns}}

\source{
Sun, C., S. Pokharel, W.D. Jones, S.C. Grado, and D.L. Grebner. 2007. Extent of recreational incidents and determinants of liability insurance coverage for hunters and anglers in Mississippi. Southern Journal of Applied Forestry 31(3):151-158.}

\seealso{\code{\link{daIns}}}

\examples{
data(daInsNam)
str(daInsNam); dim(daInsNam); daInsNam
}
\keyword{datasets}