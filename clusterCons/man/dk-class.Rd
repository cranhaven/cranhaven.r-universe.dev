\name{dk-class}
\docType{class}
\alias{dk-class}

\title{Class "dk"}
\description{
Objects of class \code{'dk'} contain a data.frame which have three variables \code{k}, \code{a} and \code{deltak}
as described in the \code{\link{deltak}} function description. This class simply holds the result from a call to \code{\link{deltak}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("dk", ...)}, although they are normally generated internally by the \code{\link{deltak}} function. 
}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\seealso{
Also see the \code{\link{aucs}} function.
}
\examples{
showClass("dk")
}
\keyword{classes}