\name{auc-class}
\docType{class}
\alias{auc-class}

\title{Class "auc"}
\description{
Objects of class \code{'auc'} contain a data.frame which have three variables \code{k}, \code{a} and \code{auc}
as described in the \code{\link{aucs}} function description. This class simply holds the result from a call to \code{\link{aucs}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("auc", ...)}, although they are normally generated internally by the \code{\link{aucs}} function. 
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
showClass("auc")
}
\keyword{classes}
