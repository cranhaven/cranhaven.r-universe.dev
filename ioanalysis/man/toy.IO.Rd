\name{toy.IO}
\alias{toy.IO}
\alias{toy.IO2}
\docType{data}
\title{
  An example dataset of class \code{InputOutput}
}
\description{
  An object of \code{InputOutput} class created from \code{\link{toy.FullIOTable}} using \code{\link{as.inputoutput}}.
}
\usage{data("toy.IO")}
\format{
  toy.IO is a list with 14 elements: 7 matrices and 7 labels.
}
\value{
  \item{Z}{Intermediate Transactions}
  \item{RS_label}{Column matrix of labels for region and sector}
  \item{f}{Final Demand}
  \item{f_label}{Row matrix of labels for accounts for \code{f}}
  \item{E}{Exports}
  \item{E_label}{Row matrix of labels for exports by sector and region for \code{E}}
  \item{X}{Total Production}
  \item{V}{Value added}
  \item{V_label}{Column matrix of labels for types of value added for \code{V}}
  \item{M}{Imports}
  \item{M_label}{Colum matrix of labels for type of imports for \code{M}}
  \item{A}{Technical Coefficients}
  \item{L}{Leontief Inverse}
}
\examples{
data(toy.IO)
class(toy.IO)
}
\keyword{datasets}
