\name{paste}
\alias{paste}
\alias{\%,\%}
\title{The paste operator}
\description{A binary operator shortcut for paste(x,y)}
\usage{
x \%,\% y
}
\arguments{
  \item{x}{a character string}
  \item{y}{a character string}
}

\value{
  The concatenated character string 
}
\author{Grant Izmirlian \email{izmirlian@nih.gov}}

\examples{
  library(pwrFDR)
  "var" \%,\% (1:10)
}
\keyword{character}
