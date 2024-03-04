\name{sig}
\alias{sig}

\title{
Significance level cheking function
}
\description{
Function that return if the p-value allows to accept H0 in a Kolmogorov Smirnov or Anderson Darling test

}
\usage{
sig(n)
}

\arguments{
  \item{n}{
p-value
}
}

\value{
Text string ("Significant"" / "Not Significant"")
}

\examples{
sig ( 0.001 )

sig ( 0.1 )
}

\keyword{ methods }