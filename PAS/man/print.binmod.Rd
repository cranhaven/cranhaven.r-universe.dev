\name{PAS.binmod.print}
\alias{PAS.binmod.print}
\alias{binmod.print}
\alias{print}
\alias{print.binmod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
binmod.print
}
\description{
Show a terse summarize for a binmod object.
}
\usage{
\method{print}{binmod}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An binmod object.}
  \item{...}{Further parameters may also be supplied as arguments.}

}

\examples{
#load PAS library
library (PAS)
#load the demo data
data (beef)
#conduct bin model analysis.
binmod.result=binmod (x, y, map)
print(binmod.result)
#show structure of a binmod object
str(binmod.result)
}

