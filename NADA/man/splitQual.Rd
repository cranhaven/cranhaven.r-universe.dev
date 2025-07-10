\name{splitQual} 
\alias{splitQual} 
\title{Split character qualifiers and numeric values from qualified data} 
\description{
    \code{splitQual} extracts qualified and unqualified vectors 
    from a character vector containing concatenated 
    numeric and qualifying characters.

    Typically used to split ``less-thans" in qualifier-numeric 
    concatenations like ``<0.5".
}
\usage{ splitQual(v, qual.symbol= "<") } 
\arguments{
        \item{v}{
            A character vector.
        } 
        \item{qual.symbol}{
            The qualifier symbol to split from the characters in v.
            Defaults to ``<".
        }
}
\value{
    \code{splitQual} returns a list of three vectors.

    \item{qual}{
        A numeric vector of values associated with qualified input.
    }
    \item{unqual}{
        A numeric vector of values associated with unqualified input 
    }
    \item{qual.index}{
        Indexes of qualified values (ie., where qual.symbol was matched)
    }
    \item{unqual.index}{
        Indexes of unqualified values (ie., where qual.symbol was not matched)
    }
}
\references{

    Lee and Helsel (2005), Statistical analysis of environmental data
    containing multiple detection limits: S-language software for
    regression on order statistics, Computers in Geoscience vol. 31,
    pp. 1241-1248
} 
\author{
    R. Lopaka Lee <rclee@usgs.gov>

    Dennis Helsel <dhelsel@practicalstats.com>
} 
\examples{
    v = c('<1', 1, '<1', 1, 2)
    splitQual(v)
} 
\keyword{utilities} 
