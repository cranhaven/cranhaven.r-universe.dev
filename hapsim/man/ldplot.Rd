\name{ldplot}

\alias{ldplot}

\title{LD plot}

\description{Creates a linkage disequilibrium plot from a matrix of pair-wise LD coefficients}

\usage{ldplot(ld.mat, ld.type, color = heat.colors(50), title = NULL)}

\arguments{
  \item{ld.mat}{A square matrix of LD coefficients}
  \item{ld.type}{A character value specifying what coefficients are used as input: either 'r' for correlation coefficients or 'd' for D/Dprime scores}
  \item{color}{A range of colors to be used for drawing. Default is \code{heat.colors}}
  \item{title}{Character string for the title of the plot}
}

% \details{}

%\value{}

\references{
Montana, G. HapSim: a simulation tool for generating haplotype data with
pre-specified allele frequencies and LD coefficients. 2005.
}

\author{Giovanni Montana}

%\note{ ~~further notes~~ }

% \seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }

\examples{

data(ACEdata)

# LD plot of ACEdata using r^2 coefficients
ldplot(cor(ACEdata), ld.type='r') 

}

\keyword{ utilities }