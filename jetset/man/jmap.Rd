\name{jmap}
\alias{jmap}
\title{Retrieve jetset mapped probe sets}
\description{This function retrieves probe sets corresponding to the queried genes}
\usage{
jmap(chip, eg, symbol, alias, ensembl)
}
\arguments{
  \item{chip}{ Chip name }
  \item{eg}{ A vector of Entrez GeneIDs (optional) }
  \item{symbol}{ A vector of gene symbols (optional) }
  \item{alias}{ A vector of gene aliases (optional) }
  \item{ensembl}{ A vector of Ensembl IDs (optional) }
}
\details{
Currently, \code{chip} can be \code{"hgu95av2"}, \code{"hgu133a"}, \code{"hgu133plus2"}, or \code{"u133x3p"}.  

Queried genes must be specified by either \code{eg}, \code{symbol}, \code{alias}, or \code{ensembl}.

If the query is not recognized, or is ambiguous, or corresponds to a gene
that is not detected by the array, \code{NA} will be returned.

Details about the jetset algorithm are available in the vignette.
}
\value{A character vector of probe set IDs}
\references{
Qiyuan Li, Nicolai J. Birkbak, Balazs Gyorffy, Zoltan Szallasi and Aron C. Eklund. (2011) Jetset: selecting the optimal microarray probe set to represent a gene. BMC Bioinformatics. 12:474.
}
\seealso{The underlying Entrez ID to probeset data is available in (e.g.) \code{\link{scores.hgu95av2}}.
Symbol, alias, and ensembl lookups are generated from e.g.
\code{\link[org.Hs.eg.db]{org.Hs.egSYMBOL2EG}}.
}
\examples{
  genes <- c('MKI67', 'CHD5', 'ESR1', 'FGF19', 'ERBB2', 'NoSuchGene')

  # This generates several informative warnings
  jmap('hgu133a', symbol = genes)

}
\keyword{ misc }
