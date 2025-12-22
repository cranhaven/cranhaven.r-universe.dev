\name{jscores}
\alias{jscores}
\title{Retrieve jetset scores for probe sets}
\description{This function retrieves jetset scores, which indicate the predicted quality of individual probe sets on selected Affymetrix microarrays.}
\usage{
jscores(chip, probeset, eg, symbol, alias, ensembl)
}
\arguments{
  \item{chip}{ Chip name }
  \item{probeset}{ A vector of probe set IDs (optional) }
  \item{eg}{ A vector of Entrez GeneIDs (optional) }
  \item{symbol}{ A vector of gene symbols (optional) }
  \item{alias}{ A vector of gene aliases (optional) }
  \item{ensembl}{ A vector of Ensembl IDs (optional) }
}
\details{
Currently, \code{chip} can be \code{"hgu95av2"}, \code{"hgu133a"}, \code{"hgu133plus2"}, or \code{"u133x3p"}.  If no further arguments are specified, the scores for all probe sets on the chip are returned.

If any of  \code{probeset}, \code{eg}, \code{symbol}, \code{alias}, or \code{ensembl} are specified, these are used to filter the resulting data frame in a logical OR sense.

Details about the jetset algorithm are available in the vignette.
}
\value{A data frame in which each row corresponds to a probe set, with 8 columns:

    \item{EntrezID}{Entrez GeneID of the targeted gene (character).}
    \item{nProbes}{Number of probes in the probe set  (integer).}
    \item{process}{Processivity requirement  (integer).}
    \item{specificity}{Specificity score (numeric).}
    \item{coverage}{Coverage score (numeric).}
    \item{robust}{Robustness score (numeric).}
    \item{overall}{Overall score (numeric).}
    \item{symbol}{HUGO gene symbol (character).}

The rows are sorted by decreasing overall score.
}

\references{
Qiyuan Li, Nicolai J. Birkbak, Balazs Gyorffy, Zoltan Szallasi and Aron C. Eklund. (2011) Jetset: selecting the optimal microarray probe set to represent a gene. BMC Bioinformatics. 12:474.
}
\seealso{The underlying data comes from (e.g.) \code{\link{scores.hgu95av2}},
with gene symbol lookups coming from \code{\link[org.Hs.eg.db]{org.Hs.egSYMBOL}}.
}
\examples{
  genes <- c('MKI67', 'CHD5', 'ESR1', 'FGF19', 'ERBB2', 'NoSuchGene')

  # This generates several informative warnings
  jscores('hgu133a', symbol = genes)

}
\keyword{ misc }
