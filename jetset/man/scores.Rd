\name{scores}
\docType{data}
\alias{scores.hgu95av2}
\alias{scores.hgu133a}
\alias{scores.hgu133plus2}
\alias{scores.u133x3p}
\title{Data: Probe set quality scores}
\description{
This data set provides gene target and quality scores for each probe set on the 
corresponding Affymetrix gene expression microarrays. }
\usage{
scores.hgu95av2
scores.hgu133a
scores.hgu133plus2
scores.u133x3p
}
\details{
If there is a relative majority (plurality) of the probes in a probe set that are specific 
for a single gene, this is defined as the \emph{targeted gene}. If no such majority exists, 
the targeted gene is defined as \code{NA}, as are the following scores.

The \emph{processivity requirement} is the number of consecutive bases that
must be synthesized to generate a target that can be detected by the probe set.

The \emph{specificity score} is the fraction of the probes in a probe set 
that are likely to detect the targeted gene and unlikely to detect other genes.

The \emph{coverage score} is the fraction of the splice isoforms
belonging to the targeted gene that are detected by the probe set.

The following two scores are not contained in this data, but are calculated from the above scores; to see them use \code{\link[jetset]{jscores}}.

The \emph{robustness score} quantifies robustness against transcript
degradation.  The robustness score uses the processivity requirement to estimate the 
signal intensity of a probe set, relative to the ideal case of perfect processivity.

The \emph{overall score} is the product of the specificity score, 
coverage score, and robustness score.

All scores can range from 0 to 1.  A higher score indicates better (predicted) performance. 

Details about the jetset algorithm are available in the vignette.

}
\format{A data frame with each row corresponding to a probe set, with 4 columns:
  \describe{
    \item{EntrezID}{Entrez GeneID of the targeted gene (character).}
    \item{process}{Processivity requirement  (integer).}
    \item{specificity}{Specificity score (numeric).}
    \item{coverage}{Coverage score (numeric).}
  }
}
\source{Scores are calculated from BLASTN alignments between probe sequences
  and Refseq transcript sequences, as described in the vignette and in the reference below.
  
The Refseq human RNA was downloaded from NCBI on 2017-04-04. The lookups were based on \pkg{org.Hs.eg.db} version 3.4.0.
  }
\references{
Qiyuan Li, Nicolai J. Birkbak, Balazs Gyorffy, Zoltan Szallasi and Aron C. Eklund. (2011) Jetset: selecting the optimal microarray probe set to represent a gene. BMC Bioinformatics. 12:474.
}
\note{
This data is also available in CSV format from
\url{http://www.cbs.dtu.dk/biotools/jetset/}
}
\seealso{ \code{\link[jetset]{jscores}} for a more convenient way to access this data}
\examples{
  ## Here is the EntrezID for the ESR1 gene
  id <- "2099"
  
  ## Extract the scores for all probe sets detecting ESR1
  scores.hgu95av2[which(scores.hgu95av2$EntrezID == id), ]

  ## Compare to the recommended function 'jscores'
  jscores("hgu95av2", eg = "2099")

}
\keyword{datasets}
