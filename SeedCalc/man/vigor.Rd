\name{vigor}
\alias{vigor}
\title{Seed Vigor Index}
\usage{
vigor(lengths, wg = 0.7, wu = 0.3, Unif = 1)
}

\arguments{
  \item{lengths}{ A \code{data.frame} object containing seedling lengths data with four columns. The first and second columns are for identification, e.g. treatments and repetitions. The third is the shoot length and the fourth is the root lenght. }
  \item{wg}{ A numeric value between zero and one. Default is 0.7. Is the weight given to the seedling lenght in the \code{Vigor} index calculation.}
  \item{wu}{ A numeric value between zero and one. Default is 0.3. Is the weight given to the \code{Unif_2} index calculation. }
  \item{Unif}{ A numeric value, 1 or 2. If 1, the \code{unif_1} index is used for vigor index calculation. If 2, the \code{Unif_2} index is used for \code{Vigor} index calculation.  }
}


\description{
It calculates the Seed Vigor Index (Sako, 2001).
}

\references{
SAKO, Y.; MCDONALD, M. B.; FUJIMURA, K.; EVANS, A. F.; BENNETT, M. A. A system for automated seed vigour assessment. Seed Science and Technology, v. 29, n. 3, p. 625-636, 2001.
}

\examples{
Seedling <- data.frame(
LOTE = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
REP = c(1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2),
SH = c(0.00, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 0.64, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 1.98),
ROOT = c(4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 8.75)
)
print(Seedling)
VigInd <- vigor(Seedling, wg = 0.7, wu = 0.3, Unif = 1)
VigInd
}
