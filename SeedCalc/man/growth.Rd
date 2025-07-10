\name{growth}
\alias{growth}
\title{Seedling Growth Index}
\usage{
growth(lengths, wr = 90, wh = 10)
}

\arguments{
  \item{lengths}{ A \code{data.frame} object containing seedling lengths data with four columns. The first and second columns are for identification, e.g. treatments and repetitions. The third is the shoot length and the fourth is the root length. }
  \item{wr}{ A numeric value between zero and 100. Default is 90. Is the weight given to the root lenght in the \code{Growth} index calculation. }
  \item{wh}{ A numeric value between zero and 100. Default is 10. Is the weight given to the shoot lenght in the \code{Growth} index calculation. }
}


\description{
It calculates the Seedling Growth Index (Sako, 2001).
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
Unif <- growth(Seedling, wr = 90, wh = 10)
Unif
}
