\name{unif_2}
\alias{unif_2}
\title{Seedling Uniformity Index}
\usage{
unif_2(lengths)
}

\arguments{
  \item{lengths}{ A \code{data.frame} object containing seedling lengths data with four columns. The first and second columns are for identification, e.g. treatments and repetitions. The third is the shoot length and the fourth is the root lenght. }
}


\description{
It calculates the Seedling Uniformity Index (Christiansen, 1942; adapted for Castan et al., 2018).
}

\references{
CASTAN, D. O. C.; GOMES-JUNIOR, F. G.; MARCOS-FILHO, J. Vigor-S, a new system for evaluating the physiological potential of maize seeds. Scientia Agricola, v. 75, n. 2, p. 167-172, 2018.
}

\examples{
Seedling <- data.frame(
LOTE = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
REP = c(1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2),
SH = c(0.00, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 0.64, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 1.98),
ROOT = c(4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 8.75)
)
print(Seedling)
Unif <- unif_2(Seedling)
Unif
}
