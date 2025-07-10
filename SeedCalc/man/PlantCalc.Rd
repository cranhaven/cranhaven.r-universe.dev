\name{PlantCalc}
\alias{PlantCalc}
\title{PlantCalc Function}
\usage{
PlantCalc(lengths, Ger=100, wr = 90, wh = 10, wg = 0.7, wu = 0.3, Unif = 1)
}

\arguments{
  \item{lengths}{ A \code{data.frame} object containing seedling lengths data with four columns. The first and second columns are for identification, e.g. treatments and repetitions. The third is the shoot length and the fourth is the root lenght. }
  \item{Ger}{ A \code{data.frame} object containing two columns. The first must contain the identification of the treatments identical to the first column of the seedling lenght data. These values is used for \code{vigor_corr} calculation. The default is the numeric value 100 and in this case the \code{vigor_corr} is equal to \code{vigor} index. }
  \item{wr}{ A numeric value between zero and 100. Default is 90. Is the weight given to the root lenght in the \code{Growth} index calculation. }
  \item{wh}{ A numeric value between zero and 100. Default is 10. Is the weight given to the shoot lenght in the \code{Growth} index calculation. }
  \item{wg}{ A numeric value between zero and one. Default is 0.7. Is the weight given to the seedling lenght in the \code{Vigor} index calculation.}
  \item{wu}{ A numeric value between zero and one. Default is 0.3. Is the weight given to the \code{Unif_2} index calculation. }
  \item{Unif}{ A numeric value, 1 or 2. If 1, the \code{unif_1} index is used for Vigor index calculation. If 2, the \code{Unif_2} index is used for \code{Vigor} index calculation.  }
}


\description{
Calculates all indices for seedlings.
}

\author{
Laercio Junio da Silva \email{laerciojdsilva@gmail.com.br}
Andre Dantas de Medeiros \email{medeiros.seeds@gmail.com }
Ariadne Morbeck Santos Oliveira  \email{ariadneoliveira86@gmail.com}
}

\examples{
Seedling <- data.frame(
LOTE = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
REP = c(1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2),
SH = c(0.00, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 0.64, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 1.98),
ROOT = c(4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 8.75)
)
print(Seedling)
SeedlIndexes <- PlantCalc(Seedling)
SeedlIndexes

Ger <- data.frame(LOTE = c(1,2), GER = (c(90,80)))
print(Ger)

SeedlIndexes <- PlantCalc(Seedling, Ger)
SeedlIndexes
}

\keyword{ indexes }
