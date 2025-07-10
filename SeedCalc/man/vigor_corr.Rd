\name{vigor_corr}
\alias{vigor_corr}
\title{Seed Corrected Vigor Index}
\usage{
vigor_corr(lengths, Ger = 100, wg = 0.7, wu = 0.3, Unif = 1)
}

\arguments{
  \item{lengths}{ A \code{data.frame} object containing seedling lenghts data with four columns. The first and second columns are for identification, e.g. treatments and repetitions. The third is the shoot lenght and the fourth is the root lenght. }
  \item{Ger}{ A \code{data.frame} object containing two columns. The first must contain the identification of the treatments identical to the first column of the seedling length data. The default is the numeric value 100 and in this case the \code{vigor_corr} is equal to \code{vigor} index. }
  \item{wg}{ A numeric value between zero and one. Default is 0.7. Is the weight given to the seedling lenght in the \code{Vigor} index calculation.}
  \item{wu}{ A numeric value between zero and one. Default is 0.3. Is the weight given to the \code{Unif_2} index calculation. }
  \item{Unif}{ A numeric value, 1 or 2. If 1, the \code{unif_1} index is used for vigor index calculation. If 2, the \code{Unif_2} index is used for \code{Vigor} index calculation.  }
}


\description{
It calculates the Seed Corrected Vigor Index (Medeiros & Pereira, 2018).
}

\references{
MEDEIROS, A. D. DE; PEREIRA, M. D. SAPL: a free software for determining the physiological potential in soybean seeds. Pesquisa Agropecuaria Tropical, v. 48, n. 3, p. 222-228, 2018.
}

\examples{
Seedling <- data.frame(
LOTE = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2),
REP = c(1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2),
SH = c(0.00, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 0.64, 2.77, 1.18, 1.07, 0.80, 2.65, 3.51, 1.98),
ROOT = c(4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 4.86, 6.71, 7.88, 3.68, 9.68, 8.88, 9.85, 8.75)
)
print(Seedling)

Ger <- data.frame(LOTE = c(1,2), GER = (c(90,80)))
print(Ger)

VigInd <- vigor_corr(Seedling, Ger, wg = 0.7, wu = 0.3, Unif = 1)
VigInd
}
