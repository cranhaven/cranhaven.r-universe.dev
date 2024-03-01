\name{itpdat}
\docType{data}
\alias{itpdat}
\title{Data on MMR and ITP}
\description{
The data comprise ages in days at measles, mumps and rubella (MMR) vaccination and hospital admission for idiopathic thrombocytopaenic purpura (ITP). There are 44 admissions in 35 children.
}
\usage{itpdat}
\format{A data frame containing 44 rows and 9 columns. The column names are 'case' (individual identifier), 'sta' (age on first day of the observation period), 'end' (age on last day of the observation period), 'itp' (age at admission for ITP), 'mmr' (age at mmr vaccination), 'sex' (1 for males, 2 for females), 'gap' (days from ITP admission to next ITP admission within the same case, or to end of observation), 'cen' (0 if last admission for this case, 1 otherwise), 'rec' (within-case event number).}
\source{
Whitaker, H. J., Farrington, C. P., Spiessens, B., and Musonda, P. (2006). Tutorial in biostatistics: The self-controlled case series method. Statistics in Medicine 25, 1768--1797.
}
\references{
Miller, E., Waight, P., Farrington, P., Andrews, N., Stowe, J., and Taylor B. (2001). Idiopathic thrombocytopenic purpura and MMR vaccine. Archives of Disease in Childhood 84, 227--229.
}
\keyword{datasets}
