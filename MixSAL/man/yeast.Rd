\name{yeast}
\alias{yeast}
\docType{data}
\title{Yeast Data}
\description{Subset of the yeast dataset from Nakai and Kanehisa (1991,1992). This subset contains three variables: McGeoch's method for signal sequence recognition (mcg), the score of the ALOM membrane spanning region prediction program (alm), and the score of discriminant analysis of the amina acid content of vacuolar and extracellular protiens (vac).}
\usage{data(yeast)}
\source{UCI macnine learning respository.}
\format{A vector containing 141 observations.}
\references{
Franczak et. al (2014). Mixtures of Shifted Asymmetric Laplace Distributions. IEEE Transactions on Pattern Analysis and Machine Intelligence, 38(6), 1149-1157.

Nakai, N. and Kanehisa, M. (1991). Expert System for Predicting Protein Localization Sites in Gram-Negative Bacteria. Proteins, 11(2), 95-110.

Nakai, N. and Kanehisa, M. (1992). A Knowledge Base for Predicting Protein Loczalization Sites in Eukaryotic Cells. Genomics, 14(4), 897-911.
}
\examples{
data(yeast) # Loads the subset of the yeast data set
head(yeast) # Displays the first six rows of this subset of the yeast data set
}
\keyword{datasets}
\keyword{yeast}
