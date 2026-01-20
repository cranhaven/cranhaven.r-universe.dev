\name{DEMOVA-package}
\alias{DEMOVA-package}
\alias{DEMOVA}
\docType{package}
\title{
DEvelopment of (multi-linear QSPR/QSAR) MOdels VAlidated using test set.
}
\description{
Tool for the development of multi-linear QSPR/QSAR models (Quantitative structure-property/activity relationship). Theses models are used in chemistry, biology and pharmacy to find a relationship between the structure of a molecule and its property (such as activity, toxicology but also physical properties). The various functions of this package allows: selection of descriptors based of variances, intercorrelation and user expertise; selection of the best multi-linear regression in terms of correlation and robustness; methods of internal validation (Leave-One-Out, Leave-Many-Out, Y-scrambling) and external using test sets.
}
\details{
\tabular{ll}{
Package: \tab DEMOVA\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-03-15\cr
License: \tab GPL (>= 2)\cr
}
Example of input files are avaible into the floder "tests". \cr
# data<-read.csv("NameOfInputFile.csv",header = TRUE , sep=" ")  \cr
# mydesc<-data[,3:dim[2]] \cr
Functions should be use in this order: \cr 
- preselection \cr 
- select_variables \cr 
- select_MLR \cr 
- fit \cr 
- LOO / LMO / Scramb  (No specific order between these ones. Optional to do the rest) \cr 
- prediction \cr 
- graphe_3Sets \cr 
}

\references{
1. Selassie, C. D. History of Quantitative Structure-Activity Relationship; Burger's Medicinal Chemistry and Drug Discovery Sixth Edition; John Wiley & Sons Inc., 2002; Vol. 1. (2)  \cr
2. Willett, P. Chemoinformatics: a History. Wiley Interdisciplinary Reviews: Computational Molecular Science 2011, 1, 46-56.
}

\author{
Vinca Prana \cr
Maintainer: Vinca Prana <vinca.prana@free.fr>
}

\keyword{ package }
\keyword{ chemoinformatics }
