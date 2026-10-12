\name{MaxentVariableSelection-package}
\alias{MaxentVariableSelection}
\docType{package}
\title{Selecting the Best Set of Relevant Environmental Variables along with the
Optimal Regularization Multiplier for Maxent Niche Modeling}

\description{Complex niche models show low performance in identifying
    the most important range-limiting environmental variables and in
    transferring habitat suitability to novel environmental conditions
    (Warren and Seifert, 2011 <DOI:10.1890/10-1171.1>; Warren et al.,
    2014 <DOI:10.1111/ddi.12160>). This package helps to identify the
    most important set of uncorrelated variables and to fine-tune Maxent's
    regularization multiplier. In combination, this allows to constrain
    complexity and increase performance of Maxent niche models (assessed
    by information criteria, such as AICc (Akaike, 1974
    <DOI:10.1109/TAC.1974.1100705>), and by the area under the receiver
    operating characteristic (AUC) (Fielding and Bell, 1997
    <DOI:10.1017/S0376892997000088>). Users of this package should be
    familiar with Maxent niche modelling.}

\details{
\tabular{ll}{
Package: \tab MaxentVariableSelection\cr
Type: \tab Package\cr
Version: \tab 1.0-3\cr
Date: \tab 2018-01-23\cr
Depends: \tab R (>= 3.1.2)\cr
Imports: \tab ggplot2, raster\cr
Suggests: \tab knitr, rmarkdown\cr
VignetteBuilder: \tab knitr\cr
License: \tab GPL (>= 2)\cr
Literature: \tab Akaike H (1974)\cr
\tab A new look at the statistical model identification\cr
\tab \emph{IEEE Transactions on Automatic Control} \bold{19}:6 716--723.\cr
\tab \cr
\tab Fielding AH and Bell JF (1997)\cr
\tab A review of methods for the assessment of prediction\cr
\tab errors in conservation presence/absence models\cr
\tab \emph{Environmental Conservation} \bold{24}:1 38--49.\cr
\tab \cr
\tab Jimenez-Valverde A (2012)\cr
\tab Insights into the area under the receiver operating characteristic curve\cr
\tab (AUC) as a discrimination measure in species distribution modelling\cr
\tab \emph{Global Ecology and Biogeography} \bold{21}:4 498--507.\cr
\tab \cr
\tab Tyberghein L, Verbruggen H, Pauly K, Troupin C, Mineur F and {De Clerck}, O (2012)\cr
\tab Bio-ORACLE: a global environmental dataset for marine species distribution modelling\cr
\tab \emph{Global Ecology and Biogeography} \bold{21}:2 272--281.\cr
\tab \cr
\tab Warren DL, Glor RE, and Turelli M (2010)\cr
\tab ENMTools: a toolbox for comparative studies of environmental niche\cr
\tab models\cr
\tab \emph{Ecography} \bold{33}:3 607--611.\cr
\tab \cr
\tab Warren DL and Seifert SN (2011)\cr
\tab Ecological niche modeling in Maxent: the importance of model\cr
\tab complexity and the performance of model selection criteria\cr
\tab \emph{Ecological Applications} \bold{21}:2 335--342.\cr
}
}

\author{
Alexander Jueterbock

Maintainer: Alexander Jueterbock, <Alexander-Jueterbock@web.de> }

\section{Citation}{To cite the package 'MaxentVariableSelection' in publications use:\cr\cr
Jueterbock A, Smolina I, Coyer JA and  Hoarau, G (2016)\cr
The fate of the Arctic seaweed \emph{Fucus distichus} under climate change:\cr
an ecological niche modelling approach\cr
\emph{Ecology and Evolution} \bold{6}(6), 1712-1724}

\keyword{package}
