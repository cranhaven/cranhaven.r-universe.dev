\name{lctools-package}
\alias{lctools-package}
\alias{lctools}
\docType{package}
\title{Local Correlation, Spatial Inequalities, Spatial Regression and Other Tools}
\description{
The main purpose of lctools is to assist spatial analysis researchers and educators to use simple, yet powerful, transparent and user friendly tools for calculating key spatial statistics and fitting spatial models. lctools was originally created to help testing the existence of local multi-collinearity among the explanatory variables of local regression models. The main function (lcorrel) allows for the computation of Local Pearson and Geographically Weighted Pearson Correlation Coefficients and their significance. However, the latter could also be used for examining the existence of local association between pairs of variables. As spatial analysis techniques develop, this package has other spatial statistical tools: the spatial decomposition of the Gini coefficient, the spatial/Focal LQ, global and local Moran's I and tools that help computing variables for Spatial Interaction Models. Since the version 0.2-4, lctools allows for the application of various Geographically Weighted Regression methods including the Geographically Weighted Zero Inflated Poisson Regression recently proposed in the literature (Kalogirou, 2016). This package also contains functions for measuring the significance level for each statistic calculated. The latter mainly refers to Monte Carlo simulations. The package comes with two datasets one of which is a spatial data frame that refers to the Municipalities in Greece.}

\details{
\tabular{ll}{
Package: \tab lctools\cr
Type: \tab Package\cr
Version: \tab 0.2-10\cr
Date: \tab 2024-03-01\cr
License: \tab GPL (>= 2)
}
  
}
\author{
Stamatis Kalogirou

Maintainer: Stamatis Kalogirou <stamatis.science@gmail.com>}

\note{Acknowledgement: I am grateful to the University of Luxembourg and would like to personally thank Ass. Professor Geoffrey Caruso, Professor Markus Hesse and Professor Christian Schulz for their support during my research visit at the Institute of Geography and Spatial Planning (Sept. 2013 - Feb. 2014) where this package was originally developed.}

\references{
Hope, A.C.A. (1968) A Simplified Monte Carlo Significance Test Procedure, Journal of the Royal Statistical Society. Series B (Methodological), 30 (3), pp. 582 - 598.

Kalogirou, S. (2003) The Statistical Analysis and Modelling of Internal Migration Flows within England and Wales, PhD Thesis, School of Geography, Politics and Sociology, University of Newcastle upon Tyne, UK. \url{https://theses.ncl.ac.uk/jspui/handle/10443/204}

Kalogirou, S. (2012) Testing local versions of correlation coefficients, Review of Regional Research - Jahrbuch fur Regionalwissenschaft, 32(1), pp. 45-61, doi: 10.1007/s10037-011-0061-y. \url{https://link.springer.com/article/10.1007/s10037-011-0061-y}

Kalogirou, S. (2013) Testing geographically weighted multicollinearity diagnostics, GISRUK 2013, Department of Geography and Planning, School of Environmental Sciences, University of Liverpool, Liverpool, UK, 3-5 April 2013. 

Kalogirou, S. (2015) Spatial Analysis: Methodology and Applications with R. [ebook] Athens: Hellenic Academic Libraries Link. ISBN: 978-960-603-285-1 (in Greek). \url{https://repository.kallipos.gr/handle/11419/5029?locale=en}

Kalogirou, S. (2016) Destination Choice of Athenians: an application of geographically weighted versions of standard and zero inflated Poisson spatial interaction models, Geographical Analysis, 48(2),pp. 191-230. DOI: 10.1111/gean.12092 \url{https://onlinelibrary.wiley.com/doi/abs/10.1111/gean.12092}

Rey, S.J., Smith, R.J. (2013) A spatial decomposition of the Gini coefficient, Letters in Spatial and Resource Sciences, 6 (2), pp. 55-70.} 

\keyword{package}
\keyword{spatial analysis}
\keyword{geostatistics}
\keyword{spatial inequalities}
\keyword{GWR}