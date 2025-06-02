\name{MLCIRTwithin-package}

\alias{MLCIRTwithin-package}

\alias{MLCIRTwithin}

\docType{package}

\title{Latent Class Item Response Theory (LC-IRT) Models under Within-Item Multidimensionality}

\description{This package provides a flexible framework for the estimation of discrete two-tier Item Response Theory (IRT)  models for the analysis of dichotomous and ordinal polytomous item responses. The class of models at issue is based on the assumption that one or more items are shared by (at most) two latent traits (i.e., within-item multidimensionality) and on the discreteness of latent traits (abilities). 
Every level of the abilities identify a latent class of subjects. The fitting algorithms are based on the Expectation-Maximization (EM) paradigm and allow for missing responses and for different item parametrizations. The package also allows for the inclusion of individual covariates affecting the class weights together with possibility of constraints on all model parameters.
}

\details{
\tabular{ll}{
Package: \tab MultiLCIRT\cr
Type: \tab Package\cr
Version: \tab 2.1.1\cr
Date: \tab 2019-09-30\cr
License: \tab GPL (>= 2)\cr
}
Function \code{est_multi_poly_within} performs the parameter estimation of the same model considered in the \code{R} package \code{MultiLCIRT} when one or more items are shared by two latent traits (within-item multidimensionality); in addition, fixed values and constraints on support points and item parameters are allowed.
}
\author{Francesco Bartolucci, Silvia Bacci - University of Perugia (IT)

Maintainer: Francesco Bartolucci <bart@stat.unipg.it>
}
 \references{
 Adams, R., Wilson, M., and Wang, W. (1997), The multidimensional random coefficients multinomial logit, \emph{Applied Psychological Measurement}, \bold{21}, 1-24.
 
 Bacci, S.  and Bartolucci, F. (2015), A multidimensional finite mixture SEM for non-ignorable missing responses to test items, \emph{Structural Equation Modeling}, \bold{22}, 352-365.

 Bacci, S., Bartolucci, F., and Gnaldi, M. (2014), A class of Multidimensional Latent Class IRT models for 
 ordinal polytomous item responses, \emph{Communications in Statistics - Theory and Methods}, \bold{43}, 787-800.

 Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering
 items, \emph{Psychometrika}, \bold{72}, 141-157.
 
 Bartolucci, F., Bacci, S., and Gnaldi, M. (2015), \emph{Statistical Analysis of Questionnaires: A Unified Approach Based on R and Stata}, Chapman and Hall/CRC press.

 Bartolucci, F., Bacci, S., and Gnaldi, M. (2014), MultiLCIRT: An R package for multidimensional latent class item response models, \emph{Computational Statistics and Data Analysis}, \bold{71}, 971-985.
 
 Bock, R.D., Gibbons, R., and Muraki, E. (1988), Full-information item factor analysis, \emph{Applied Psychological Measurement}, \bold{12}, 261-280.
 
Bonifay, W. E. (2015), An illustration of the two-tier item factor analysis model, in S. P. Reise and D. A. Revicki (eds), Handbook of Item Response Theory Modeling, p. 207-225, Routledge.
 
Cai, L. (2010), A two-tier full-information item factor analysis model with applications, \emph{Psychometrika}, \bold{75}, 581-612.

Cai, L., Yang, J. S., and Hansen, M. (2011), Generalized full-information item bifactor analysis, \emph{Psychological Methods}, \bold{16}, 221-248.

 Gibbons, R. D., Darrell, R. B., Hedeker, D., Weiss, D. J., Segawa, E., Bhaumik, D. K., and Stover, A. (2007), Full-information item bifactor analysis of graded response data, \emph{Applied Psychological Measurement},  \bold{31}, 4-19.
 
 Gibbons, R. D. and Hedeker, D. R. (1992), Full-information item bi-factor analysis, \emph{Psychometrika}, \bold{57}, 423-436.
 }
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
 \examples{
\dontrun{
# Estimation of a two-tier LC-IRT model 
data(SF12_nomiss)
S = SF12_nomiss[,1:12]
X = SF12_nomiss[,13]
# Define matrices to allocate each item on the latent variables  
multi1=rbind(1:6, 7:12)
multi2=rbind(4:8, c(2:3, 10:12))
# Graded response model with two primary latent variables, each of them
# having two dimensions (free discrimination and difficulty parameters;
# two latent classes for both the latent variables; one covariate):
tol = 10^-6 # decrease the tolerance to obtain more reliable results
out1 = est_multi_poly_within(S=S,k1=2,k2=2,X=X,link="global",disc=TRUE,
                             multi1=multi1,multi2=multi2,tol=tol,
                             disp=TRUE,out_se=FALSE) 
# Display output
summary(out1)
}
}
