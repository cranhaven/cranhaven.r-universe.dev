\name{nparACT-package}
\alias{nparACT-package}
\alias{nparACT}
\docType{package}
\title{
Non-Parametric Measures of Actigraphy Data
}
\description{
Computes interdaily stability (IS), intradaily variability (IV) and the relative amplitude (RA) from actigraphy data for single or multiple actigraphy files as described in van Someren et al. (1999). Additionally, it also computes the L5 (i.e. the 5 hours with lowest average actigraphy amplitude) and M10 (the 10 hours with highest average amplitude) values as well as the respective start times. The flex versions will also compute the L-value for a user-defined number of minutes. IS describes the strength of coupling of a rhythm to supposedly stable zeitgebers such as sunlight. It varies between 0 (Gaussian Noise) and 1 for perfect IS. IV describes the fragmentation of a rhythm, i.e. the frequency and extent of transitions between rest and activity. It is near 0 for a perfect sine wave, about 2 for Gaussian noise and may be even higher when a definite ultradian period of about 2 hrs is present. RA is the relative amplitude of a rhythm. Note that to obtain reliable results, actigraphy data should cover a reasonable number of days.
}
\details{
\tabular{ll}{
Package: \tab nparACT\cr
Type: \tab Package\cr
Version: \tab 0.8\cr
Date: \tab 2017-12-19\cr
}
}
\author{
Christine Blume <christine.blume@sbg.ac.at>
Nayantara Santhi <n.santhi@surrey.ac.uk>
Manuel Schabus <manuel.schabus@sbg.ac.at>
}
\references{
Blume C., Santhi N., Schabus M. (2016). "nparACT" package for R: A free software tool for the non-parametric analysis of actigraphy data. MethodsX 3: 430-435. <doi:10.1016/j.mex.2016.05.006>
van Someren E.J., Swaab D.F., Colenda C.C., Cohen W., McCall W.V., Rosenquist P.B. (1999). Bright Light Therapy - Improved sensitivity to its effects on rest-activity rhythms in alzheimer patients by application of nonparametric methods. Chronobiology International 16(4): 505-18. <doi:10.3109/07420529908998724>
}
\keyword{ package }
\examples{
# example function nparACT_base
data(sleepstudy)
r <- nparACT_base("sleepstudy", SR = 4/60) 
}