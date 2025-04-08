\name{SDSS_ptsrc_train}
\docType{data}
\alias{SDSS_ptsrc_train}

\title{Sloan Digital Sky Survey point source photometry: Training sample}
\description{
Together with SDSS_ptsrc_teset, these two datasets are designed to exercise methods of supervised classification in low dimensions. The datasets are extracted from the very large (~300 million) Sloan Digital Sky Survey in five photometric bands (ugriz). See York et al. (2000) and \url{http://en.wikipedia.org/wiki/Sloan_Digital_Sky_Survey} for background on the survey.  The SDSS_test sample has four color indices (u-g, g-r, r-i, i-z) for 12884 unclassified point sources from the 5th Data Release.  The SDSS_train sample has the four color indices together with known classes for 9000 well-characterized Sloan point sources:  2000 quasars (Class 1), 5000 main sequence and giant stars (Class 2), and 2000 white dwarfs (Class 3).  The references, extraction and pre-processing necessary to obtain these samples are described in Appendix C.9 of Feigelson & Babu (2012).  Application of statistican and machine learning classification methods area given in Chapter 9.  
}
\usage{SDSS_ptsrc_train}
\format{Table SDSS_ptsrc_train contains 9000 rows and 5 columns with header row.  No missing or censored data are present.}
\source{Feigelson and Babu (2012), York et al. (2000)}
\references{
Feigelson, E. D. and Babu, G. J. (2012) \emph{Modern Statistical Methods for Astronomy with R Applications}, Cambridge UK:Cambridge University Press

York, D. G., Adelman, J., Anderson, J. E., Anderson, S. F., Annis, J., and 140 others (2000) The Sloan Digital Sky Survey: Technical Summary, \emph{Astronomical Journal}, 120, 1579-1587 \url{http://adsabs.harvard.edu/abs/2000AJ....120.1579Y}
}
\keyword{datasets}
