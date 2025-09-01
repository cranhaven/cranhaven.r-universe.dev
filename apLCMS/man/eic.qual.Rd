\name{eic.qual}
\alias{eic.qual}
\title{
 Internal function: Calculate the single predictor quality. 
}
\description{
For each column of an EIC data feature matrix, find its predictive power on the m/z match to known metabolites. 
}
\usage{
eic.qual(eic.rec, known.mz, mass.matched = NA, match.tol.ppm = 5, do.plot = FALSE,
pos.confidence = 0.99, neg.confidence = 0.99)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eic.rec}{ The EIC data feature matrix. Each row is an EIC. Each column is a data feature. }
  \item{known.mz}{ The known m/z values to be matched to. }
  \item{mass.matched}{ A vector of indicators of whether the m/z of each EIC is matched to the known m/z values. The default is NA, in which case it is calculated within the function. }
  \item{match.tol.ppm}{ The tolerance level of m/z match. }
  \item{do.plot}{ Whether to produce plots of the ROCS. }
  \item{pos.confidence}{The confidence level for the features matched to the known feature list. }
  \item{neg.confidence}{The confidence level for the features not matching to the known feature list. }
}
\value{
A matrix of four columns. The first two columns are the VUS and AUC without uncertainty. The next two columns are the VUS and AUC with uncertainty. 
}
\references{Bioinformatics. 30(20):  2941-2948.}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }