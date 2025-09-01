\name{learn.cdf}
\alias{learn.cdf}
\title{ Peak detection using the machine learning approach. }
\description{ The procedure uses information of known metabolites, and constructs prediction models to differentiate EICs. }
\usage{
learn.cdf(filename, output_path, tol = 2e-05, min.run = 4, min.pres = 0.3, 
baseline.correct = 0, ridge.smoother.window = 50, smoother.window = c(1, 5, 10), 
known.mz, match.tol.ppm = 5, do.plot = FALSE, pos.confidence = 0.99, 
neg.confidence = 0.99, max.ftrs.to.use = 10, do.grp.reduce = TRUE, 
remove.bottom.ftrs = 0, max.fpr = seq(0, 0.6, by = 0.1), min.tpr = seq(0.8, 1, by = 0.1),
intensity.weighted=FALSE)
}
\arguments{
  \item{filename}{ The cdf file name. If the file is not in the working directory, the path needs to be given. }
  \item{output_path}{Path to the output directory}
  \item{min.pres}{ Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped by m/z to be considered a peak.}
  \item{min.run}{ Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be considered a peak. }
  \item{tol}{ m/z tolerance level for the grouping of data points. This value is expressed as the fraction of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. The recommended value is the machine's nominal accuracy level. Divide the ppm value by 1e6. For FTMS, 1e-5 is recommended. }
  \item{baseline.correct}{After grouping the observations, the highest intensity in each group is found. If the highest is lower than this value, the entire group will be deleted. The default value is NA, in which case the program uses the 75th percentile of the height of the noise groups. }
  \item{ridge.smoother.window}{ The size of the smoother window used by the kernel smoother to remove long ridge noise from each EIC.  }
  \item{smoother.window}{The smoother windows to use in data feature generation.}
  \item{known.mz}{ The m/z values of the known metabolites. }
  \item{match.tol.ppm}{ The ppm tolerance to match identified features to known metabolites/features. }
  \item{do.plot}{ Whether to produce diagnostic plots. }
  \item{pos.confidence}{The confidence level for the features matched to the known feature list. }
  \item{neg.confidence}{The confidence level for the features not matching to the known feature list. }
  \item{max.ftrs.to.use}{The maximum number of data features to use in a predictive model. }
  \item{do.grp.reduce}{Whether to reduce data features that are similar. It is based on data feature predictability. }
  \item{remove.bottom.ftrs}{The number of worst performing data features to remove before model building. }
  \item{max.fpr}{The proportion of unmatched features to be selected in the feature detection step. }
  \item{min.tpr}{The proportion of matched features to be selected in the feature detection step. }
  \item{intensity.weighted}{Whether to weight the local density by signal intensities.}
}
\details{
  The subroutine takes CDF, mxml etc LC/MS profile.   
  First the profile is sliced into EICs using adaptive binning. Then data features are extracted from each EIC. The EICs are classified into two groups: those that have m/z values that match to known m/z values, and those that don't. Classification models are built to separate the two classes, and each EIC is given a score by the classification model. Those with better scores are selected to enter the feature quantification step. 
}
\value{
A matrix with four columns: m/z value, retention time, intensity, and group number. 
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }
