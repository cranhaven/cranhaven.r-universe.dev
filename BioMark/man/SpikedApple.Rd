\name{SpikedApple}
\alias{SpikedApple}
\alias{SpikeNeg}
\alias{SpikePos}
\alias{neg.markers}
\alias{pos.markers}
\docType{data}
\title{Spike-in metabolomics data for apple extracts}
\description{
Data from a spike-in experiment for apple extracts. Twenty apple
extracts are divided in two groups, one control, and one spike-in
group. The control group is measured without any spiking - the spike-in
group is spiked with nine chemical compounds in three different
combinations of concentrations. The data provide the experimental data
of the forty apple extracts in lists \code{SpikePos} and \code{SpikeNeg}
for positive and negative ionization, respectively, and in two separate
data.frames (\code{pos.markers} and \code{neg.markers}) contains
information of the features of the standards, i.e., the spike-in compounds.  
}
\usage{
data(SpikePos)
data(SpikeNeg)
}
\format{\code{SpikePos} and \code{SpikeNeg} are lists with three
  elements:
  \describe{
    \item{data}{Data matrix, describing for each of the forty injections
    the intensity of the features (columns). Column names consist of a
    combination of retention time (in seconds) and m/z values, and are
    sorted on retention time.}
    \item{classes}{Class labels for the forty injections (control, or
      group1, 2 or 3).}
    \item{annotation}{Matrix, containing for each of the features XCMS
      and CAMERA information, such as mz, rt, number of times a feature
      is identified in the control or spike-in samples, possible isotope
      or adduct annotation, and whether or not the feature is identified
      in the standards (the spike-in data).}
  }

  In addition, \code{pos.markers} and \code{neg.markers} contain the
  information of the standards, i.e. the compounds that are spiked
  in. These data.frames describe in their rows single features
  identified with XCMS and CAMERA, using the same settings as the 
  experimental apple data, and have the following columns:
  \describe{
    \item{comp}{The (short) name of the spiked-in compound giving rise
      to this particular feature.}
    \item{mz, rt, isotope, adduct}{Feature information, similar to the
      information in the \code{annotation} fields in \code{SpikePos} and
      \code{SpikeNeg}.}
    \item{feature.nr}{The number of the corresponding feature in either
      \code{SpikePos} or \code{SpikeNeg}.}
    \item{group1, group2, group3}{Approximate spiking levels for the
      three groups. A value of 1.0 corresponds to an increase that is
      roughly equal to the naturally occuring concentration in
      apple. Exceptions are trans-resveratrol and
      cyanidin-3-galactoside, both not naturally occuring. These two
      compounds have been spiked in at one constant level which gives
      features of comparable size.}
    }
}
\details{
  This is the complete data set, from which \code{spikedApples} is a
  subset, basically presenting the control and group1 information with
  hand-picked spike-in features. The data in SpikePos and SpikeNeg use
  CAMERA grouping to automatically determine which features are
  corresponding to which spike-in compounds. Raw data in CDF format are
  available from the MetaboLights repository.
}
\seealso{spikedApples}
\author{Pietro Franceschi}
\source{
  http://www.ebi.ac.uk/metabolights/MTBLS59
  
  P. Franceschi, D. Masuero, U. Vrhovsek, F. Mattivi and R. Wehrens: A
  benchmark spike-in data set for biomarker identification in
  metabolomics. J. Chemom. 26, 16-24 (2012).
}
\examples{
data(SpikePos)
plot(SpikePos$annotation[,c("rt", "mz")],
     xlab = "Time (s)", ylab = "m/z",
     main = "Positive ionization mode")
points(pos.markers[!is.na(pos.markers$feature.nr), c("rt", "mz")],
       pch = 19, col = 2)

data(SpikeNeg)
plot(SpikeNeg$annotation[,c("rt", "mz")],
     xlab = "Time (s)", ylab = "m/z",
     main = "Negative ionization mode")
points(neg.markers[!is.na(neg.markers$feature.nr), c("rt", "mz")],
       pch = 19, col = 2)
}
\keyword{datasets}
