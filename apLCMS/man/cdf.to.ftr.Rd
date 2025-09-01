\encoding{latin1}
\name{cdf.to.ftr}
\alias{cdf.to.ftr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert a number of cdf files in the same directory to a feature table }
\description{
  This is a wrapper function, which calls four other functions to convert a number of cdf files to a feature table. All cdf files to be processed must be in a single folder. 
}
\usage{
cdf.to.ftr(folder, output_path, file.pattern=".cdf", n.nodes=4, min.exp=2, 
min.pres=0.5, min.run=12, mz.tol=1e-5, baseline.correct.noise.percentile=0.05, 
shape.model="bi-Gaussian", BIC.factor=2, baseline.correct=0, peak.estim.method="moment",
min.bw=NA, max.bw=NA, sd.cut=c(0.01,500), sigma.ratio.lim=c(0.01, 100), 
component.eliminate=0.01, moment.power=1, subs=NULL, align.mz.tol=NA, align.chr.tol=NA,
max.align.mz.diff=0.01, pre.process=FALSE, recover.mz.range=NA, recover.chr.range=NA,
use.observed.range=TRUE, recover.min.count=3, intensity.weighted=FALSE)
}
\arguments{
  \item{folder}{ The folder where all CDF files to be processed are located. For example ?C:/CDF/this_experiment? }
  \item{output_path}{Path to the output directory}
  \item{file.pattern}{The pattern in the names of the files to be processed. The default is ".cdf". Other formats supported by mzR package can also be used, e.g. "mzML" etc.}
  \item{n.nodes}{ The number of CPU cores to be used through doSNOW. }
  \item{min.exp}{ If a feature is to be included in the final feature table, it must be present in at least this number of spectra. }
  \item{min.pres}{ This is a parameter of thr run filter, to be passed to the function proc.cdf(). Please see the help for proc.cdf() for details. }
  \item{min.run}{ This is a parameter of thr run filter, to be passed to the function proc.cdf(). Please see the help for proc.cdf() for details. }
  \item{subs}{ If not all the CDF files in the folder are to be processed, the user can define a subset using this parameter. For example, subs=15:30, or subs=c(2,4,6,8) }
  \item{mz.tol}{ The user can provide the m/z tolerance level for peak identification. This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. Please see the help for proc.cdf() for details.}
  \item{baseline.correct.noise.percentile}{ The perenctile of signal strength of those EIC that don't pass the run filter, to be used as the baseline threshold of signal strength. This parameter is passed to proc.cdf() }
  \item{shape.model}{ The mathematical model for the shape of a peak. There are two choices - bi-Gaussian and Gaussian. When the peaks are asymmetric, the bi-Gaussian is better. The default is bi-Gaussian.}
  \item{BIC.factor}{the factor that is multiplied on the number of parameters to modify the BIC criterion. If larger than 1, models with more peaks are penalized more.}
  \item{baseline.correct}{This is a parameter in peak detection. After grouping the observations, the highest observation in each group is found. If the highest is lower than this value, the entire group will be deleted. The default value is NA, which allows the program to search for the cutoff level. Please see the help for proc.cdf() for details.}
  \item{peak.estim.method}{the bi-Gaussian peak parameter estimation method, to be passed to subroutine prof.to.features. Two possible values: moment and EM.}
  \item{min.bw}{The minimum bandwidth in the smoother in prof.to.features(). Please see the help file for prof.to.features() for details.}
  \item{max.bw}{The maximum bandwidth in the smoother in prof.to.features(). Please see the help file for prof.to.features() for details.}
  \item{sd.cut}{A parameter for the prof.to.features() function. A vector of two. Features with standard deviation outside the range defined by the two numbers are eliminated.}
  \item{sigma.ratio.lim}{A parameter for the prof.to.features() function. A vector of two. It enforces the belief of the range of the ratio between the left-standard deviation and the righ-standard deviation of the bi-Gaussian fuction used to fit the data.}
  \item{component.eliminate}{In fitting mixture of bi-Gaussian (or Gaussian) model of an EIC, when a component accounts for a proportion of intensities less than this value, the component will be ignored.}
  \item{moment.power}{The power parameter for data transformation when fitting the bi-Gaussian or Gaussian mixture model in an EIC.}
  \item{align.chr.tol}{ The user can provide the elution time tolerance level to override the program?s selection. This value is in the same unit as the elution time, normaly seconds. Please see the help for match.time() for details.}
  \item{align.mz.tol}{ The user can provide the m/z tolerance level for peak alignment to override the program?s selection.  This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.Please see the help for feature.align() for details.}
  \item{max.align.mz.diff}{As the m/z tolerance in alignment is expressed in relative terms (ppm), it may not be suitable when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching in higher m/z range.}
  \item{pre.process}{ Logical. If true, the program will not perform time correction and alignment. It will only generate peak tables for each spectra and save the files. It allows manually dividing the task to multiple machines. }
  \item{recover.mz.range}{A parameter of the recover.weaker() function. The m/z around the feature m/z to search for observations. The default value is NA, in which case 1.5 times the m/z tolerance in the aligned object will be used. }
  \item{recover.chr.range}{A parameter of the recover.weaker() function. The retention time around the feature retention time to search for observations. The default value is NA, in which case 0.5 times the retention time tolerance in the aligned object will be used.}
  \item{use.observed.range}{A parameter of the recover.weaker() function. If the value is TRUE, the actual range of the observed locations of the feature in all the spectra will be used. }
  \item{recover.min.count}{ The minimum time point count for a series of point in the EIC for it to be considered a true feature. }
  \item{intensity.weighted}{Whether to weight the local density by signal intensities in the initial peak detection.}
}
\details{
  The wrapper function calls five other functions to perform the feature table generation. Every spectrum (cdf file) first goes through proc.cdf() and prof.to.feature() to generate a spectrum-level peak table. The eluction time correction is done by match.time(). Then the peaks are aligned across spectra by feature.align(). For features deteced in a portion of the spectra, weaker signals in other spectra are recovered by recover.weaker().
  From version 4, the parameter mz.tol can no longer be NA. This is to allow the program better process data other than FTLCMS. It is recommended that the user use the machine's claimed accuracy. For FTMS, 1e-5 is recommended. 
}
\value{
  A list is returned.
  \item{features}{ A list object, each component of which being the peak table from a single spectrum.}
  \item{features2}{A list object, each component of which being the peak table from a single spectrum, after elution time correction.}
  \item{aligned.ftrs}{ Feature table BEFORE weak signal recovery.}
  \item{final.ftrs}{Feature table after weak signal recovery. This is the end product of the function.}
  \item{pk.times}{ Table of feature elution time BEFORE weak signal recovery.}
  \item{final.times}{Table of feature elution time after weak signal recovery.}
  \item{mz.tol}{The input mz.tol value by the user.}
  \item{align.mz.tol}{The m/z tolerance level in the alignment across spectra, either input from the user or automatically selected when the user input is NA.}
  \item{align.chr.tol}{The retention time tolerance level in the alignment across spectra, either input from the user or automatically selected when the user input is NA.}
}
\author{ Tianwei Yu <tyu8@sph.emory.edu> }
\seealso{ proc.cdf, prof.to.feature, adjust.time, feature.align, recover.weaker }
\keyword{ models }
