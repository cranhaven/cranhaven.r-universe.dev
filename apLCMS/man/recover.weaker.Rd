\name{recover.weaker}
\alias{recover.weaker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Recover weak signals in some profiles that is not identified as a peak, but corresponds to identified peaks in other spectra. }
\description{
  Given the aligned feature table, some features are identified in a subgroup of spectra. This doesn't mean they don't exist in the other spectra. The signal could be too low to pass the run filter. Thus after obtaining the aligned feature table, this function re-analyzes each spectrum to try and fill in the holes in the aligned feature table.
}
\usage{
recover.weaker(filename, loc, aligned.ftrs, pk.times, align.mz.tol,
                 align.chr.tol, this.f1, this.f2, mz.range = NA,
                 chr.range = NA, use.observed.range = TRUE, orig.tol =
                 1e-05, min.bw = NA, max.bw = NA, bandwidth = 0.5,
                 recover.min.count = 3, intensity.weighted=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ the cdf file name from which weaker signal is to be recovered. }
  \item{loc}{the location of the filename in the vector of filenames.}
  \item{aligned.ftrs}{matrix, with columns of m/z values, elution times, signal strengths in each spectrum.}
  \item{pk.times}{matrix, with columns of m/z, median elution time, and elution times in each spectrum.}
  \item{align.mz.tol}{the m/z tolerance used in the alignment.}
  \item{align.chr.tol}{the elution time tolerance in the alignment. }
  \item{this.f1}{The matrix which is the output from proc.to.feature().}
  \item{this.f2}{The matrix which is the output from proc.to.feature(). The retention time in this object have been adjusted by the function adjust.time().}
  \item{orig.tol}{ The mz.tol parameter provided to the proc.cdf() function. This helps retrieve the intermediate file.}
  \item{mz.range}{The m/z around the feature m/z to search for observations. The default value is NA, in which case 1.5 times the m/z tolerance in the aligned object will be used. }
  \item{chr.range}{The retention time around the feature retention time to search for observations. The default value is NA, in which case 0.5 times the retention time tolerance in the aligned object will be used.}
  \item{use.observed.range}{If the value is TRUE, the actual range of the observed locations of the feature in all the spectra will be used. }
  \item{min.bw}{The minimum bandwidth to use in the kernel smoother.See the details section.}
  \item{max.bw}{The maximum bandwidth to use in the kernel smoother.See the details section.}
  \item{bandwidth}{A value between zero and one. Multiplying this value to the length of the signal along the time axis helps determine the bandwidth in the kernel smoother used for peak identification. See the details section.}
  \item{recover.min.count}{minimum number of raw data points to support a recovery.}
  \item{intensity.weighted}{Whether to use intensity to weight mass density estimation.}
}
\details{
  For every feature, if it is not present in a spectrum, open the spectrum, and look around the m/z and elution time location of the feature. The observed intensities with m/z and elution time most consistent with the feature are collected. The peak location and intensity is evaluated. 
  For each spectrum, the partially processed file: .rawprof is loaded. This file is the product of the function proc.cdf(). The m/z values are already grouped and the median taken. The function searches around the feature m/z and retention time. 
  When a series of signals is found at an m/z group, kernel smoother is fit along the time axis to determine whether there is one single peak or multiple peaks. The bandwidth of the kernel smoother is determined as follows: multiply the length of the signals by the bandwidth parameter. If the resulting value is between min.bw and max.bw, use that value; else if the value is below min.bw, use min.bw; else if the value is above max.bw, use max.bw. The default values of min.bw and max.bw are NA, in which case min.bw is set to be 1/30 of the retention time range, and max.bw is set to be 1/15 of the retention time range. A modified EM algorithm which allows missing completely at random at certain time points is used for the evaluation of the peak location and area. If a single peak is detected by the kernel smoother, the maximum likelihood normal curve is fitted. If multiple peaks are detected, the EM algorithm finds the normal mixture that best explain the data.
  After finding the peaks around the target feature, find the closest one to the target feature and record its information in the $aligned.ftrs and $pk.times matrices. 
}
\value{
  Returns a list object with the following objects in it:
  \item{aligned.ftrs}{A matrix, with columns of m/z values, elution times, and signal strengths in each spectrum.}
  \item{pk.times}{A matrix, with columns of m/z, median elution time, and elution times in each spectrum.}
  \item{mz.tol}{The m/z tolerance in the aligned object.}
  \item{chr.tol}{The elution time tolerance in the aligned object.}
}
\author{ Tianwei Yu <tyu8@sph.emory.edu> }

