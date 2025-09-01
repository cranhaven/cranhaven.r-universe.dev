\name{proc.txt}
\alias{proc.txt}
\title{ Filter noise and detect peaks from LC/MS data in text format }
\description{
  This function applies the run filter to remove noise. Data points are grouped into EICs in this step.  
}
\usage{
proc.txt(filename, output_path, min.pres=0.5, min.run=12,tol=NA, find.tol.maxd=1e-4, 
baseline.correct.noise.percentile=0.25, baseline.correct=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ The text file name. If the file is not in the working directory, the path needs to be given. }
  \item{output_path}{Path to the output directory}
  \item{min.pres}{ Run filter parameter. The minimum proportion of presence in the time period for a series of signals grouped by m/z to be considered a peak.}
  \item{min.run}{ Run filter parameter. The minimum length of elution time for a series of signals grouped by m/z to be considered a peak. }
  \item{tol}{ m/z tolerance level for the grouping of data points. This value is expressed as the fraction of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. The recommended value is the machine's nominal accuracy level. Divide the ppm value by 1e6. For FTMS, 1e-5 is recommended. }
  \item{find.tol.maxd}{maximum distance between datapoints that are allowed in the procedure to find tolerance.}
  \item{baseline.correct}{After grouping the observations, the highest intensity in each group is found. If the highest is lower than this value, the entire group will be deleted. The default value is NA, in which case the program uses the 75th percentile of the height of the noise groups. }
  \item{baseline.correct.noise.percentile}{ The perenctile of signal strength of those EIC that don't pass the run filter, to be used as the baseline threshold of signal strength. }
}
\details{
  The columns in the text file need to be separated by tab. The first column needs to be the retention time, the second column the m/z values, and the third column the intensity values. The first row needs to be the column labels, rather than values. 
  The m/z are grouped based on the tolerance level using multi-stage smoothing and peak finding. Non-parametric density estimation is used in both m/z dimension and elution time dimension to fine-tune the signal grouping. A run filter is applied, which requires a "true peak" to have a minimum length in the retention time dimension (parameter: min.run), as well as being detected at or higher than a proportion of the time points within the time period (parameter: min.pres). }
\value{
A matrix with four columns: m/z value, retention time, intensity, and group number. 
}
\author{ Tianwei Yu <tyu8@emory.edu> }
\keyword{ models }
