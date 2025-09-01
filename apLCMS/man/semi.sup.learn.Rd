\name{semi.sup.learn}
\alias{semi.sup.learn}
\title{Semi-supervised feature detection using machine learning approach. }
\description{
The semi-supervised procedure utilizes a database of known metabolites and previously detected features to identify features in a new dataset. It is recommended ONLY for experienced users. The user may need to construct the known feature database that strictly follows the format described below. 
}
\usage{
semi.sup.learn(folder, output_path, file.pattern=".cdf", known.table=NA, n.nodes=4,
min.exp=2, min.pres=0.3, min.run=4, mz.tol=1e-5, shape.model="bi-Gaussian", 
baseline.correct=0, peak.estim.method="moment", min.bw=NA, max.bw=NA, sd.cut=c(0.01,500), 
component.eliminate=0.01, moment.power=1, sigma.ratio.lim=c(0.01, 100), subs=NULL,
align.mz.tol=NA, align.chr.tol=NA, max.align.mz.diff=0.01, pre.process=FALSE, 
recover.mz.range=NA, recover.chr.range=NA, use.observed.range=TRUE, match.tol.ppm=5,
new.feature.min.count=2, recover.min.count=3, use.learn=TRUE, ridge.smoother.window=50,
smoother.window=c(1, 5, 10),pos.confidence=0.99, neg.confidence=0.99, max.ftrs.to.use=10,
do.grp.reduce=TRUE, remove.bottom.ftrs=0, max.fpr=0.5, min.tpr=0.9, 
intensity.weighted=FALSE)
}
\arguments{
  \item{folder}{ The folder where all CDF files to be processed are located. For example "C:/CDF/this_experiment" }
  \item{output_path}{Path to the output directory}
  \item{file.pattern}{The pattern in the names of the files to be processed. The default is ".cdf". Other formats supported by mzR package can also be used, e.g. "mzML" etc.}
  \item{known.table}{ A data frame containing the known metabolite ions and previously found features. It contains 18 columns: 
	"chemical_formula": the chemical formula if knonw; 
	"HMDB_ID": HMDB ID if known; 
	"KEGG_compound_ID": KEGG compound ID if known; 
	"neutral.mass": the neutral mass if known: 
	"ion.type": the ion form, such as H+, Na+, ..., if known; 
	"m.z": m/z value, either theoretical for known metabolites, or mean observed value for unknown but previously found features; 
	"Number_profiles_processed": the total number of LC/MS profiles that were used to build this database;
	"Percent_found": in what percentage was this feature found historically amount all data processed in building this database;
	"mz_min": the minimum m/z value observed for this feature;
	"mz_max": the maximum m/z value observed for this feature;
	"RT_mean": the mean retention time observed for this feature;
	"RT_sd": the standard deviation of retention time observed for this feature;
	"RT_min": the minimum retention time observed for this feature;
	"RT_max": the maximum retention time observed for this feature;
	"int_mean.log.": the mean log intensity observed for this feature; 
	"int_sd.log.": the standard deviation of log intensity observed for this feature;
	"int_min.log.": the minimum log intensity observed for this feature;
	"int_max.log.": the maximum log intensity observed for this feature;
  }
  \item{n.nodes}{ The number of CPU cores to be used through doSNOW. }
  \item{min.exp}{ If a feature is to be included in the final feature table, it must be present in at least this number of spectra. }
  \item{min.pres}{ This is a parameter of thr run filter, to be passed to the function proc.cdf(). Please see the help for proc.cdf() for details. }
  \item{min.run}{ This is a parameter of thr run filter, to be passed to the function proc.cdf(). Please see the help for proc.cdf() for details. }
  \item{mz.tol}{ The user can provide the m/z tolerance level for peak identification. This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. Please see the help for proc.cdf() for details.}
  \item{shape.model}{ The mathematical model for the shape of a peak. There are two choices - "bi-Gaussian" and "Gaussian". When the peaks are asymmetric, the bi-Gaussian is better. The default is "bi-Gaussian".}
  \item{baseline.correct}{This is a parameter in peak detection. After grouping the observations, the highest observation in each group is found. If the highest is lower than this value, the entire group will be deleted. The default value is NA, which allows the program to search for the cutoff level. Please see the help for proc.cdf() for details.}
  \item{peak.estim.method}{the bi-Gaussian peak parameter estimation method, to be passed to subroutine prof.to.features. Two possible values: moment and EM.}
  \item{min.bw}{The minimum bandwidth in the smoother in prof.to.features(). Please see the help file for prof.to.features() for details.}
  \item{max.bw}{The maximum bandwidth in the smoother in prof.to.features(). Please see the help file for prof.to.features() for details.}
  \item{sd.cut}{A parameter for the prof.to.features() function. A vector of two. Features with standard deviation outside the range defined by the two numbers are eliminated.}
  \item{sigma.ratio.lim}{A parameter for the prof.to.features() function. A vector of two. It enforces the belief of the range of the ratio between the left-standard deviation and the righ-standard deviation of the bi-Gaussian fuction used to fit the data.}
  \item{subs}{ If not all the CDF files in the folder are to be processed, the user can define a subset using this parameter. For example, subs=15:30, or subs=c(2,4,6,8) }
  \item{component.eliminate}{In fitting mixture of bi-Gaussian (or Gaussian) model of an EIC, when a component accounts for a proportion of intensities less than this value, the component will be ignored.}
  \item{moment.power}{The power parameter for data transformation when fitting the bi-Gaussian or Gaussian mixture model in an EIC.}
  \item{align.chr.tol}{ The user can provide the elution time tolerance level to override the program's selection. This value is in the same unit as the elution time, normaly seconds. Please see the help for match.time() for details.}
  \item{align.mz.tol}{ The user can provide the m/z tolerance level for peak alignment to override the program's selection.  This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.Please see the help for feature.align() for details.}
    \item{max.align.mz.diff}{As the m/z tolerance in alignment is expressed in relative terms (ppm), it may not be suitable when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching in higher m/z range.}
  \item{pre.process}{ Logical. If true, the program will not perform time correction and alignment. It will only generate peak tables for each spectra and save the files. It allows manually dividing the task to multiple machines. }
  \item{recover.mz.range}{A parameter of the recover.weaker() function. The m/z around the feature m/z to search for observations. The default value is NA, in which case 1.5 times the m/z tolerance in the aligned object will be used. }
  \item{recover.chr.range}{A parameter of the recover.weaker() function. The retention time around the feature retention time to search for observations. The default value is NA, in which case 0.5 times the retention time tolerance in the aligned object will be used.}
  \item{use.observed.range}{A parameter of the recover.weaker() function. If the value is TRUE, the actual range of the observed locations of the feature in all the spectra will be used. }
  \item{match.tol.ppm}{ The ppm tolerance to match identified features to known metabolites/features. }
  \item{new.feature.min.count}{ The number of profiles a new feature must be present for it to be added to the database. }
  \item{recover.min.count}{ The minimum time point count for a series of point in the EIC for it to be considered a true feature. }
  \item{use.learn}{whether to use machine learning approach. The default is TRUE. }
  \item{ridge.smoother.window}{The size of the smoother window used by the kernel smoother to remove long ridge noise from each EIC. }
  \item{smoother.window}{The smoother windows to use in data feature generation. }
  \item{pos.confidence}{The confidence level for the features matched to the known feature list. }
  \item{neg.confidence}{The confidence level for the features not matching to the known feature list. }
  \item{max.ftrs.to.use}{The maximum number of data features to use in a predictive model. }
  \item{do.grp.reduce}{Whether to reduce data features that are similar. It is based on data feature predictability. }
  \item{remove.bottom.ftrs}{The number of worst performing data features to remove before model building. }
  \item{max.fpr}{The proportion of unmatched features to be selected in the feature detection step. }
  \item{min.tpr}{The proportion of matched features to be selected in the feature detection step. }
  \item{intensity.weighted}{Whether to weight the local density by signal intensities in the initial peak detection stage.}
}
\details{
The function first conducts a machine-learning feature detection in the new dataset. And the conducts the regular feature alignment, retention time adjustment and weak signal recovery.  
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
  \item{updated.known.table}{ The known table updated using the newly processed data. It should be used for future datasets generated using the same machine and LC column. }
  \item{ftrs.known.table.pairing}{ The paring information between the feature table of the current dataset and the known feature tabel. }
}
\author{ Tianwei Yu < tianwei.yu@emory.edu> }
\seealso{ cdf.to.ftrs, semi.sup, learn.cdf, prof.to.feature, adjust.time, feature.align, recover.weaker }
\keyword{ models }
