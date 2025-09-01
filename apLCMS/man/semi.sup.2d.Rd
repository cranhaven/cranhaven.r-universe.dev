\name{semi.sup.2d}
\alias{semi.sup.2d}
\title{Semi-supervised feature detection using 2D peak detection}
\description{
The semi-supervised procedure utilizes a database of known metabolites and previously detected features to identify features in a new dataset. It is recommended ONLY for experienced users. The user may need to construct the known feature database that strictly follows the format described below. 
}
\usage{
semi.sup.2d(folder, output_path, file.pattern=".cdf", known.table=NA, n.nodes=4, 
min.exp=2, mz.cut = 5e-5, rt.cut = 50,mz.search.range =  2e-4, rt.search.range = 200,  
intensity.limit.quantile = 0.05, mPower=4, mz.tol=1e-5, subs=NULL, align.mz.tol=NA,
align.chr.tol=NA, max.align.mz.diff=0.01, pre.process=FALSE, recover.mz.range=NA,
recover.chr.range=NA, use.observed.range=TRUE, match.tol.ppm=NA, new.feature.min.count=2,
recover.min.count=3, intensity.weighted=FALSE)
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
  \item{mz.cut}{
 The divided gird width in m/z when calculate the density of each point.  }
  \item{rt.cut}{
 The divided gird width in RT when calculate the density of each point. }
  \item{mz.search.range}{maximum peak width in m/z}
  \item{rt.search.range}{(maximum peak width in RT}
  \item{intensity.limit.quantile}{intensity threshold}
  \item{mPower}{The power parameter for data transformation when fitting the bi-Gaussian or Gaussian mixture model in an EIC.}
  \item{subs}{ If not all the CDF files in the folder are to be processed, the user can define a subset using this parameter. For example, subs=15:30, or subs=c(2,4,6,8) }
  \item{mz.tol}{ The user can provide the m/z tolerance level for peak identification. This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level. Please see the help for proc.cdf() for details.}
  \item{align.mz.tol}{ The user can provide the m/z tolerance level for peak alignment to override the program's selection.  This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.Please see the help for feature.align() for details.}
  \item{align.chr.tol}{ The user can provide the elution time tolerance level to override the program's selection. This value is in the same unit as the elution time, normaly seconds. Please see the help for match.time() for details.}
  \item{max.align.mz.diff}{As the m/z tolerance in alignment is expressed in relative terms (ppm), it may not be suitable when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching in higher m/z range.}
  \item{pre.process}{ Logical. If true, the program will not perform time correction and alignment. It will only generate peak tables for each spectra and save the files. It allows manually dividing the task to multiple machines. }
  \item{recover.mz.range}{A parameter of the recover.weaker() function. The m/z around the feature m/z to search for observations. The default value is NA, in which case 1.5 times the m/z tolerance in the aligned object will be used. }
  \item{recover.chr.range}{A parameter of the recover.weaker() function. The retention time around the feature retention time to search for observations. The default value is NA, in which case 0.5 times the retention time tolerance in the aligned object will be used.}
  \item{use.observed.range}{A parameter of the recover.weaker() function. If the value is TRUE, the actual range of the observed locations of the feature in all the spectra will be used. }
  \item{match.tol.ppm}{ The ppm tolerance to match identified features to known metabolites/features. }
  \item{new.feature.min.count}{ The number of profiles a new feature must be present for it to be added to the database. }
  \item{recover.min.count}{ The minimum time point count for a series of point in the EIC for it to be considered a true feature. }
  \item{intensity.weighted}{Whether to weight the local density by signal intensities.}
}
\details{
The function first conducts a unsupervised feature detection in the new dataset. It then matches the newly identified features to the database. Then merging unfound features in the database and the newly found features, a weak signal recovery is performed. The final feature table is used to update the database. 
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
  \item{intensity.weighted}{Whether to weight the local density by signal intensities in the initial peak detection stage.}
}
\author{ Tianwei Yu < tianwei.yu@emory.edu> }
\seealso{ cdf.to.ftrs, proc.cdf, prof.to.feature, adjust.time, feature.align, recover.weaker }
\keyword{ models }
