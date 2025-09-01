\name{target.search}
\alias{target.search}
\title{
Targeted search of metabolites with given m/z and (optional) retention time
}
\description{
The function conducts targeted search only. The search is based on m/z and (optionally) retention time. If there are sufficient number of peaks (>=100) in each profile, the function will conduct retention time correction and peak alignment, in order to reduce potential redundancies. 
}
\usage{
target.search(folder, output_path, file.pattern = ".cdf", known.table = NA, n.nodes = 4,
  min.exp = 2, min.bw = NA, max.bw = NA, subs = NULL, align.mz.tol = 2e-05,
  align.chr.tol = 150, max.align.mz.diff = 0.01, recover.mz.range = NA,
  recover.chr.range = NA, use.observed.range = TRUE, match.tol.ppm = 5,
  new.feature.min.count = 2, recover.min.count = 3)
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
  \item{min.bw}{The minimum bandwidth in the smoother in prof.to.features(). Please see the help file for prof.to.features() for details.}
  \item{max.bw}{The maximum bandwidth in the smoother in prof.to.features(). Please see the help file for prof.to.features() for details.}
  \item{subs}{ If not all the CDF files in the folder are to be processed, the user can define a subset using this parameter. For example, subs=15:30, or subs=c(2,4,6,8) }
  \item{align.chr.tol}{ The user can provide the elution time tolerance level to override the program's selection. This value is in the same unit as the elution time, normaly seconds. Please see the help for match.time() for details.}
  \item{align.mz.tol}{ The user can provide the m/z tolerance level for peak alignment to override the program's selection.  This value is expressed as the percentage of the m/z value. This value, multiplied by the m/z value, becomes the cutoff level.Please see the help for feature.align() for details.}
    \item{max.align.mz.diff}{As the m/z tolerance in alignment is expressed in relative terms (ppm), it may not be suitable when the m/z range is wide. This parameter limits the tolerance in absolute terms. It mostly influences feature matching in higher m/z range.}
  \item{recover.mz.range}{A parameter of the recover.weaker() function. The m/z around the feature m/z to search for observations. The default value is NA, in which case 1.5 times the m/z tolerance in the aligned object will be used. }
  \item{recover.chr.range}{A parameter of the recover.weaker() function. The retention time around the feature retention time to search for observations. The default value is NA, in which case 0.5 times the retention time tolerance in the aligned object will be used.}
  \item{use.observed.range}{A parameter of the recover.weaker() function. If the value is TRUE, the actual range of the observed locations of the feature in all the spectra will be used. }
  \item{match.tol.ppm}{ The ppm tolerance to match identified features to known metabolites/features. }
  \item{new.feature.min.count}{ The number of profiles a new feature must be present for it to be added to the database. }
  \item{recover.min.count}{ The minimum time point count for a series of point in the EIC for it to be considered a true feature. }
}
\value{
 \item{features}{ A list object, each component of which being the peak table from a single spectrum.}
 \item{filled.ftrs}{The target features are filled one by one. Notice this table may contain duplicates if some target features are too close. }
 \item{reduced.ftrs}{If the number of target features are big enough (>=100 detected in each profile), retention time correction and peak alignments are conducted to generate this feature table without redundancy.}
 \item{filled.times}{The target features are filled one by one. This is the retention time table. Notice this table may contain duplicates if some target features are too close. }
  \item{reduced.times}{If the number of target features are big enough (>=100 detected in each profile), retention time correction and peak alignments are conducted to generate this feature table without redundancy. This is the retention time table of the aligned features. }
 }
\author{ Tianwei Yu < tianwei.yu@emory.edu> }
\seealso{ cdf.to.ftrs, proc.cdf, prof.to.feature, adjust.time, feature.align, recover.weaker }
\keyword{ models }
