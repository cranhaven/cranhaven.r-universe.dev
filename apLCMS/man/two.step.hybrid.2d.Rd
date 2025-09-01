\name{two.step.hybrid.2d}
\alias{two.step.hybrid.2d}
\title{
Two step hybrid feature detection using 2D peak detection.
}
\description{
A two-stage hybrid feature detection and alignment procedure, for data generated in multiple batches.
}
\usage{
two.step.hybrid.2d(folder, info, min.within.batch.prop.detect=0.4, 
min.within.batch.prop.report=0.5, min.batch.prop=0.5, batch.align.mz.tol=1e-5,
batch.align.chr.tol=50, file.pattern=".cdf", known.table=NA, n.nodes=4, mz.cut = 1e-4,
rt.cut = 50, mz.search.range =  5e-4, rt.search.range = 200,  
intensity.limit.quantile = 0.05, mPower=4, mz.tol=1e-5, align.mz.tol=NA, align.chr.tol=NA,
max.align.mz.diff=0.01, pre.process=FALSE, recover.mz.range=NA, recover.chr.range=NA,
use.observed.range=TRUE, match.tol.ppm=NA, new.feature.min.count=2, recover.min.count=3)
}
\arguments{
  \item{folder}{ The folder where all CDF files to be processed are located. For example "C:/CDF/this_experiment" }
  \item{info}{A table with two columns. The first column is the file names, and the second column is the batch label of each file.}
  \item{min.within.batch.prop.detect}{A feature needs to be present in at least this proportion of the files, for it to be initially detected as a feature for a batch. This parameter replaces the "min.exp" parameter in semi.sup().}
  \item{min.within.batch.prop.report}{A feature needs to be present in at least this proportion of the files, in a proportion of batches controlled by "min.batch.prop", to be included in the final feature table. This parameter replaces the "min.exp" parameter in semi.sup().}
  \item{min.batch.prop}{A feature needs to be present in at least this proportion of the batches, for it to be considered in the entire data.}
  \item{batch.align.mz.tol}{The m/z tolerance in ppm for between-batch alignment.}
  \item{batch.align.chr.tol}{The RT tolerance for between-batch alignment.}
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
  \item{mz.cut}{
 The divided gird width in m/z when calculate the density of each point.  }
  \item{rt.cut}{
 The divided gird width in RT when calculate the density of each point. }
  \item{mz.search.range}{maximum peak width in m/z}
  \item{rt.search.range}{(maximum peak width in RT}
  \item{intensity.limit.quantile}{intensity threshold}
  \item{mPower}{The power parameter for data transformation when fitting the bi-Gaussian or Gaussian mixture model in an EIC.}
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
}
\details{
The function first conducts hybrid feature detection and alignment in each batch separately. Then a between-batch RT correction and feature alignment is conducted. Weak signal recovery is conducted at the single feature table level. 
}
\value{
A list is returned.
  \item{batchwise.results}{A list. Each item in the list is the product of semi.sup() from a single batch.}
  \item{final.ftrs}{Feature table. This is the end product of the function.}
}
\author{ Tianwei Yu < tianwei.yu@emory.edu> }
\seealso{ semi.sup, cdf.to.ftrs, proc.cdf, prof.to.feature, adjust.time, feature.align, recover.weaker }
\keyword{ models }
