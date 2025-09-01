\name{make.known.table}
\alias{make.known.table}
\title{
Producing a table of known features based on a table of metabolites and a table of allowable adducts.
}
\description{
Given a table of known metabolites with original mass and charge information, and a table of allowable adducts, this function outputs a new table of potential features.
}
\usage{
make.known.table(metabolite.table, adduct.table, ion.mode = "+")
}
\arguments{
  \item{metabolite.table}{A table of known metabolites. See the description of the object "metabolite.table" for details.}
  \item{adduct.table}{A table of allowable adducts. See the description of the object "adduct.table" for details.}
  \item{ion.mode}{Character. Either "+" or "-".}
}
\details{
For each allowable ion form, the function produces the m/z of every metabolite given to it. The output table follows the format that is required by the function semi.sup(), so that the user can directly use the table for semi supervised feature detection.}
\value{
A data frame containing the known metabolite ions. It contains 18 columns: 
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
\references{
Yu T, Park Y, Li S, Jones DP (2013) Hybrid feature detection and information accumulation using high-resolution LC-MS metabolomics data. J. Proteome Res. 12(3):1419-27.
}
\author{Tianwei Yu <tyu8@emory.edu>}
\seealso{metabolite.table, adduct.table, semi.sup}
\examples{
data(metabolite.table)
data(adduct.table)
known.table.example<-make.known.table(metabolite.table[1001:1020,], adduct.table[1:4,])
}
\keyword{ models }
