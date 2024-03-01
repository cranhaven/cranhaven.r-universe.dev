\name{bioinfer.new.JGR}
\alias{bioinfer.new.JGR}
\title{Dialog box for predicting env. conditions from biological observations}
\description{
  This module estimates environmental conditions from observations of
  benthic macroinvertebrates for a set of sites, by combining
  observations of each particular taxon with a taxon-environment
  relationship.

  This calculation has three steps.  First, observations
  of benthic macroinvertebrates are matched with standard taxonomic
  names, with a full taxonomic hierarchy.  Second, operational taxonomic
  units (OTUs) are defined for the data set, so that each individual
  taxon is counted at a consistent taxonomic level.  Third, observations
  are matched with existing taxon-environment relationships and an
  estimate of environmental conditions computed. 

}

\details{
  	
	\strong{User Interface}

	Select \emph{Analysis Tools -> Predicting env. conditions from
	  biological observations} from the menus.

	The Data Directory can be specified to indicate the default
	location that you wish to search for input files and write
	output files.  Unless the browse buttons for individual files
	are used to specify other directories, all input and output
	comes from this directory. 
Note: Changing the data directory does not change the working directory
for R, only for the default locations of the input/output files for this
module.

There are three options for starting points for running this module.  By
default, the entire process should be run.  However, since the first two
steps of the process may require some feedback, these steps can be
skipped in subsequent runs to reduce user effort.

\bold{Run Entire Process:} This option is the default setting.  It
begins with the benthic macroinvertebrate count file and runs all steps
required for: merging taxonomy with a standard taxonomic hierarchy from
the Integrated Taxonomic Information System (ITIS), generating OTUs
corresponding to the coefficient file, and computing maximum likelihood
estimates of environmental conditions.  Output files include an merged
taxonomic file, an OTU file, and a maximum likelihood file.

\bold{Start with Merge Taxonomic Merge File} This option assumes that
the tool has been run previously to merge taxonomy of the benthic
macroinvertebrate count file with a standard taxonomic hierarchy.  The
benthic macroinvertebrate file is not necessary for input, as the merged
taxonomic file is read as input for the starting point.  Output files
include an OTU file and a maximum likelihood file.  This option is
recommended only for experienced users.

\bold{Start with Operational Taxonomy File} This option assumes that the
tool has been run previously to merge taxonomy of the benthic
macroinvertebrate count file with a standard taxonomic hierarchy and to
generate OTUs.  The benthic macroinvertebrate file and the taxonomic
merge file are not necessary for input, as the OTU file is read as input
for the starting point.  Only one output file is generated: the maximum
likelihood file.  This option is recommended only for experienced
users.

\strong{Input files}

\bold{Coefficients file:}  This file is an R data file (e.g., .rda or
.rdata) that contains the taxon-environment relationships.  At the
present time only one coefficient file is available: coef.west.wt.rda,
which provides relationships for stream temperature and percent
substrate sand/fines for macroinvertebrates of western U.S. streams.
Select this file from the default directory that appears when the Browse
button is selected.

\bold{Benthic Count File:} This file contains the observations of
benthic macroinvertebrate.  The file should be supplied as tab-delimited
text.  Three columns in the following order are required: (1) Sample
identifier, (2) taxon name, and (3) taxon abundance.  

\bold{Taxonomic Merge File:} If Run Entire Process is chosen, this file
is the result of the merge between local macroinvertebrate observations
and a standard taxonomic hierarchy.  If \bold{Start with Merge Taxonomic Merge
File} is chosen, this file is the starting input file. The format of the
file is tab-delimited text.

Operational Taxonomy File:  If \bold{Start with Operational Taxonomy
  File} is chosen, it is the starting input file.  See above.

\bold{Output file}

\bold{Maximum likelihood File:} This file is the saved output of this module.
It is a tab-delimited text file, with at least three columns: 1) Site
name; 2) Maximum likelihood estimate of environmental conditions; 3) An
indicator of whether or not multiple maximum likelihood estimates were
found. \bold{TRUE} here indicates that global convergence was not
attained.  The number of columns may increase, depending on how many
environmental parameters are estimated simultaneously.
}

\seealso{
  \code{\link[bio.infer]{get.taxonomic}},
  \code{\link[bio.infer]{get.otu}},
  \code{\link[bio.infer]{mlsolve}},
  \code{\link[bio.infer]{coef.west.wt}}
  
  }

\keyword{regression} 

