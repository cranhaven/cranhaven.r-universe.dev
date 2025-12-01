\name{loadISEdata}
\alias{loadISEdata}
\title{Load ISE calibration and experimental data.}
\usage{
loadISEdata(filename.calibration, filename.experimental = NA)
}
\arguments{
\item{filename.calibration}{The name and location of the tab-delimited calibration file\cr
It should have the following structure:\cr
First line (header row): ISEID log10x emf\cr
Remaining lines (data): ISEID is an identifier for the ISE.  The ISEID variables should be integers, with the lowest value equal to 1, and no gaps.  That is, if there are four ISEs, they must be labeled 1, 2, 3, and 4. log10x is the log10 concentration (mol/l) of the calibration samples. The emf readings (in mV) follow.\cr}

\item{filename.experimental}{The experimental file (if there is one, otherwise keep the default filename.experimental=NA) should have one of the following structures:\cr\cr
basic model: The header row will include ISEID, SampleID, and emf.  ISEID is defined the same way as in the calibration file.  SampleID is an integer indicating which sample is being measured, and must follow the same numbering rules as ISEID. Finally, emf is the mV reading of the experimental samples for each ISE.\cr or\cr
standard addition: When using the standard addition model, the experimental file will contain ISEID and SampleID as before.  Two emf values are recorded: emf1 is the mV reading of the sample, and emf2 is the mV reading of the sample plus the addition.  Additionally, V.s is the volume of the sample, V.add is the volume of the addition, and conc.add is the concentration (mol/l) of the addition.  The units of V.s and V.add do not matter as long as they are the same. \cr}
}
\value{
loadISEdata returns the following values in a list of class ISEdata:\cr
Calibration variables:\cr

\item{N }{Total number of calibration measurements (e.g. for 5 calibration points measured with 3 ISEs, N = 15)}

\item{R }{Number of ISEs}

\item{ISEID }{Identifier for the ISE}

\item{log10x }{log concentration (mol/l) of calibration data}

\item{emf }{emf (mV) for calibration data}

Experimental variables:\cr

\item{M }{Number of experimental samples}

\item{M.obs }{Total number of experimental measurements. E.g. for 4 samples each measured by 3 ISEs, M.obs = 12. Only returned if R > 1}

\item{ISEID.exp }{Identifier for the ISE for the experimental data (returned if R >1)}

\item{x.exp }{Identifier for the experimental (returned if R > 1)}

Basic format only: \cr

\item{emf.exp }{emf (mV) for experimental data}

Standard addition format only:\cr

\item{delta.emf }{difference between emf1 and emf2 (mV) for experimental data}

\item{V.s }{Sample volume (any units allowed but must be consistent)}

\item{V.add }{Volume added to the sample}

\item{conc.add }{Concentration added.}

Summary variables of calibration and experimental data:\cr

\item{calibration.only }{Indicates whether there was only calibration data (TRUE) or calibration and experimental data (FALSE)}

\item{stdadd }{Indicates whether standard addition was used. Returns NA (calibration data only), FALSE (basic experimental data), or TRUE (standard addition experimental data)}

\item{data.calib }{The loaded calibration data frame}

\item{data.exp }{The loaded experimental data frame}
}
\description{
Loads tab-delimited calibration and (if it exists) experimental sample data.
}
\details{
Internally calls 'ISEdata.calibration' if there is no experimental data.
}
\examples{
###
# Loading the example tab-delimited text files for the lead data
###

# 1. Find pathnames for the lead example txt files:
path.calib = paste(path.package('ISEtools'), "/extdata", 
	"/Lead_calibration.txt", sep="")
path.basic = paste(path.package('ISEtools'), "/extdata", 
	"/Lead_experimentalBasic.txt", sep="")
path.sa = paste(path.package('ISEtools'), "/extdata", 
	"/Lead_experimentalSA.txt", sep="")
# Load the calibration data
lead.example1 = loadISEdata(filename.calibration = path.calib)
print(lead.example1)

# ... and with experimental data, Basic format
lead.example2 = loadISEdata(filename.calibration = path.calib, 
	filename.experimental = path.basic)
print(lead.example2)
	
# ... and with experimental data, Standard Addition format
lead.example3 = loadISEdata(filename.calibration = path.calib, 
	filename.experimental = path.sa)
print(lead.example3)
}
\author{
Peter Dillingham \email{peter.dillingham@otago.ac.nz}
}
