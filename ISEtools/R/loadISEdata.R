#' @title Load ISE calibration and experimental data.
#' @description Loads tab-delimited calibration and (if it exists) experimental sample data.  
#' @param filename.calibration The name and location of the tab-delimited calibration file\cr
#' It should have the following structure:\cr
#' First line (header row): ISEID log10x emf\cr
#' Remaining lines (data): ISEID is an identifier for the ISE.  The ISEID variables should be integers, with the lowest value equal to 1, and no gaps.  That is, if there are four ISEs, they must be labeled 1, 2, 3, and 4. log10x is the log10 concentration (mol/l) of the calibration samples. The emf readings (in mV) follow.\cr
#' @param filename.experimental The experimental file (if there is one, otherwise keep the default filename.experimental=NA) should have one of the following structures:\cr\cr

#' basic model: The header row will include ISEID, SampleID, and emf.  ISEID is defined the same way as in the calibration file.  SampleID is an integer indicating which sample is being measured, and must follow the same numbering rules as ISEID. Finally, emf is the mV reading of the experimental samples for each ISE.\cr or\cr

#' standard addition: When using the standard addition model, the experimental file will contain ISEID and SampleID as before.  Two emf values are recorded: emf1 is the mV reading of the sample, and emf2 is the mV reading of the sample plus the addition.  Additionally, V.s is the volume of the sample, V.add is the volume of the addition, and conc.add is the concentration (mol/l) of the addition.  The units of V.s and V.add do not matter as long as they are the same. \cr

#' @details Internally calls 'ISEdata.calibration' if there is no experimental data.
#' @return loadISEdata returns the following values in a list of class ISEdata:\cr
#' Calibration variables:\cr
#' @return \item{N }{Total number of calibration measurements (e.g. for 5 calibration points measured with 3 ISEs, N = 15)}
#' @return \item{R }{Number of ISEs}
#' @return \item{ISEID }{Identifier for the ISE}
#' @return \item{log10x }{log concentration (mol/l) of calibration data}
#' @return \item{emf }{emf (mV) for calibration data}

#' @return Experimental variables:\cr
#' @return \item{M }{Number of experimental samples}
#' @return \item{M.obs }{Total number of experimental measurements. E.g. for 4 samples each measured by 3 ISEs, M.obs = 12. Only returned if R > 1}
#' @return \item{ISEID.exp }{Identifier for the ISE for the experimental data (returned if R >1)}
#' @return \item{x.exp }{Identifier for the experimental (returned if R > 1)}

#' @return Basic format only: \cr
#' @return \item{emf.exp }{emf (mV) for experimental data}

#' @return Standard addition format only:\cr
#' @return \item{delta.emf }{difference between emf1 and emf2 (mV) for experimental data}
#' @return \item{V.s }{Sample volume (any units allowed but must be consistent)}
#' @return \item{V.add }{Volume added to the sample}
#' @return \item{conc.add }{Concentration added.}

#' @return Summary variables of calibration and experimental data:\cr
#' @return \item{calibration.only }{Indicates whether there was only calibration data (TRUE) or calibration and experimental data (FALSE)}
#' @return \item{stdadd }{Indicates whether standard addition was used. Returns NA (calibration data only), FALSE (basic experimental data), or TRUE (standard addition experimental data)}
#' @return \item{data.calib }{The loaded calibration data frame}
#' @return \item{data.exp }{The loaded experimental data frame}

#' @author Peter Dillingham \email{peter.dillingham@@otago.ac.nz}
#' @examples
#' ###
#' # Loading the example tab-delimited text files for the lead data
#' ###
#' 
#' # 1. Find pathnames for the lead example txt files:
#' path.calib = paste(path.package('ISEtools'), "/extdata", 
#' 	"/Lead_calibration.txt", sep="")
#' path.basic = paste(path.package('ISEtools'), "/extdata", 
#' 	"/Lead_experimentalBasic.txt", sep="")
#' path.sa = paste(path.package('ISEtools'), "/extdata", 
#' 	"/Lead_experimentalSA.txt", sep="")
#' # Load the calibration data
#' lead.example1 = loadISEdata(filename.calibration = path.calib)
#' print(lead.example1)
#' 
#' # ... and with experimental data, Basic format
#' lead.example2 = loadISEdata(filename.calibration = path.calib, 
#' 	filename.experimental = path.basic)
#' print(lead.example2)
#' 	
#' # ... and with experimental data, Standard Addition format
#' lead.example3 = loadISEdata(filename.calibration = path.calib, 
#' 	filename.experimental = path.sa)
#' print(lead.example3)
#' 	
#' \dontrun{
#' ###
#' # Example writing data to an external file and loading via loadISEdata
#' ###
#' # Example calibration data
#' ISEID = c(1,1,1,1,1)
#' log10x = c(-7.14, -6.10, -5.10, -4.11, -3.11)
#' emf = c(110.8, 108.6, 100.4, 60.5, 12.7)
#' ISEexample_calibration = cbind(ISEID, log10x, emf)
#' 
#' # Example standard addition experimental data for two samples
#' ISEID = c(1,1)
#' SampleID = c(1,2)
#' emf1 = c(153.09, 110.0)
#' emf2 = c(25.3, 43.1)
#' V.s = c(50, 50)
#' V.add = c(0.3, 0.3)
#' conc.add = c(0.1, 0.1)
#' ISEexample_experimental = cbind(ISEID, SampleID, emf1, emf2, V.s, V.add, conc.add)
#'
#' write.table(ISEexample_calibration, "ISEexample_calibration.txt", sep="\\t")
#' write.table(ISEexample_experimental, "ISEexample_experimental.txt", sep="\\t")
#' 
#' # Load data
#' example4 = loadISEdata(filename.calibration = "ISEexample_calibration.txt", 
#' 						filename.experimental="ISEexample_experimental.txt")
#' print(example4)
#' summary(example4)
#' plot(example4)
#' 
#' # Tidy up and remove the example files
#' file.remove("ISEexample_calibration.txt")
#' file.remove("ISEexample_experimental.txt")
#' }
#' @export
loadISEdata <-
function(filename.calibration, filename.experimental = NA) {
###################################################################
# 				ISEdata::                                 #
#   	load calibration data for one or more ISEs                  #
#   	(optional) load sample data for one or more samples         #
###################################################################


### Loads data for Multiple ISEs from tab delimited files and puts them in an appropriate format.
## One file should contain calibration data, with a header row as follows:
# ISEID: A number (1, 2, 3, ...; must be consecutive) identifying the ISE
# log10x: The known concentration on a log10 scale
# emf: The measured emf
#
## If there are experimental samples, the second file should contain experimental data, with a header row as follows:
# For the basic model:
# 	ISEID: A number (1, 2, 3, ...; must be consecutive) identifying the ISE
# 	SampleID: A number identifying the unknown sample that is being measured
# 	emf: The measured emf
# For the standard addition model:
# 	ISEID: A number (1, 2, 3, ...; must be consecutive) identifying the ISE
# 	SampleID: A number identifying the unknown sample that is being measured
# 	emf1: The measured emf for the sample
#	emf2: The measured emf for the sample plus the standard addition
# 	V.s: volume of the sample (any convenient unit, e.g. ml or l)
# 	V.add: volume of the addition (same units as V.s)
# 	conc.add: concentration of the addition (mol/l)
###

   # calibration.only: a TRUE/FALSE flag indicating whether there is only a calibration file (calibration.only=TRUE)
   #    or whether there is also a file with experimental sample data (calibration.only=F)
   calibration.only = FALSE
   if (is.na(filename.experimental)) { calibration.only = TRUE }

   if (!calibration.only) {
	# Load the experimental and calibration files
	data.exp = read.delim(filename.experimental,
		header = TRUE, sep = "\t", quote="\"", dec=".", fill = TRUE, comment.char="")

	# Check whether the data is in standard addition format or not,
	#    evidenced by the existence of the sample volume variable
	stdadd = exists('V.s', where = data.exp)
	
	data.calib = read.delim(filename.calibration,
		header = TRUE, sep = "\t", quote="\"", dec=".", fill = TRUE, comment.char="")

	# Format data from the calibration file
	N = nrow(data.calib)
	R = max(data.calib$ISEID)
	ISEID = data.calib$ISEID
	log10x = data.calib$log10x
	emf = data.calib$emf

	# Format data from the experimental file
	M = max(data.exp$SampleID)
	M.obs = nrow(data.exp)
	ISEID.exp = data.exp$ISEID
	xID.exp = data.exp$SampleID
	if (stdadd == TRUE) {
		delta.emf = data.exp$emf2 - data.exp$emf1
		V.s = data.exp$V.s
		V.add = data.exp$V.add
		conc.add = data.exp$conc.add
	}
	if (stdadd != TRUE) {
		emf.exp = data.exp$emf
	}

	### If there is only one ISE, format the data accordingly:
	if(R == 1) {
		if (stdadd == TRUE) {
			data.out = list(N=N, R=R, log10x = log10x, emf=emf,
				M=M, delta.emf = delta.emf, 
				V.s = V.s, V.add = V.add, conc.add = conc.add, 
				stdadd = stdadd, calibration.only = calibration.only, 
				data.calib = data.calib, data.exp = data.exp)
		}
		if (stdadd != TRUE) {
			data.out = list(N=N, R=R, log10x = log10x, emf=emf,
				M=M, emf.exp = emf.exp, 
				stdadd = stdadd, calibration.only = calibration.only, 
				data.calib = data.calib, data.exp = data.exp)
		}
	}

	### If multiple ISEs, format as follows:
	if(R > 1) {
		if (stdadd == TRUE) {
			data.out = list(N=N, R=R, ISEID=ISEID, log10x = log10x, emf=emf,
				M=M, M.obs = M.obs, ISEID.exp = ISEID.exp,
				xID.exp=xID.exp, delta.emf = delta.emf, 
				V.s = V.s, V.add = V.add, conc.add = conc.add, 
				stdadd = stdadd, calibration.only = calibration.only, 
				data.calib = data.calib, data.exp = data.exp)
		}
		if (stdadd != TRUE) {
			data.out = list(N=N, R=R, ISEID=ISEID, log10x = log10x, emf=emf,
				M=M, M.obs = M.obs, ISEID.exp = ISEID.exp,
				xID.exp=xID.exp, emf.exp = emf.exp, 
				stdadd = stdadd, calibration.only = calibration.only, 
				data.calib = data.calib, data.exp = data.exp)
		}
	}
   }
   if (calibration.only) {
	data.out = ISEdata.calibration(filename.calibration = filename.calibration, calibration.only=calibration.only)
   }
   class(data.out) = "ISEdata"
   return(data.out)   

}
