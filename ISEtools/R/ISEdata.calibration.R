ISEdata.calibration <-
function(filename.calibration, calibration.only) {
##
# A sub-function called by loadISEdata for when there is only calibration data (i.e. the goal is to characterise an ISE, not estimate
#   activity for unknowns
##
	# Load the calibration files
	data.calib = read.delim(filename.calibration,
		header = TRUE, sep = "\t", quote="\"", dec=".", fill = TRUE, comment.char="")

	# Format data from the calibration file
	N = nrow(data.calib)
	R = max(data.calib$ISEID)
	ISEID = data.calib$ISEID
	log10x = data.calib$log10x
	emf = data.calib$emf
	
	if (R == 1) { data.out = list(N=N, R=R, log10x = log10x, emf=emf, 
		stdadd = NA, calibration.only = calibration.only, data.calib = data.calib, data.exp = NA) }
	if (R > 1) { data.out = list(N=N, R=R, ISEID=ISEID, log10x = log10x, emf=emf, 
		stdadd = NA, calibration.only = calibration.only, data.calib = data.calib, data.exp = NA) }

	return(data.out)
}
