print.DS_GF_micro <-
function(x, ...){
	#cat(paste0("Posterior summary for y = ",x$study[1],", n = ", x$study[2],":\n"))
	cat(paste0("\tPosterior Mean = ",round(x$DS.mean,4), "\n"))
	cat(paste0("\tPosterior Mode = ",round(x$DS.mode,4), "\n"))
	cat(paste0("Use plot(x) to generate posterior plot\n"))
	}
