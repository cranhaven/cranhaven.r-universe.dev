print.DS_FB_obj <-
function(x, ...){
	cat(paste0("\tPosterior Mean = ",round(x$post.vec[2],4), "\n"))
	cat(paste0("\tPosterior Mode = ",round(x$post.vec[4],4), "\n"))
	cat(paste0("\tLower Bound Credible Interval = ",round(x$interval[1],4), "\n"))
	cat(paste0("\tUpper Bound Credible Interval = ",round(x$interval[2],4), "\n"))
	cat(paste0("Use plot(x) to generate finite Bayes plot\n"))
	}
