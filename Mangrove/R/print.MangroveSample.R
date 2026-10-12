print.MangroveSample <-
function(x,...){
	cat("A Mangrove simulation.\n")
	cat("Number of individuals: ")
	cat(x$N)
	cat("\nNumber of cases observed: ")
	cat(x$Ncases)
	cat("\nprevalence of the disease: ")
	cat(x$K)
	cat("\nResults:\n")
	print.default(x$sample)
	}

