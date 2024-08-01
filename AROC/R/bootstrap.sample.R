bootstrap.sample <-
function(data, group, method = c("ncoutcome","coutcome")) {
	method <- match.arg(method)
	if(method == "coutcome") { 
		res <- do.call("rbind", lapply(split(data,data[,group]), function(x)x[sample(nrow(x), replace=TRUE),]))       
	} else {
		res <- data[sample(nrow(data), replace=TRUE),]
	}
	res
}
