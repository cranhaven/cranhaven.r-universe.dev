print.fads<-function(x,...) {
	NextMethod()
}

print.fads.kfun<-function(x,...) {
	cat("Univariate second-order neighbourhood functions:\n")
	str(x)	
}

print.fads.kdfun<-function(x,...) {
  cat("Multiscale second-order neigbourhood analysis of a spatial phylogenetic or functional diversity pattern from fully mapped data:\n")
  str(x)   
}

print.fads.k12fun<-function(x,...) {
	cat("Bivariate second-order neighbourhood functions:\n")
	str(x)	
}

print.fads.kpqfun<-function(x,...) {
	cat("Multivariate second-order neighbourhood functions :\n")
	cat("Interaction between each category p and each category q\n")
	str(x)
}

print.fads.kp.fun<-function(x,...) {
	cat("Multivariate second-order neighbourhood functions:\n")
	cat("Interaction between each category p and all the remaining categories.\n")
	str(x)
}

print.fads.kmfun<-function(x,...) {
	cat("Mark correlation functions:\n")
	str(x)
}

print.fads.ksfun<-function(x,...) {
	cat("Shimatani multivariate functions:\n")
	str(x)
}

print.fads.krfun<-function(x,...) {
	cat("Rao multivariate functions:\n")
	str(x)
}
