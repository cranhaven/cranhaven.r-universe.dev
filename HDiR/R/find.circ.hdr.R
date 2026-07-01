#' find.circ.hdr
#'
#' Calculates the boundaries of HDRs in the circular setting
#'
#' @param x Numeric vector in [0,2pi]
#' @param f Numeric vector f(x), usually a density
#' @param level Threshold of the HDR
find.circ.hdr<-function(x,f,level){
	if(!is.numeric(x)|!is.numeric(f)|(length(x)!=length(f))) stop("arguments 'x' and 'f' must be numeric with the same length")
	if((level<0)|(level>max(f))){
		return(c(NA,NA))
	}else{
		xaux<-rep(NA,length=length(x)-1)
      	for(i in 1:(length(x)-1)){if(((f[i]-level)*(f[i+1]-level))<0 ){xaux[i]=(x[i]+x[i+1])/2}}
	      return(xaux[!is.na(xaux)])
	}
}
#' @noRd
#' @keywords internal
