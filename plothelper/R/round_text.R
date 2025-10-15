#' Converting Numeric Values into Characters with the Same Digits
#'
#' This simple function is to facilitate something like 
#' decimal horizontal adjustment 
#' which demands each value has the 
#' same digits after the decimal point.
#' 
#' @param x a vector of numeric values.
#' @param digits digits which is to be passed 
#' to \code{round}. It should not be smaller than 0.
#' @param na how to show NAs. The default is 
#' to show " NA", however, you can change it 
#' to "NA" or simply \code{NA}.
#'
#' @export
#' @examples
#' v=c(3, 3.1, 3.456, 3.452, 3.77, NA, 0, 10.56332)
#' res=round_text(v, 2, na=NA)
round_text=function(x, digits=2, na=NULL){
	if (! is.vector(x)) stop("x must be a numeric vector.")
	stopifnot(is.numeric(x))
	stopifnot(digits >= 0)
	y=formatC(x, format="f", digits=digits, drop0trailing=FALSE)
	if (is.null(na)){
		y
	} else {
		if (! is.na(na)) na=as.character(na)
		y[which(y == " NA")]=na
		y
	}
}

# round_text=function(x, digits=2){
# 	if (! is.vector(x)) stop("x must be a numeric vector.")
# 	stopifnot(is.numeric(x))
# 	stopifnot(digits >= 0)
# 	x=as.character(round(x, digits=digits))
# 	innerpaster=function(X, D){
# 		if (! grepl("\\.", X)){
# 			paste0(c(X, ".", rep("0", D)), collapse="")
# 		} else {
# 			DIF=D-(nchar(regmatches(X, gregexpr("\\.\\d.*$", X)))-1)
# 			if (DIF<=0) X else paste0(c(X, rep(0, DIF)), collapse="")
# 		}
# 	}
# 	res=unlist(lapply(x, innerpaster, D=digits))
# 	if (digits==0) res=gsub("\\.", "", res)
# 	res[grepl("NA", res)]=NA
# 	res
# }
