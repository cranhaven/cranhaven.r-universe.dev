#' Scale values into a Certain Location
#'
#' A simple function to put numeric values into 
#' a certain interval. Suppose you have 
#' 20, 60, 80, 100, and you want them to be in the 
#' interval of [0, 1], so you can get 0, 0.5, 0.75, 1.
#'
#' @param x a numeric vector or a numeric matrix, 
#' data frame, tibble object.
#' @param left the smallest value of the the interval. 
#' If \code{x} has n columns, then \code{left}
#'  is expected to 
#' be of length n. However, if it is shorter, it will be 
#' repeated to reach that length.
#' @param right the largest value of the the interval. 
#' If \code{x} has n columns, then \code{right}
#'  is expected to 
#' be of length n. However, if it is shorter, it will be 
#' repeated to reach that length.
#' @param reverse whether to assign values in a 
#' reverse way. Default is FALSE. 
#' If \code{x} has n columns, then \code{reverse}
#'  is expected to 
#' be of length n. However, if it is shorter, it will be 
#' repeated to reach that length.
#' @param xmin the min value. Default is NULL, 
#' which means use the min value of \code{x}. However, 
#' sometimes the min value of \code{x} may not be the 
#' true min value. Suppose the two scores of a 
#' 100-point test are 59, 87, then the true min score is 0 
#' and the true max score is 100. Thus you must 
#' add \code{xmin = 0, xmax = 100}. 
#' If \code{reverse = TRUE} (that is, 0 is better than 100), 
#' also add \code{xmin = 0, xmax = 100}. 
#' @param xmax the same meaning as \code{xmin}, 
#' but for max value.
#' @param na.rm used by \code{min} and \code{max}. 
#' Default is FALSE.
#'
#' @export
#' @examples
#' y=scale_free(c(-1, 0, 2))
#' y=scale_free(c(-1, 0, 2), rev=TRUE)
#' #
#' # x is a data frame.
#' x=data.frame(
#' 	c(-1, 0, 0, 0, 2), c(-1, 0, 0, 0, 2), 
#' 	c(-2, 0, 2, 4, 6), c(-2, 0, 2, 4, 6)
#' )
#' y=scale_free(x, 
#' 	left=0, right=10, 
#' 	reverse=c(FALSE, TRUE, FALSE, TRUE)
#' )
#' y=scale_free(x, 
#' 	left=c(0, 0, 100, 100), right=c(10, 100, 200, 200), 
#' 	reverse=c(FALSE, TRUE, FALSE, TRUE)
#' )
scale_free=function(x, left=0, right=1, reverse=FALSE, xmin=NULL, xmax=NULL, na.rm=FALSE){
	cla=class(x)[1]
	if ( ! cla %in% c("matrix", "data.frame", "tbl_df") & is.numeric(x) == FALSE) stop("x must be a numeric vector or a matrix, data frame, tibble object with numeric values.")
	if (! is.numeric(left)||! is.numeric(right)) stop("left, right must be numeric!")	
	if ((is.null(xmin) + is.null(xmax)) == 1)  stop("Either xmin and xmax are all NULL, or they are all non-NULL.")
	if (! cla %in% c("matrix", "data.frame", "tbl_df")){
		if(left >= right) stop("left must be smaller than right!")
		if (is.null(xmin)){
			res=scAlE_frEE(X=x, LEFT=left, RIGHT=right, REVERSE=reverse, NA.RM=na.rm)
		} else {
			res=scAlE_frEE(X=x, LEFT=left, RIGHT=right, REVERSE=reverse, MIN=xmin, MAX=xmax, NA.RM=na.rm)
		}
	} else {
		nc=ncol(x)
		left=rep_len(left, length.out=nc)
		right=rep_len(right, length.out=nc)
		reverse=rep_len(reverse, length.out=nc)
		res=x
		if (is.null(xmin)){
			for (i in 1: nc) res[i]=scAlE_frEE(X=res[i], LEFT=left[i], RIGHT=right[i], REVERSE=reverse[i], NA.RM=na.rm)
		} else {
			xmin=rep_len(xmin, length.out=nc)
			xmax=rep_len(xmax, length.out=nc)
			for (i in 1: nc) res[i]=scAlE_frEE(X=res[i], LEFT=left[i], RIGHT=right[i], REVERSE=reverse[i], MIN=xmin[i], MAX=xmax[i], NA.RM=na.rm)
		}
	}
	res
}	
		
scAlE_frEE=function(X, LEFT=0, RIGHT=1, REVERSE=FALSE, MIN=NULL, MAX=NULL, NA.RM){
	MINMAX=sort(c(MIN, MAX))
	MIN=MINMAX[1]
	MAX=MINMAX[2]
	if (REVERSE==TRUE){
		X=X*(-1)
		maxn=if (is.null(MAX)) base::max(X, na.rm=NA.RM) else -MIN
		minn=if (is.null(MIN)) base::min(X, na.rm=NA.RM) else -MAX
	} else {
		maxn=if (is.null(MAX)) base::max(X, na.rm=NA.RM) else MAX
		minn=if (is.null(MIN)) base::min(X, na.rm=NA.RM) else MIN
	}
	pos=(X-minn)/(maxn-minn)
	pos=(RIGHT-LEFT)*pos
	pos+LEFT
}
