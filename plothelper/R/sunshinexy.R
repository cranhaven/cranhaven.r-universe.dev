#' Generating Lines Which Link One Points to Many 
#'
#' Suppose there is a middle point a, this function 
#' simultaneous generates points on lines that start 
#' from a to other points.
#'
#' @param x the x coordinate of the middle points. 
#' It should be of length 1.
#' @param y the y coordinate of the middle points. 
#' It should be of length 1.
#' @param outer the other points. It can be a data frame, 
#‘ matrix, tibble object. 
#' It must have exactly 2 columns 
#' and must be numeric without NA.
#' @param n default is 10. The number of points per line.
#' @param delete_n default is 0. The number of points to be deleted. 
#' Suppose a line has p1, p2, p3, p4, p5 points on it with 
#' p1 as the starting point. if delete_n is 2, then p1 and p2 
#' will be deleted. Note: \code{n - delete_n} 
#' must be larger than 1.
#' @param distance default is FALSE. If it is TRUE, a column 
#' named "distance" is added which indicates the distances from 
#' the middle point to other points.
#' @param checks default is TRUE. It indicates whether 
#' to check input validity. Do not turn it off unless you are sure 
#' that the input is OK.
#'
#' @return A data frame that has 3 columns.
#' The first and second columns are 
#' named "x" and "y", the third column is  
#' named "g" indicating group numbers.
#' If \code{distance = TRUE}, a fourth column is added 
#' which indicates the distances from 
#' the middle point to other points.
#'
#' @export
#' @examples
#' library(ggplot2)
#' p=c(1, 1, 0, -1, -1, -1, 0, 1)
#' q=c(0, 1, 1, 1, 0, -1, -1, -1)
#' pq=data.frame(cbind(p, q))
#' dat=sunshinexy(outer=pq, n=20, delete_n=5, distance=TRUE)
#' ggplot()+coord_fixed()+theme_void()+
#' 	geom_point(data=pq, aes(p, q), size=4)+
#' 	geom_line(show.legend=FALSE, data=dat, aes(x, y, group=g, color=distance), size=2)+
#' 	scale_color_continuous(low="blue", high="red")
sunshinexy=function(x=0, y=0, outer=data.frame(1, 1), n=10, delete_n=0, distance=FALSE, checks=TRUE){
	# do NOT use approx because it fails when approx(x=c(0, 0), y=c(0, 2))

	if (checks){
		if (is.null(x) || anyNA(x)) stop("x must not be NULL and must have no NA.")
		if (is.null(y) || anyNA(y)) stop("y must not be NULL and must have no NA.")
		stopifnot(distance %in% c(TRUE, FALSE))
		outer=chEck_rAw_df(outer, column_num=2, must_numeric=TRUE)
	}	
	stopifnot(n>=2)
	stopifnot(delete_n >= 0 & delete_n < n)
	if ((n-delete_n) < 2) stop("At least 2 points should be left after deleting.")
	
	# multiple local function definitions for 'Apply_sUnshInE'
    # with different formal arguments
	# so add 1, 2
	
	if (delete_n==0){
		Apply_sUnshInE1=function(target, origin, N) seq.int(from=origin, to=target, length.out=N)
		result=data.frame(
			x=unlist(plyr::llply(outer[, 1], Apply_sUnshInE1, origin=x, N=n)), 
			y=unlist(plyr::llply(outer[, 2], Apply_sUnshInE1, origin=y, N=n))
		)
		g=rep(1: nrow(outer), each=n)
	} else {
		Apply_sUnshInE2=function(target, origin, N, delete_N) seq.int(from=origin, to=target, length.out=N)[-c(1: delete_N)]
		result=data.frame(
			x=unlist(plyr::llply(outer[, 1], Apply_sUnshInE2, origin=x, N=n, delete_N=delete_n)), 
			y=unlist(plyr::llply(outer[, 2], Apply_sUnshInE2, origin=y, N=n, delete_N=delete_n))
		)
		g=rep(1: nrow(outer), each=n-delete_n)
	} 
	if (distance==FALSE){
		cbind(result, g)
	} else {
		distance=sqrt((result[, 1]-x)^2+(result[, 2]-y)^2)
		cbind(result, g, distance)
	}
}	
