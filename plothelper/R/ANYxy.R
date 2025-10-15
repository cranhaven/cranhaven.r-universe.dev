#' Generating Groups of Coordinates for Any Polygon
#' 
#' Given your function to create a multiple of
#' points (for example, points to form a polygon), this 
#' function generates x and y coordinates for 
#' groups of points of the same type with different parameters.
#' The output of this function can be shown by 
#' \code{ellipsexy} and \code{rectxy} in this package.
#' 
#' @param myfun your function to generate a single polygon.
#' Note: \bold{the value of each argument of your function 
#' must be a single-value vector. And the result of your 
#' function should be a data frame!}. See examples.
#' @param ... named parameters used by your function. These 
#' parameters will be passed to \code{mapply}.
#' @param MoreArgs this will be passed to the \code{MoreArgs} 
#' argument of \code{mapply}.
#' @param group default is TRUE which means a column 
#' named "g" will be added to each data frame. This facilitates 
#' further drawing using \code{aes(..., group = g)}.
#' @param todf default is TRUE which means to combine the 
#' result into a data frame. Otherwise, the result is a list.
#' 
#' @export
#' @examples
#' library(ggplot2)
#' # First, you need a function to generate
#' # x and y coordinates for a single group
#' # of points.
#' x_square=function(start, end, A, B){
#' 	x=seq(start, end, 0.1)
#' 	data.frame(x=x, y=A*(x^2)+B)
#' }
#' # All the arguments of your function 
#' # (here, start, end, A, B) should only accept
#' # vectors of length 1. And, the result of 
#' # your function should be a data frame
#' # of x and y coordinates 
#' # (here, coordinates of curves).
#' dat=ANYxy(myfun=x_square, 
#'		start=-1, end=1, A=c(1, 2), MoreArgs=list(B=1), 
#'		group=TRUE, todf=TRUE)
#' ggplot(dat)+geom_line(aes(x, y, group=g, color=factor(g)))
ANYxy=function(myfun=NULL, ..., MoreArgs=NULL, group=TRUE, todf=TRUE){ 
	stopifnot(group %in% c(TRUE, FALSE))
	stopifnot(todf %in% c(TRUE, FALSE))
	if (is.null(myfun) || identical (myfun, "HEART")) myfun=HEARTXY_internal
	if (identical(myfun, "RADAR")) myfun=RADARXY
	stopifnot(is.function(myfun))
	if (! is.null(MoreArgs)) stopifnot(is.list(MoreArgs))
	FINAL=mapply(FUN=myfun, ..., MoreArgs=MoreArgs, SIMPLIFY=FALSE)
	if (group==TRUE){
		FINAL=mapply(
			FUN=function(two_column, addindex) cbind(two_column, g=addindex), 
			two_column=FINAL, addindex=1: length(FINAL), SIMPLIFY=FALSE
		)
	}	
	if (todf) do.call(rbind, FINAL) else FINAL
}

HEARTXY_internal=function(x, y, r=1, n=30, p1=13, p2=5, p3=2, p4=1){
	t=seq(0, 2*pi, length.out=n)
	xcurve=r*sin(t)^3
	ycurve=((p1*r)*cos(t) - (p2*r)*cos(2*t) - (p3*r)*cos(3*t) - p4*r*cos(4*t))/16
	data.frame(x=xcurve+x, y=ycurve+y)
 }

 RADARXY=function(r, x=0, y=0, theta=NULL){
	if (is.null(theta)) theta=seq(pi/2, -3*pi/2, length.out=length(r)+1)[1: length(r)]
	# if multiple drawing, r should be a list of vectors, 
	# even when the list has 
	# only one vector. So does theta.
	# The length of each vector in r 
	# should be the same as that of the corresponding vector in theta
	xy=data.frame(x=r*cos(theta)+x, y=r*sin(theta)+y)
}
 