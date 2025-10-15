#' Generating Coordinates of Multiple Ellipses or Circles
#'
#' If radius a is equal to radius b, then the shape
#' will be a circle. 
#' Note: the shapes are correct  
#' only when \code{ggplot2::coord_fixed()} is used.
#'
#' @param x the x coordinates of relative points. 
#' Its length can be larger than 1.
#' See \code{xytype}.
#' @param y the y coordinates of relative points. 
#' Its length can be larger than 1.
#' See \code{xytype}.
#' @param a the radius that is parallel to 
#' x-axis before rotation. Its length can be larger than 1.
#' @param b the radius that is parallel to 
#' y-axis before rotation. Its length can be larger than 1.
#' @param start default is 0. The angle of the starting 
#' point of the arc. Its length can be larger than 1.
#' Note: "radian = degree * pi / 180". 
#' @param end default is 6.283185. The angle of the ending 
#' point of the arc.
#' Its length can be 
#' larger than 1.
#' @param angle default is 0. The rotation angle in radian.
#' Its length can be larger than 1.
#' Note: "radian = degree * pi / 180". 
#' The rotation direction is anti-clockwise.
#' @param n default is 40. The number of points used to 
#' draw an arc. The larger, the smoother.
#' It must at least be 4. However, when \code{checks} is 
#' FALSE, this check is ignored.
#' NOTE: to draw a triangle, you must use 
#' \code{ellipsexy(n=4, fan=FALSE)}, 
#' as the first and 4th points are so close. 
#' Similarly, to draw a rectangle, 
#' use \code{ellipsexy(n=5, fan=FALSE)}.
#' @param xytype should be one of "middle/center" (default), 
#' "bottomleft", "middleleft/left/centerleft". 
#' It indicates the type of argument 
# x and y. If it is "middle", then x and y are the coordinates 
#' of the middle point of an ellipse. If it is "middleleft", x and 
#' y are the middle-left coordinates before rotation. If it is 
#' "bottomleft", x and y are the coordinates of the 
#' bottom-left corner of the rectangle that walls the ellipse.
#' @param fan default is FALSE. If it is TRUE, the coordinates 
#' of the middle of an ellipse is added to the output 
#' data frame. Meanwhile, if, say, you set n = 50, then n 
#' becomes 49 automatically because the last position is reserved 
#' for the middle. This helps draw a fan.
#' @param group default is TRUE. It indicates 
#' whether to add a 3rd column named 
#' "g" to label the group number of each group of points. It is useful 
#' when using \code{aes(...group=g)} with 'ggplot2'.
#' @param todf default is TRUE. It indicates whether to 
#' combine the output (a list) into a data frame.
#' @param checks default is TRUE. It indicates whether 
#' to check input validity. Do not turn it off unless you are sure 
#' that the input is OK.
#'
#' @return if \code{todf = TRUE}, the output will be a data frame
#' with coordinates of possibly several polygons, otherwise, 
#' it will be a list of data frames. Data frames have 2 columns
#' named "x" and "y", and if \code{group = TRUE}, a third column 
#' named "g" is added indicating group numbers.
#'
#' @export
#' @examples
#' library(ggplot2)
#' dat1=ellipsexy(x=1, y=1, 
#'	a=seq(1, 4, length.out=8), angle=seq(0, pi, length.out=8), 
#'		xytype="middleleft", n=30, todf=TRUE)		
#' ggplot()+coord_fixed()+
#' 	geom_polygon(show.legend=FALSE, 
#'			data=dat1, aes(x=x, y=y, group=g, fill=factor(g)), alpha=0.3)
ellipsexy=function(x=0, y=0, a=2, b=1, start=0, end=6.283185, angle=0, n=40, xytype="middle", fan=FALSE, group=TRUE, todf=TRUE, checks=TRUE){ 
	
	if (checks == TRUE){
		if (any(n<4)) stop("Each element in n must be >= 4.")
		# in case sometimes n=n-1, here n must at least be 4 rather than 3
		if (is.null(x) || anyNA(x)) stop("x must not be NULL and must have no NA.")
		if (is.null(y) || anyNA(y)) stop("y must not be NULL and must have no NA.")
		if (is.null(a) || anyNA(a)) stop("a must not be NULL and must have no NA.")		
		if (is.null(b) || anyNA(b)) stop("b must not be NULL and must have no NA.")		
		if (is.null(start) || anyNA(start)) stop("start must not be NULL and must have no NA.")
		if (is.null(end) || anyNA(end)) stop("end must not be NULL and must have no NA.")
		if (is.null(angle) || anyNA(angle)) stop("angle must not be NULL and must have no NA.")		
		if (is.null(n) || anyNA(n)) stop("n must not be NULL and must have no NA.")
		if (is.null(fan) || anyNA(fan)) stop("fan must not be NULL and must have no NA.")
		stopifnot(group %in% c(TRUE, FALSE))
		stopifnot(todf %in% c(TRUE, FALSE))
	}
	if (! is.logical(fan)) stop("fan must be a vector of logical values.")
	if (! xytype %in% c("bottomleft", "middle", "middleleft", "center", "centerleft", "left")) stop ("xytype must be one of bottomleft, middle/center, middleleft/centerleft/left.")
	if (xytype=="center") xytype="middle"
	if (xytype %in% c("centerleft", "left")) xytype="middleleft"
	# if (all.equal(length(x), length(y), length(a), length(b), length(start), length(end), length(angel)) != TRUE) stop("x, y, a, b, start, end, angle must be of the same length.")
	
	ELLIPSEXY=function(X, Y, A, B, START, END, N, XYTYPE, FAN){
		theta=seq(START, END, length.out = if (FAN==FALSE) N else N-1)
		if (XYTYPE=="middle"){
			xy=data.frame(x=A*cos(theta)+X, y=B*sin(theta)+Y)
			if (FAN) rbind(xy, c(X, Y)) else xy
		} else if (XYTYPE=="middleleft"){
			xy=data.frame(x=A*cos(theta)+X+A, y=B*sin(theta)+Y)
			if (FAN) rbind(xy, c(X+A, Y)) else xy
		} else if (XYTYPE=="bottomleft"){
			xy=data.frame(x=A*cos(theta)+X+A, y=B*sin(theta)+Y+B)
			if (FAN) rbind(xy, c(X+A, Y+B)) else xy
		}
	}
	FINAL=mapply(ELLIPSEXY, X=x, Y=y, A=a, B=b, START=start, END=end, N=n, FAN=fan, MoreArgs=list(XYTYPE=xytype), SIMPLIFY=FALSE)
	
	if (any(angle !=0)){
		FINAL=mapply(ROTATEXY, X=FINAL, ANGLE=angle, X0=x, Y0=y, SIMPLIFY=FALSE)
	}
	if (group==TRUE){
		FINAL=mapply(
			FUN=function(two_column, addindex) cbind(two_column, g=addindex), 
			two_column=FINAL, addindex=1: length(FINAL), SIMPLIFY=FALSE
		)
	}
	if (todf) do.call(rbind, FINAL) else FINAL
}	
