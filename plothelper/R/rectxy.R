#' Generating Coordinates of Multiple Rectangles 
#'
#' Note: the shapes are correct  
#' only when \code{ggplot2::coord_fixed()} is used.
#'
#' @param x the x coordinates of relative points. 
#' Its length can be larger than 1.
#' See \code{xytype}.
#' @param y the y coordinates of relative points. 
#' Its length can be larger than 1.
#' See \code{xytype}.
#' @param a the side that is parallel to 
#' x-axis before rotation. Its length can be larger than 1.
#' @param b the side that is parallel to 
#' y-axis before rotation. Its length can be larger than 1.
#' @param angle default is 0. The rotation angle in radian.
#' Note: "radian = degree * pi / 180". Its length can be 
#' larger than 1.
#' The rotation direction is anti-clockwise.
#' @param xytype should be one of "middle/center" (default), 
#' "bottomleft", "middleleft/centerleft/left". 
#' It indicates the type of argument 
# x and y. If it is "middle", then x and y are the coordinates 
#' of the middle point of an shape. If it is "middleleft", x and 
#' y are the middle-left coordinates before rotation. If it is 
#' "bottomleft", x and y are the coordinates of the 
#' bottom-left corner.
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
#' dat1=rectxy(x=4, y=3, a=2, b=1, angle=0, xytype="bottomleft", todf=TRUE) 
#' dat2=rectxy(x=4, y=3, a=2, b=1, angle=pi/6, xytype="bottomleft", todf=TRUE)
#' ggplot()+
#' 	geom_polygon(data=dat1, aes(x=x, y=y), fill="red", alpha=0.3)+
#' 	geom_polygon(data=dat2, aes(x=x, y=y), fill="blue", alpha=0.3)+	
#' 	coord_fixed()
rectxy=function(x=0, y=0, a=1, b=1, angle=0, xytype="middle", group=TRUE, todf=TRUE, checks=TRUE){
	if (checks){
		if (is.null(x) || anyNA(x)) stop("x must not be NULL and must have no NA.")
		if (is.null(y) || anyNA(y)) stop("y must not be NULL and must have no NA.")
		if (is.null(a) || anyNA(a)) stop("a must not be NULL and must have no NA.")		
		if (is.null(b) || anyNA(b)) stop("b must not be NULL and must have no NA.")		
		if (is.null(angle) || anyNA(angle)) stop("angle must not be NULL and must have no NA.")		
		stopifnot(group %in% c(TRUE, FALSE))
		stopifnot(todf %in% c(TRUE, FALSE))
	}

	if (! xytype %in% c("bottomleft", "middle", "middleleft", "center", "centerleft", "left")) stop ("xytype must be one of bottomleft, middle/center, middleleft/centerleft/left.")
	if (xytype=="center") xytype="middle"
	if (xytype %in% c("centerleft", "left")) xytype="middleleft"
	
	if (xytype=="bottomleft") 
		RECTXY=function(X, Y, A, B) data.frame(x=c(X, X+A, X+A, X), y=rep(c(Y, Y+B), each=2))
	if (xytype=="middle")
		RECTXY=function(X, Y, A, B){
			A_2=A/2; B_2=B/2
			data.frame(
				x=c(X-A_2, X+A_2, X+A_2, X-A_2), 
				y=rep(c(Y-B_2, Y+B_2), each=2)
			)
		}
	if (xytype=="middleleft") 
		RECTXY=function(X, Y, A, B){
			B_2=B/2
			data.frame(x=c(X, rep(X+A, 2), X), y=rep(c(Y-B_2, Y+B_2), each=2))
		}
	FINAL=mapply(RECTXY, X=x, Y=y, A=a, B=b, SIMPLIFY=FALSE)
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
