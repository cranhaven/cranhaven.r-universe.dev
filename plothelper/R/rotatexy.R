#' Rotation Transformation 
#'
#' A2 (output) is the result of rotating A1 (input) 
#' around a point.
#' Note: the two shapes look 
#' the same (though with different angles) 
#' only when \code{ggplot2::coord_fixed()} is used.
#'
#' @param x the input. It can be a data frame, matrix, tibble object, 
#' or a list of 
#' these kinds of objects. Each object must have exactly 2 columns 
#' and must be numeric without NA.
#' If it has more than 2 columns, only the first 2 columns 
#' will be used.
#' @param angle default is pi/4. The rotation angle in radian.
#' Note: "radian = degree * pi / 180". Its length can be 
#' larger than 1.
#' The rotation direction is anti-clockwise.
#' @param xmiddle the x coordinates of rotation centers.
#' Its length can be 
#' larger than 1.
#' @param ymiddle the y coordinates of rotation centers.
#' Its length can be 
#' larger than 1.
#' @param f argument passed to \code{split} to divide a data frame 
#' into a list of data frames. It should be a vector whose length is 
#' equal to the number of rows of x (if x is a data frame).
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
#' dat1=data.frame(x=c(0, 4, 4, 0), y=c(0, 0, 2, 2))
#' dat2=data.frame(x=c(5, 6, 6, 5), y=c(4, 4, 8, 8))
#' dat3=rotatexy(list(dat1, dat2), angle=c(pi, pi/4), 
#'		xmiddle=c(0, 5), ymiddle=c(0, 4), todf=TRUE)
#' ggplot()+
#' 	coord_fixed()+
#' 	geom_polygon(data=dat1, aes(x=x, y=y), fill="red", alpha=0.2)+
#' 	geom_polygon(data=dat2, aes(x=x, y=y), fill="blue", alpha=0.2)+
#' 	geom_polygon(show.legend=FALSE, data=dat3, 
#'			aes(x=x, y=y, group=g, fill=factor(g)), alpha=0.2)
rotatexy=function(x, angle=pi/4, xmiddle=0, ymiddle=0, f=NULL, group=TRUE, todf=TRUE, checks=TRUE){
	if (is.matrix(x)) x=data.frame(x)
	if (is.data.frame(x)){
		if (is.null(f)) x=list(x) else x=split(x, f)
	}
	if (checks){
		stopifnot(group %in% c(TRUE, FALSE))
		stopifnot(todf %in% c(TRUE, FALSE))
		x=plyr::llply(x, .fun=chEck_rAw_df, column_num=2, must_numeric=TRUE)
	}

	FINAL=mapply(FUN=ROTATEXY, X=x, ANGLE=angle, X0=xmiddle, Y0=ymiddle, SIMPLIFY=FALSE)
	if (group==TRUE){
		FINAL=mapply(
			FUN=function(two_column, addindex) cbind(two_column, g=addindex), 
			two_column=FINAL, addindex=1: length(FINAL), SIMPLIFY=FALSE
		)
	}
	if (todf) do.call(rbind, FINAL) else FINAL
}

ROTATEXY=function(X, ANGLE, X0=0, Y0=0){
	# basic version
	cosa=cos(ANGLE); sina=sin(ANGLE)
	xx=X[, 1]-X0; yy=X[, 2]-Y0
	data.frame(x=X0+cosa*xx-sina*yy, y=Y0+sina*xx+cosa*yy)
}
# NOTE transform and plyr::mutate cannot pass check( ), so use the simplest function
# ROTATEXY_mutate=function(X, ANGLE, X0=0, Y0=0){
# 	# mutate version
# 	cosa=cos(ANGLE); sina=sin(ANGLE)
# 	colnames(X)=c("x", "y")
# 	res=plyr::mutate(X, xx=x-X0, yy=y-Y0)
# 	plyr::mutate(res, x=X0+cosa*xx-sina*yy, y=Y0+sina*xx+cosa*yy)[, 1: 2]
# }
# ROTATEXY_transform=function(X, ANGLE, X0=0, Y0=0){
# 	# transform version
# 	cosa=cos(ANGLE); sina=sin(ANGLE)
# 	colnames(X)=c("x", "y")
# 	res=transform(X, xx=x-X0, yy=y-Y0)
# 	transform(res, x=X0+cosa*xx-sina*yy, y=Y0+sina*xx+cosa*yy)[, 1: 2]
# }
# tt=data.frame(x=10: 19, y=30: 39)
# res1=ROTATEXY_plyr(tt, 3, 2, 4)
# res2=ROTATEXY(tt, 3, 2, 4)
