#' Symmetrical Transformation 
#'
#' A1 and A2 are symmetrical on the two sides of 
#' Ax+By+C=0. The input of the function is A1, and 
#' the result is A2. The function also works when the 
#' line is horizontal or vertical.
#' Note: the two shapes are symmetrical 
#' only when \code{ggplot2::coord_fixed()} is used.
#'
#' @param x the input. It can be a data frame, matrix, tibble object, 
#' or a list of 
#' these kinds of objects. Each object must have exactly 2 columns 
#' and must be numeric without NA.
#' If it has more than 2 columns, only the first 2 columns 
#' will be used.
#' @param A for Ax+By+C=0.
#' @param B for Ax+By+C=0.
#' @param C for Ax+By+C=0.
#' @param p1 if A, B, C are not given, you can also give two points 
#' p1 and p2 on the supposed Ax+By+C=0 line. Note: if A, B, C, p1, 
#' p2 are all given, the given A, B, C will be ignored. It must be 
#' a vector of length 2. The first element is x and the second is y.
#' @param p2 see \code{p1}.
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
#' @import plyr
#' @examples
#' library(ggplot2)
#' dat1=data.frame(x=c(0, 2, 2, 0), y=c(0, 0, 1, 1))
#' dat2=ABCxy(dat1, -1, -1, 3)
#' ggplot()+
#' 	coord_fixed()+
#' 	geom_polygon(data=dat1, aes(x=x, y=y), fill="red")+
#' 	geom_polygon(data=dat2, aes(x=x, y=y), fill="blue")+
#' 	geom_abline(intercept=3, slope=-1)		
#' dat3=ABCxy(dat1, p1=c(0, 1), p2=c(-0.5, 0), todf=TRUE)
#' ggplot()+
#' 	coord_fixed()+
#' 	geom_polygon(data=dat1, aes(x=x, y=y), fill="red")+
#' 	geom_polygon(data=dat3, aes(x=x, y=y), fill="blue")+
#' 	geom_abline(intercept=1, slope=2)
ABCxy=function(x, A, B, C, p1=NULL, p2=NULL, f=NULL, group=TRUE, todf=TRUE, checks=TRUE){
	if (is.matrix(x)) x=data.frame(x)
	if (is.data.frame(x)){
		if (is.null(f)) x=list(x) else x=split(x, f)
	}
	if (checks){
		stopifnot(group %in% c(TRUE, FALSE))
		stopifnot(todf %in% c(TRUE, FALSE))
		x=plyr::llply(x, .fun=chEck_rAw_df, column_num=2, must_numeric=TRUE)
	}

	if (! is.null(p1) & ! is.null(p2)){
		x1=p1[1]; y1=p1[2]; x2=p2[1]; y2=p2[2]
		A=y1-y2; B=x2-x1
		if (A==0 & B != 0){
			A=0; B=1; C=-y1
		} else if (A != 0 & B == 0){
			A=1; B=0; C=-x1
		} else if (A == 0 & B ==0){
			stop("The two points are the same.")
		} else {
			A=y1-y2; B=x2-x1; C=x1*y2-x2*y1
		}
	}
	
	stopifnot(length(A)==1 & length(B)==1 & length(C)==1) # do NOT put this upwards
	if (A==0 & B==0) stop("At least one of A and B must not be 0.")
	
	ABCxy_for_each=function(XX, AA, BB, CC){
		res=data.frame(t(apply(XX, 1, InnErAbcxy, aa=AA, bb=BB, cc=CC)))
		colnames(res)=c("x", "y")
		res
	}
	
	FINAL=mapply(ABCxy_for_each, XX=x, AA=A, BB=B, CC=C, SIMPLIFY=FALSE)
	if (group==TRUE){
		FINAL=mapply(
			FUN=function(two_column, addindex) cbind(two_column, g=addindex), 
			two_column=FINAL, addindex=1: length(FINAL), SIMPLIFY=FALSE
		)
	}
	if (todf) do.call(rbind, FINAL) else FINAL
}	

InnErAbcxy=function(xy, aa, bb, cc){
	if (aa==0 & bb != 0){
		c(xy[1], (-2*cc/bb)-xy[2])
	} else if (aa != 0 & bb == 0){
		c((-2*cc/aa)-xy[1], xy[2])
	} else {
		c(xy[1]-(2*aa*(aa*xy[1]+bb*xy[2]+cc)/(aa^2+bb^2)), xy[2]-(2*bb*(aa*xy[1]+bb*xy[2]+cc)/(aa^2+bb^2)))
	}
}
