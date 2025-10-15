#' Stretching Transformation 
#'
#' A2 (output) is the result of enlarging (or shrinking) A1 (input) 
#' in x dimension and y dimension.
#' Note: the two shapes manifest enlarging or 
#' shrinking effect 
#' only when \code{ggplot2::coord_fixed()} is used.
#'
#' @param x the input. It can be a data frame, matrix, tibble object, 
#' or a list of 
#' these kinds of objects. Each object must have exactly 2 columns 
#' and must be numeric without NA.
#' If it has more than 2 columns, only the first 2 columns 
#' will be used.
#' @param xlarge the enlarging extent in x dimension.
#' If it is smaller than 1, the shape will be shrinking.
#' @param ylarge the enlarging extent in y dimension.
#' If it is smaller than 1, the shape will be shrinking.
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
#' dat1=data.frame(x=c(0, 1, 1), y=c(0, 0, 1))
#' dat2=data.frame(x=c(4, 5, 5, 4), y=c(0, 0, 3, 3))
#' dat3=stretchxy(list(dat1, dat2), xlarge=3, ylarge=c(3, 2), todf=TRUE)
#' ggplot()+coord_fixed()+
#' 	geom_polygon(data=dat1, aes(x, y), fill="red", alpha=0.3)+
#' 	geom_polygon(data=dat2, aes(x, y), fill="blue", alpha=0.3)+
#' 	geom_polygon(data=dat3, aes(x, y, fill=g, group=g), fill="blue", alpha=0.3)	
stretchxy=function(x, xlarge=2, ylarge=2, f=NULL, group=TRUE, todf=TRUE, checks=TRUE){
	if (is.matrix(x)) x=data.frame(x)
	if (is.data.frame(x)){
		if (is.null(f)) x=list(x) else x=split(x, f)
	}
	if (checks){
		if (anyNA(xlarge) | anyNA(ylarge)) stop("xlarge and ylarge must not have NA.")
		if (any(xlarge<0) | any(ylarge<0)) stop("xlarge and ylarge must be larger than 0.")
		stopifnot(group %in% c(TRUE, FALSE))
		stopifnot(todf %in% c(TRUE, FALSE))
		x=plyr::llply(x, .fun=chEck_rAw_df, column_num=2, must_numeric=TRUE)
	}
	
	STRETCHXY=function(X, XLARGE=2, YLARGE=2) data.frame(x=X[, 1]+(X[, 1]-mean(X[, 1]))*(XLARGE-1), y=X[, 2]+(X[, 2]-mean(X[, 2]))*(YLARGE-1))
	
	FINAL=mapply(STRETCHXY, X=x, XLARGE=xlarge, YLARGE=ylarge, SIMPLIFY=FALSE)
	if (group==TRUE){
		FINAL=mapply(
			FUN=function(two_column, addindex) cbind(two_column, g=addindex), 
			two_column=FINAL, addindex=1: length(FINAL), SIMPLIFY=FALSE
		)
	}
	if (todf) do.call(rbind, FINAL) else FINAL	
}
