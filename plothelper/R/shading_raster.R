#' Create a Shading Raster with a Palette
#' 
#' The function is a simple wrapper of 
#' \code{scales::col_numeric}.
#' The function creates a matrix of colors 
#' that can be used to draw a shading rectangle.
#' There are 2 ways to use the function, see 
#' the following parameters.
#'
#' @param nr method 1 to use 
#' this function is to use \code{nr}, \code{nc}, 
#' \code{middle}. Suppose there is a matrix 
#' with \code{nr} rows and \code{nc} columns.
#' A cell whose position in the matrix is designated 
#' by \code{middle}. Then, this cell gets the first 
#' color of \code{palette}, and other cells get shading 
#' colors according to their distances between them 
#' and \code{middle}. Method 2 to use this 
#' function is to use \code{mat}. The biggest cell 
#' gets the first color and other cells get shading colors.
#' @param nc see \code{nr}.
#' @param middle see \code{nr}. The parameter 
#' should be a length 2 vector designating the 
#' row number and column number of a cell.
#' @param palette two or more colors used to make 
#' shading colors.
#' @param mat see \code{nr}.
#' @param FUN the default NULL makes the colors 
#' distributed in a linear way. However, \code{FUN}
#' can be a single parameter function which 
#' transforms the numeric values, such as 
#' \code{log}, \code{sqrt}.
#' 
#' @export
#' @examples
#' # Use method 1.
#' r=shading_raster(nr=31, nc=60, middle=c(10, 55), 
#' 	palette=c("darkorange", "red", "purple"))
#' ggplot()+xlim(0, 8)+ylim(0, 6)+
#' 	annotation_raster(r, xmin=-Inf, xmax=Inf, 
#' 	ymin=-Inf, ymax=Inf, interpolate=TRUE)
#'  # Use method 2.
#' r=matrix(c(
#' 	1, 2, 3, 4, 5, 6, 7, 8, 
#' 	1, 2, 3, 4, 5, 6, 7, 8, 
#' 	1, 1, 1, 1, 1, 1, 1, 1), 
#' 	nrow=3, byrow=TRUE)
#' r=shading_raster(mat=r, palette=c("green", "blue"))
shading_raster=function(nr=NULL, nc=NULL, middle=NULL, palette=c("blue", "red"), mat=NULL, FUN=NULL){
	if ( (is.null(nr)) + (is.null(mat)) != 1 ) stop("One and only one of nr and mat should be NULL.")
	if ( ! is.null(nr)){
		mid1=middle[1]
		mid2=middle[2]
		m=mapply(GET_SQRTABC, MIDX=mid1, MIDY=mid2, 
			X=rep(1: nr, times=nc), 
			Y=rep(1: nc, each=nr), 
			SIMPLIFY=TRUE
		)
		if (mid1 >= 1 & mid1 <= nr & mid2 >= 1 & mid2 <= nc) m[nr*(mid2-1)+mid1]=0.1
		if (!is.null(FUN)) m=match.fun(FUN)(m)
		m=scales::col_numeric(palette, domain=range(m))(m)
		m=matrix(m, nrow=nr)
	} 
	if (! is.null(mat)){
		nr_mat=nrow(mat)
		mat=as.numeric(mat)
		if (!is.null(FUN)) m=match.fun(FUN)(m)
		mat=scales::col_numeric(palette, domain=range(mat))(mat)
		mat=matrix(mat, nrow=nr_mat)
	}
	if (is.null(nr)) mat else m
}

GET_SQRTABC=function(X, Y, MIDX, MIDY) sqrt(((MIDX - X)^2)+((MIDY - Y)^2))
