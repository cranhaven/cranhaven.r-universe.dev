#' Checking Min, Max, Labels and Label Positions
#'
#' Given a numeric vector or a ggplot object, the function
#' will check the range, labels and label 
#' positions (the same as major grid lines) that 
#' will used on the axis. The result is a length 5 list for 
#' min limit, max limit, labels, major grid-line positions, 
#' all (major and minor) grid-line positions.
#' 
#' @param a extreme values of a numeric vector. Note: only 
#' one of \code{a}, \code{v}, \code{gg} can be non-NULL.
#' It can also be a gg object.
#' @param b another extreme value if \code{a} is not NULL.
#' @param v a numeric vector.
#' @param gg a gg object created by ggplot function.
#' Which value will be checked depends on \code{axis}.
#' @param mult default is 0.05 and should be of length 
#' 1 or 2. It mimics the \code{mult} argument of 
#' \code{ggplot2::expansion}. It is only used when 
#' a is numeric or v is non-NULL.
#' @param add default is 0. It mimics the \code{add} 
#' argument of \code{ggplot2::expansion}.
#' @param axis if \code{gg} is used or \code{a} is a 
#' ggplot object, 
#' which axis will 
#' be checked? It can be "x" or "y" (default).
#'
#' @export
#' @examples
#' get_gg_label(a=1, b=1000)
#' # The following three have the same results.
#' get_gg_label(a=1, b=1000, mult=0)
#' get_gg_label(v=c(1, 500, 1000), mult=0)
#' p=ggplot()+geom_point(aes(1: 3, c(1, 500, 1000)))+
#'   scale_y_continuous(expand=expansion(mult=0))
#' get_gg_label(gg=p)
get_gg_label=function(a=NULL, b=NULL, v=NULL, gg=NULL, mult=0.05, add=0, axis="y"){
	## ggplot2::expansion uses the folloing method for numeric:
	## Suppose min=3, max=10, mult=c(1.5, 1.8), add=c(5, 6), then
	## the dif=10-3=7
	## x left range=3-7*1.5-5
	## x right range=10+7*1.8+6
	
	stopifnot(axis %in% c("x", "y"))	
	
	if ((!is.null(a)) + (!is.null(v)) + (!is.null(gg)) != 1) stop("Only 1 of a, v and gg can be non-NULL.")
	
	a_is_gg=if (class(a)[1] == "gg") TRUE else FALSE
	
	if (is.null(gg) & a_is_gg == FALSE){
		if (is.null(a)){
			a=min(v, na.rm=TRUE)
			b=max(v, na.rm=TRUE)
		}
		create_new_gg=ggplot2::ggplot()+ggplot2::geom_blank(aes(x=c(1, 2), y=c(a, b)))+ggplot2::scale_y_continuous(expand=ggplot2::expansion(mult=mult, add=add))
		all_item=INNER_GET_LABEL(OBJ=create_new_gg, WHICH="y")
	} else {
		all_item=INNER_GET_LABEL(
			OBJ=if (a_is_gg == TRUE) a else gg, 
			WHICH=axis
		)
	}
	
	return(all_item)
}
	
INNER_GET_LABEL=function(OBJ, WHICH="y"){
	OBJ=ggplot2::ggplot_build(OBJ)$layout$panel_params[[1]]
	if (WHICH=="y"){
		RNG1=OBJ$y.range[1]
		RNG2=OBJ$y.range[2]
	} else {
		RNG1=OBJ$x.range[1]
		RNG2=OBJ$x.range[2]
	}
	
	OBJ=if (WHICH=="y") OBJ$y else OBJ$x
	BREAKS=OBJ$breaks
	MINOR_BREAKS=OBJ$minor_breaks
	
	LABELS=OBJ$scale$get_labels(BREAKS)
	
	list(min=RNG1, max=RNG2, label=LABELS, position=BREAKS, all=MINOR_BREAKS)
}
