#' Drawing Barplot with Shading Colors
#' 
#' In ordinary barplot, each bar has only one color.
#' This function aims to draw a barplot whose bars 
#' have shading effect. Note: unlike 
#' \code{ggplot2::geom_bar}, this function can only 
#' deals with a vector of frequencies.
#'
#' @param v a vector of item frequencies.
#' Negative values are OK.
#' @param labels a vector of item names.
#' Its length should be equal to that of \code{v}.
#' If it is NULL, default names will be used.
#' If it is of class numeric or factor, it will be 
#' transformed to a character vector.
#' @param raster a list. The length of the list 
#' should be equal to that of \code{v}. 
#' Each element of the list should be 
#' a color vector corresponding to a value 
#' in \code{v}. If it is a vector, it will be 
#' automatically transformed to a list. 
#' If its length is 1, but the length of 
#' \code{v} is, say, 3, then it will be 
#' automatically repeated for 3 times.
#' Let us suppose \code{v = 5} 
#' and \code{raster = list(c("green", "red"))}.
#' This means the starting side of the bar is 
#' green and the far side is red. See examples.
#' @param flip default is FALSE and the bars 
#' are vertical. When it is TRUE, the bars are 
#' horizontal. Note: when using this function, 
#' \bold{DO NOT USE} 
#' \code{ggplot2::coord_flip} !
#' @param change_order when it is "normal" (default), 
#' the drawing order is the order of \code{v}.
#' When it is "big", big values will be drawn first.
#' When it is "small", small values will be drawn first.
#' When it is "rev", the inverse order of \code{v} 
#' will be used.
#' @param equal_scale default is FALSE. When it is 
#' TRUE, a bar will use a certain part of the shading 
#' colors according to a global scale. See examples.
#' @param smooth default is 15. The number of 
#' shading colors each bar has. The bigger, the better.
#' @param interpolate when it is TRUE (default), it 
#' makes the colors smoother.
#' @param width the width of each bar. It should be 
#' between 0 and 1.
#' @param color color of the outlines of the bars.
#' @param linetype line type of the outlines 
#' of the bars.
#' @param size line width of the outlines of the bars.
#' @param modify_raster if it is TRUE (default), colors 
#' will be smoothed using the value of \code{smooth}. 
#' If \code{raster} has enough colors, you can set 
#' this to FALSE.
#' @param space the \code{space} parameter used by 
#' \code{colorRampPalette}. It should be 
#' "rgb" (default) or "Lab".
#' @param ... additional arguments used by 
#' \code{ggplot2::coord_flip} when \code{flip = TRUE}.
#' 
#' @export
#' @import grDevices
#' @examples
#' library(ggplot2)
#' x=c(10, 30, 25, 6)
#' lab=c("children", "youth", "middle", "aged")
#' r=list(c("cyan", "red"), c("blue", "yellow"), 
#' 	c("green", "orange"), c("grey", "black"))
#' #
#' ## (1) change_order
#' # change_order = "ordinary", the default
#' p1=gg_shading_bar(v=x, labels=lab)
#' # change_order = "big"
#' p2=gg_shading_bar(v=x, labels=lab, change_order="big")
#' # flip and let the largest on the top
#' p3=gg_shading_bar(v=x, labels=lab, 
#' 	change_order="small", flip=TRUE)
#' #
#' ## (2) how to use argument raster
#' p1=gg_shading_bar(v=x, labels=lab, raster=r)
#' p2=gg_shading_bar(v=x, labels=lab, raster=c("green","red"))
#' #
#' ## (3) how to use argument equal_scale
#' # equal_scale = FALSE
#' # the far side of each bar is red
#' gg_shading_bar(c(3, 5), raster=c("green", "red"))
#' # equal_scale = TRUE
#' # the far side of the shorter bar 
#' # is not red. Rather, it is something
#' #' between red and green
#' gg_shading_bar(c(3, 5), raster=c("green", "red"), 
#' 	equal_scale=TRUE)
gg_shading_bar=function(v, labels=NULL, raster=NULL, flip=FALSE, change_order="normal", equal_scale=FALSE, smooth=15, interpolate=TRUE, width=0.8, color=NA, linetype=1, size=1, modify_raster=TRUE, space="rgb", ...){
    nlen=length(v)
	if (nlen==0) stop("v must not be of length 0.")
    if (! is.numeric(v)) stop("v must be numeric.")	
    if (anyNA(v)) stop("v must have no NA.")	
    if (! any(v != 0)) stop("v must have at least one non-zero value.")
	names(v)=NULL
    if (!is.null(labels)){
        if (anyNA(labels)) stop("If labels is not NULL, it must have no NA.") 
		if (anyDuplicated(labels) != 0) stop("Names in labels must be unique.")
		if (is.numeric(labels) | is.factor(labels)) labels=as.character(labels)
        stopifnot(is.character(labels))
    }
	if (smooth<5){
		smooth=5
		message("smooth is automatically set to 5 due to a smaller-than-5 input.")
	}
    stopifnot(flip %in% c(TRUE, FALSE))
    stopifnot(change_order %in% c("normal", "small", "big", "rev"))
    stopifnot(interpolate %in% c(TRUE, FALSE))
    stopifnot(equal_scale %in% c(TRUE, FALSE))	
    stopifnot(width>0 & width<=1)
    w_2=width/2
    if (is.null(labels)) labels=paste0("x", 1: nlen, sep="")
    if (is.null(raster)) raster=list(c("blue", "red"))[rep(1, nlen)]
    if (is.vector(raster) & ! is.list(raster)) raster=list(raster)
    raster_len=length(raster)
    if (raster_len != nlen){
        if (raster_len == 1) raster=raster[rep(1, nlen)] else stop("Length of raster must either be 1 or equal to the length of v.")
    }
    
	if (modify_raster==TRUE){
		raster=transform_raster_list(x=raster, smooth=smooth, equal_scale=equal_scale, raw_data=v, SPACE=space)
    }
	
    if (change_order=="rev"){
        v=rev(v)
        labels=rev(labels)
        raster=raster[nlen: 1]
    }
	if (change_order=="small"){
		o=order(v)
		v=v[o]
		labels=labels[o]
		raster=raster[o]
	}
	if (change_order=="big"){
		o=order(v, decreasing=TRUE)
		v=v[o]
		labels=labels[o]
		raster=raster[o]
	}	
    
    inner_p=ggplot2::ggplot()
	mAnyrAstEr=function(xx, V, RASTER, FLIP, W_2, interpolate){
        iy=V[xx]
        iraster=if (FLIP==FALSE) grDevices::as.raster(rev(RASTER[[xx]])) else grDevices::as.raster(matrix(RASTER[[xx]], nrow=1))
        iymin=min(c(0, iy))
        iymax=max(c(iy, 0))
        ixmin=xx-W_2
        ixmax=xx+W_2
        ggplot2::annotation_raster(raster=iraster, xmin=ixmin, xmax=ixmax, ymin=iymin, ymax=iymax, interpolate=interpolate)
    }
	L=plyr::llply(1: nlen, .fun=mAnyrAstEr, V=v, RASTER=raster, FLIP=flip, W_2=width/2, interpolate=interpolate)
	
    re_labels=factor(labels, levels=as.character(labels))
    gb=ggplot2::geom_bar(aes(x=re_labels, y=v), width=width, stat="identity", color=color, fill=NA, size=size, linetype=linetype)
    if (flip==FALSE){
		inner_p+gb+L
	} else {
		message("The function has already used ggplot2::coord_flip(...) inside.")
		inner_p+ggplot2::coord_flip(...)+gb+L
	}
}

transform_raster_list=function(x, smooth=30, equal_scale=FALSE, raw_data=NULL, SPACE="rgb"){
    newx=plyr::llply(x, .fun=function(xx, smooth) if (length(xx)>=smooth ) xx else grDevices::colorRampPalette(xx, space=SPACE)(smooth), smooth=smooth)
    if (equal_scale==FALSE){
        newx
    } else {
        if (is.null(raw_data)) stop("When equal_scale is TRUE, raw_data must not be null.")
        if (all(raw_data==0)) stop("raw_data must have at least one non-zero value.")
        SUPERMIN=min(min(raw_data), 0)
        MAXMIN=max(0, max(raw_data))-SUPERMIN
		if (all(raw_data==0)) stop("At least one value should be non-zero value.")
        CONDITION=if(all(raw_data>=0)) 1 else if (all(raw_data<=0)) 2 else 3
        scAlEfUn_color=function(xx, smooth, supermin, maxmin, condition){
            if (condition==1){
                small=1
                big=ceiling(smooth*(xx-supermin)/maxmin)
				if (big==0) big=1
				if (big>smooth) big=smooth
            }
            if (condition==2){
                big=smooth
                small=floor(smooth*(xx-supermin)/maxmin)
                if (small == 0) small=1
            }
            if (condition==3){
                if (xx>=0){
					# bug fixed
                    small=ceiling(smooth*(0-supermin)/maxmin)
                    big=ceiling(smooth*(xx-supermin)/maxmin)
                } else {
                    small=floor(smooth*(xx-supermin)/maxmin)
                    if (small == 0) small=1
					# bug fixed
                    big=floor(smooth*(0-supermin)/maxmin)
                }
            }
            c(small, big)
        }
        color_index=plyr::llply(raw_data, .fun=scAlEfUn_color, smooth=smooth, supermin=SUPERMIN, maxmin=MAXMIN, condition=CONDITION)
        mapply(FUN=function(x, y) `[`(x, c(y[1]: y[2])), newx, color_index, SIMPLIFY=FALSE)
    }
}
