#' Cut out a Subregion of an Image by Mouse Click
#' 
#' This function is a wrapper of 
#' \code{magick::image_crop}. While the 
#' latter asks you to set a \code{geometry} 
#' parameter, this function enables you 
#' to set the four sides of a subregion only 
#' by click the mouse. You must click at 
#' least 2 times (that is, click on 2 different
#' points to define a rectangle). After clicking, 
#' please press Esc on your keyboard. You 
#' can also designate an irregular polygon 
#' by mouse with at least 3 clicks. If it is 
#' irregular, you MUST click on positions 
#' in order (something like that, when you 
#' draw a polygon in R, you must input 
#' the positions of points in order).
#' 
#' @param x an image read into R by 
#' \code{magick::image_read} or an image  
#' modified by functions in the magick 
#' package.
#' @param only_value the default is FALSE, 
#' which will return the subregion. If you set it to 
#' TRUE, the result is only four values with 
#' the order: left, right, top, bottom.
#' @param rectangle whether the cropped 
#' area is a rectangle (default is TRUE). If 
#' it is FALSE, the subregion can be irregular. 
#' @param trim this is only used when 
#' \code{rectangle} is FALSE. It decides whether 
#' the irregular subregion is to be trimmed. If it 
#' is FALSE (default), no trimming will be done. 
#' If it is a 0 to 100 value, 
#' \code{magick::image_trim} will be used, whose 
#' \code{fuzz} argument is equal to \code{trim}. 
#' If it is TRUE (not 1), trimming will be done 
#' according to the mouse click you have made.
#' 
#' @export
image_crop_click=function(x, only_value=FALSE, rectangle=TRUE, trim=FALSE){
	if (grDevices::dev.capabilities()$locator == FALSE) stop("Your device does not support mouse locator.")
	y=grDevices::as.raster(x)
	omar=graphics::par()$mar
	graphics::par(mar=rep(0.5, 4))
	on.exit(graphics::par(mar=omar))
	width=dim(y); height=width[1]; width=width[2]
	graphics::plot(y)
	clicking=graphics::locator()
	
	### rectangle
	if (rectangle==TRUE){
		if (length(clicking$x) < 2) stop("You must click at least 2 times.")
		xrng=ceiling(range(clicking$x))
		yrng=ceiling(range(clicking$y))
		xrng=MAKE_IN_BORDER(xrng, SMALL=1, BIG=width)
		yrng=MAKE_IN_BORDER(yrng, SMALL=1, BIG=height)
		top=height+1-yrng[2]
		bottom=height+1-yrng[1]
		res=c(left=xrng[1], right=xrng[2], top=top, bottom=bottom)
		cat(paste("The area is: \n", "left: ", res[1], "\n", "right: ", res[2], "\n", "top: ", res[3], "\n", "bottom: ", res[4], "\n", sep=""))
		if (only_value==TRUE){
			return(res)
		} else {
			cha=paste(res[2]-res[1]+1, "x", res[4]-res[3]+1, "+", res[1]-1, "+", res[3]-1, sep="")
			res=magick::image_crop(x, geometry=cha)
			return(res)
		}
	}
	
	### non-rectangle
	if (rectangle==FALSE){
		if (length(clicking$x) < 3) stop("For non-rectangle, you must click at least 3 times.")
		if (only_value==TRUE){
			## only output position value
			xpos=ceiling(clicking$x)
			ypos=ceiling(clicking$y)
			xpos=MAKE_IN_BORDER(xpos, SMALL=1, BIG=width)
			ypos=MAKE_IN_BORDER(ypos, SMALL=1, BIG=height)	
			yposADJ=height+1-ypos
			return(list(x=xpos, y=yposADJ))	
		} else {
			## output image
			xpos=clicking$x
			ypos=clicking$y
			if (identical(trim, TRUE)){
				## prepare for trim
				xrng=ceiling(range(xpos))
				yrng=ceiling(range(ypos))
				xrng=MAKE_IN_BORDER(xrng, SMALL=1, BIG=width)
				yrng=MAKE_IN_BORDER(yrng, SMALL=1, BIG=height)			
				trim_geometry=paste(diff(xrng)+1, "x", diff(yrng)+1, "+", xrng[1]-1, "+", height-yrng[2], sep="")
			}
			xpos=MAKE_IN_BORDER(xpos, SMALL=0, BIG=width)
			ypos=MAKE_IN_BORDER(ypos, SMALL=0, BIG=height)
			xpos=scales::rescale(c(0, width, xpos), to=c(0, 1))[-c(1, 2)]
			ypos=scales::rescale(c(0, height, ypos), to=c(0, 1))[-c(1, 2)]
			poly=grid::polygonGrob(x=xpos, y=ypos, gp=gpar(fill="black"))
			gra=magick::image_graph(width=width, height=height, bg="transparent")
			grid::grid.newpage()
			grid::grid.draw(poly)
			grDevices::dev.off()
			res=magick::image_composite(gra, x, operator="in")
			
			if (is.numeric(trim)){
				res=magick::image_trim(res, fuzz=trim)
			} else if (identical(trim, TRUE)){
				res=magick::image_crop(res, geometry=trim_geometry)
			}
			return(res)
		}
	}
}

MAKE_IN_BORDER=function(X, SMALL, BIG){
	X[which(X<SMALL)]=SMALL
	X[which(X>BIG)]=BIG
	X
}
