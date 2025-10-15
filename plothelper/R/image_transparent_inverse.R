#' Keep Certain Colors Unchanged
#' and Make Others Transparent
#' 
#' This function is an inverse version of 
#' \code{magick::image_transparent}.
#' While the latter makes certain colors 
#' transparent, the former keeps them 
#' unchanged and make others transparent.
#' 
#' @param x a magick image.
#' @param color one or more colors you want 
#' want to keep unchanged.
#' @param fuzz color tolerance between 0 and 100.
#' Its length must be 1 or the same as \code{color}
#'  (which 
#' means you can have different fuzz values for 
#' different colors).
#' Suppose your color is white. If fuzz=0, then only 
#' white will be kept unchanged; if fuzz=10, colors 
#' similar to white will also kept unchanged.
#' 
#' @export
image_transparent_inverse=function(x, color, fuzz=0){
# 	magick::image_composite(
# 		magick::image_transparent(x, color=color, fuzz=fuzz), 
# 		x, operator="out"
# 	)
	fuzz=rep_len(fuzz, length(color))
	shade=x
	for (i in 1: length(color)){
		icolor=color[i]
		ifuzz=fuzz[i]
		shade=magick::image_transparent(shade, color=icolor, fuzz=ifuzz)
	}
	magick::image_composite(shade, x, operator="out")
}
