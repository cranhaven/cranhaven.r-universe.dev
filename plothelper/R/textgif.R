#' Simple Text ".gif" File
#'
#' This is a wrapper of functions in package
#' ggfittext and magick. The output is 
#' a ".gif" with changing texts and colors.
#' Characters are automatically enlarged or 
#' shrunk.
#' 
#' @param text must be a character vector.
#' @param text_color colors of the texts. Its 
#' length must be the same as that of 
#' \code{text}.
#' @param bg_color background color 
#' of the texts. It should have the same 
#' length as \code{text}.
#' @param reflow default is FALSE. If it is 
#' TRUE, \code{ggfittext::geom_fit_text} will 
#' automatically separate characters into 
#' several lines. However, you can separate 
#' lines manually by using line 
#' break.
#' @param width the width of the 
#' final gif object. Default is 200.
#' NOTE: how texts are adjusted in the text box 
#' depends on the values of width and height. 
#' @param height the height of the 
#' final gif object. Default is 100. 
#' @param family default is "SimHei" so that 
#' Chinese characters can be shown. However, 
#' some computers may not be able to use this 
#' family. And, this family ignores fontface.
#' For Latin words, the built-in families 
#' are "serif", "sans" and "mono", and more can 
#' be found by typing "?Hershey".
#' @param fontface 1 (default) for plain, 
#' 2 for bold, 3 for italic, 4 for bold italic.
#' @param fps the larger the faster. It should 
#' be a factor of 100, say, 2 (default), 4, 5, 10, 
#' rather than 3, 6, 7.
#' @param output if it is NULL (default), 
#' an object is created. Otherwise, object will not 
#' only be created but also be saved with a 
#' file name (".gif") represented by this argument.
#' @param ... extra arguments used by 
#' \code{ggfittext::geom_fit_text}, 
#' e. g., angle (0 to 360), lineheight.
#' 
#' @export
#' @import magick
#' @import ggfittext
#' @examples
#' \donttest{
#' mytext=c("AAA", "BBB", "CCC")
#' color1=c("orange", "red", "white")
#' color2=c("black", "blue", "green")
#' g1=textgif(mytext, text_color=color1, bg_color=color2, 
#' 	width=180, height=120, fps=2, family="serif")
#' }
textgif=function(text, text_color=NULL, bg_color=NULL, reflow=FALSE, width=200, height=100, family="SimHei", fontface=1, fps=2, output=NULL, ...){
	nt=length(text)
	if (is.null(text_color)) text_color=rep("black", nt)
	if (length(text_color)==1) text_color=rep(text_color, nt)
	if (is.null(bg_color)) bg_color=rep("white", nt)
	if (length(bg_color)==1) bg_color=rep(bg_color, nt)
	if (all.equal(length(text), length(text_color), length(bg_color)) != TRUE) 
		stop("text, text_color, bg_color must be of the same length or of length 1.")
	stopifnot(reflow %in% c(TRUE, FALSE))
	xmin=-width/2; xmax=width/2; ymin=-height/2; ymax=height/2
	huabu=magick::image_graph(width=width, height=height, res=72, bg="transparent", clip=TRUE) # MUST TRUE
	for (i in 1: nt){
		i_text_color=text_color[i]
		i_bg_color=bg_color[i]
		i_text=text[i]
	 	ip=ggplot2::ggplot()+
			ggplot2::coord_cartesian(expand=FALSE)+ # DO NOT USE coord_fixed
	 		ggplot2::theme_void()+
	 		ggplot2::theme(plot.background=ggplot2::element_rect(fill=i_bg_color, color=i_bg_color))+
	 		ggfittext::geom_fit_text(ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, label=i_text), grow=TRUE, reflow=reflow, family=family, fontface=fontface, color=i_text_color, ...)
		print(ip)
	}
	grDevices::dev.off()
	
	res=magick::image_animate(image=huabu, fps=fps, dispose="previous")
	if (is.null(output)){
		res 
	} else {
		magick::image_write(res, output, format="gif")	
		res
	}
}
