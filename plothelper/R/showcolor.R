#' Show a Color Palette
#' 
#' Simple function to show colors. NOTE: 
#' do not add \code{coord_flip()}.
#'
#' @param x a character vector of colors.
#' @param label_size size of text on 
#' x-axis to show color names.
#' @param ... other arguments passed to 
#' \code{geom_bar}.
#'
#' @export
#' @examples
#' # A palette used by David Hockney
#' co=c("#833822", "#C03800", "#D3454C", 
#' 	"#DC6A30", "#F29856", "#FEEF70", 
#' 	"#A5D56D", "#16D670", "#00932F", 
#' 	"#03592E", "#04B7B0", "#007BA9", 
#' 	"#EC46BF", "#6A2C8F"
#' )
#' showcolor(co, label_size=10)
showcolor=function(x, label_size=15, ...){
	lenx=length(x)
	withname_slash_n=add_slash_n(x, delete_space=FALSE, vertical_line=FALSE)
	withname_slash_n=paste(1: lenx, withname_slash_n, sep="\n \n")
	raw_name=paste(1: lenx, x)
	cat("The colors are: \n")
	for (i in raw_name) cat(i, "\n")
	ggplot2::ggplot()+
		ggplot2::geom_bar(show.legend=FALSE, stat="identity", aes(x=1: lenx, y=rep(1, lenx)), fill=x, ...)+
		ggplot2::scale_x_continuous(labels=withname_slash_n, breaks=1: lenx, expand=ggplot2::expansion(c(0, 0)))+
		ggplot2::scale_y_continuous(limits=c(0, 1), expand=ggplot2::expansion(c(0, 0)))+
		ggplot2::theme(
			panel.background=ggplot2::element_blank(), 
			panel.grid=ggplot2::element_blank(),
			axis.ticks=ggplot2::element_blank(),
			axis.title=ggplot2::element_blank(), 
			axis.text.y=ggplot2::element_blank(), 
			axis.text.x=ggplot2::element_text(size=label_size, lineheight=0.8)
		)
}

myvisbook=function(){
	message(
		"The current URL for downloading data, code and images of my book:  \nhttps://github.com/githubwwwjjj/visbook"
	)
}
