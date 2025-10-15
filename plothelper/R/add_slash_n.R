#' Adding Slash_n inside Strings
#' 
#' This function simply adds change-line
#' signs inside strings, so 
#' that they can be put vertically as the texts 
#' of x-axis.
#' 
#' @param x a character vector
#' @param delete_space whether to delete spaces.
#' Default is TRUE.
#' @param vertical_line whether to 
#' change \code{-} into \code{|}. 
#' Default is TRUE.
#' 
#' @export
#' @examples
#' lab=add_slash_n(c("a b-c", "d - ef ", "n"))
add_slash_n=function(x, delete_space=TRUE, vertical_line=TRUE){
	if (delete_space == TRUE) x=gsub(" ", "", x)
	x=gsub("", "\n", x)
	# nnn="\n"
	# x=gsub(paste("^", nnn, "|", nnn, "$", sep=""), "", x)
	x=gsub("^\n|\n$", "", x)
	if (vertical_line == TRUE) x=gsub("-", "\\|", x)
	x
}
