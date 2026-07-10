
##' Set locale to Simplified Chinese/Traditional Chinese/UK.
##' 
##' @title Set locale to Simplified Chinese/Traditional Chinese/UK.
##' @aliases setcht setuk
##' @usage
##' setchs(rev = FALSE)
##' setcht(rev = FALSE)
##' setuk(rev = FALSE)
##' @param rev Whethet to set the locale back.
##' @return No results.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @examples
##' setchs()
##' setchs(rev = TRUE)

setchs <- function(rev = FALSE)
{
	if (identical(rev, FALSE)) {
		Sys.setlocale(category = "LC_CTYPE", locale = "chs")
	} else {
		Sys.setlocale(category = "LC_CTYPE", locale = getOption("tmcn.oldlocale"))
	}
}

setcht <- function(rev = FALSE)
{
	if (identical(rev, FALSE)) {
		Sys.setlocale(category = "LC_CTYPE", locale = "cht")
	} else {
		Sys.setlocale(category = "LC_CTYPE", locale = getOption("tmcn.oldlocale"))
	}
}


setuk <- function(rev = FALSE)
{
	if (identical(rev, FALSE)) {
		Sys.setlocale(category = "LC_CTYPE", locale = "uk")
	} else {
		Sys.setlocale(category = "LC_CTYPE", locale = getOption("tmcn.oldlocale"))
	}
}


