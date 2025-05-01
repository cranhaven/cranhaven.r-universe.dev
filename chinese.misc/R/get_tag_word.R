#' Extract Words of Some Certain Tags through Pos-Tagging
#'
#' Given a group of Chinese texts, this function manages to extract words of some specified types. For example, sometimes
#' you want to collect all verbs that are used in your texts. Note: this function uses \code{jiebaR::tagging} to segment
#' texts and do pos-tagging. The types assigned are not all correct. So, alternatively, you can first pos-tag your texts with
#' other methods and then use this function.
#'
#' The Argument each and only_unique decide what kind of return you can get.
#' \itemize{
#' 	\item if \code{each = TRUE} and \code{only_unique = FALSE}, you can get a list, each element of which 
#' contains words extracted. This is the default.
#' 	\item if \code{each = TRUE} and \code{only_unique = TRUE}, each element of the list only contains unique words.
#' 	\item if \code{each = FALSE} and \code{only_unique = FALSE}, all words extracted will be put into a single vector.
#' 	\item if \code{each = FALSE} and \code{only_unique = TRUE}, words extracted will be put into a single vector, but 
#' only unique words will be returned.
#' }
#'
#' @param x it must be a list of character vectors, even when the list contains only one element. 
#' Each element of the list is either a length 1 character vector of a text, or 
#' a length >= 1 character vector which is the result of former tagging work. It should not contain \code{NA}.
#' @param tag one or more tags should be specified. Words with these tags will be chosen. Possible tags are "v", "n", 
#' "vn", etc.
#' @param tag_pattern should be a length 1 regular expression. You can specify tags by this pattern rather than directly 
#' provide tag names. For example, you can specify tag names starting with "n" by \code{tag_pattern = "^n"}.
#' At least and at most one of tag and tag_pattern should be \code{NULL}.
#' @param mycutter a cutter created with package jiebaR and
#' given by users to tag texts. If your texts have already been pos-tagged, you
#' can set this to \code{NULL}.
#' By default, a \code{DEFAULT_cutter} is used, which is 
#' assigned as \code{worker(write = FALSE)} when loading the package. 
#' @param type if it is "word" (default), then extract the words that match your tags. If it is "position", only the positions
#' of the words are returned. Note: if it is "positions", argument \code{each} (see below) will always be set to \code{TRUE}.
#' @param each if this is \code{TRUE} (default), the return will be a list, each element of which is a extraction result of a text.
#' If it is \code{FALSE}, the return will be a character vector with extracted words. See detail.
#' @param only_unique if it is \code{TRUE}, only unique words are returned. The default is \code{FALSE}. See detail. 
#' @param keep_name whether to keep the tag names of the extracted words. The default is \code{FALSE}. Note: if 
#' \code{only_unique = TRUE}, all tag names will be removed.
#' @param checks whether to check the correctness of arguments. The default is \code{TRUE}.
#' 
#' @export
#' @examples
#' # No Chinese, so use English instead.
#' x1 <- c(v = "drink", xdrink = "coffee", v = "drink", xdrink = "cola", v = "eat", xfood = "banana")
#' x2 <- c(v = "drink", xdrink = "tea", v = "buy", x = "computer")
#' x <- list(x1, x2)
#' get_tag_word(x, tag = "v", mycutter = NULL)
#' get_tag_word(x, tag = "v", mycutter = NULL, only_unique = TRUE)
#' get_tag_word(x, tag_pattern = "^x", mycutter = NULL)
#' get_tag_word(x, tag_pattern = "^x", mycutter = NULL, keep_name = TRUE)
#' get_tag_word(x, tag = "v", mycutter = NULL, each = FALSE)
#' get_tag_word(x, tag = "v", mycutter = NULL, each = FALSE, only_unique = TRUE)
#' get_tag_word(x, tag = "v", mycutter = NULL, type = "position")
get_tag_word <- function(x, tag = NULL, tag_pattern = NULL, mycutter = DEFAULT_cutter, type = "word", each = TRUE, only_unique = FALSE, keep_name = FALSE, checks = TRUE){
	if (checks == TRUE){
		message("CHECKING ARGUMENTS")
		stopifnot(is.list(x))
		stopifnot(length(x) > 0)
		if (!is.null(mycutter)){
			stopifnot(class(mycutter)[1] == "jiebar")
		} else {
			if (is.list(x)){
				for (i in 1: length(x)){
					if (length(x[[i]]) < 1){
						stop("Length of element ", i, " is 0.")
					}
				}
			}
		}
		stopifnot(identical(type, "word") | identical(type, "position"))
		stopifnot(each %in% c(TRUE, FALSE))
		stopifnot(only_unique %in% c(TRUE, FALSE))
		stopifnot(keep_name %in% c(TRUE, FALSE))
		if (keep_name == TRUE & only_unique == TRUE)
			message("NOTE: when only_unique is TRUE, no names will be kept even when keep_name is TRUE.")
		only_1 <- sum(identical(tag, NULL), identical(tag_pattern, NULL))
		if (only_1 != 1)
			stop("At least and at most one of tag and tag_pattern should be NULL.")
	}		
	if (!is.null(mycutter)){
		x <- lapply(x, jiebaR::tagging, jiebar = mycutter)
	}
	if (!is.null(tag)){
		message("EXTRACTING BY TAG")
		extract_as_list <- lapply(x, tag_which, want = tag)
	}
	if (!is.null(tag_pattern)){
		message("EXTRACTING BY TAG PATTERN")		
		extract_as_list <- lapply(x, tag_grepl, pp = tag_pattern)
	}
	if(type == "position"){
		message("DONE")
		return(extract_as_list)
	} else {
		extract_as_list <- mapply("[", x, extract_as_list, SIMPLIFY = FALSE)
		if (only_unique){
			extract_as_list <- lapply(extract_as_list, unique)
		}
		if (only_unique == FALSE & keep_name == FALSE){
			extract_as_list <- lapply(extract_as_list, "names<-", value = NULL)
		}
		if (!each){
			extract_as_list <- unlist(extract_as_list)
			if (only_unique){
				extract_as_list <- unique(extract_as_list)
			}
		}
		message("DONE")
		return(extract_as_list)
	}	
}

tag_grepl <- function(xx, pp){
	xxn <- names(xx)
	y <- grepl(pp, xxn)
	which(y == TRUE)
}
		
tag_which <- function(xx, want){
	xxn <- names(xx)
	which(xxn %in% want)
}
