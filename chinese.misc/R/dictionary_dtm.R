#' Making DTM/TDM for Groups of Words
#'
#' A dictionary has several groups of words. Sometimes what we want is not the term frequency of this or that single word, 
#' but rather the total sum of words that belong to the same group. 
#' Given a dictionary, this function can save you a lot of time because 
#' it sums up the frequencies of all groups of words and you do not need to do it manually.
#'
#' The argument \code{dictionary} can be set in different ways:
#'
#' \itemize{
#'   \item (1) list: if it is a list, each element represents a group of words. The element should be a character vector; if it
#' is not, the function will manage to convert. However, the length of the element should be > 0 and has 
#' to contain at least 1 non-NA word.
#'   \item (2) matrix or data.frame: each entry of the input should be character; if it is not, the function will manage to convert.
#' At least one of the entries should not be \code{NA}. Each column (not row) represents a group of words.
#'   \item (3) character vector: it represents one group of words.
#'   \item (4) Note: you do not need to worry about two same words existing in the same group, because the function
#' will only count one of them. Neither should you worry about that the words in a certain group do not really
#' exist in the DTM/TDM, because the function will simply ignore those non-existent words. If none of the words 
#' of that group exists, the group will still appear in the final result, although the total frequencies of that group 
#' are all 0's. By setting \code{return_dictionary = TRUE}, you can see which words do exist.
#' }
#'
#' @param x an object of class DocumentTermMatrix or TermDocumentMatrix created by
#' \code{\link[chinese.misc]{corp_or_dtm}} or \code{tm::DocumentTermMatrix} or 
#' \code{tm::TermDocumentMatrix}. But it can also be a numeric matrix and you have to specify its type, 
#' see below.
#' @param dictionary a dictionary telling the function how you group the words. It can be a list, matrix, data.frame 
#' or character vector. Please see details for how to set this argument.
#' @param type if x is a matrix, you have to tell whether it represents a document term matrix or a term document 
#' matrix. Character starting with "D" or "d" for document term matrix, and that with "T" or "t" for term document 
#' matrix. The default is "dtm".
#' @param simple_sum if it is \code{FALSE} (default), a DTM/TDM will be returned. If \code{TRUE}, you will not 
#' see the term frequency of each word in each text. Rather, a numeric vector is returned, each of its element 
#' represents the sum of the corresponding group of words in the corpus as a whole.
#' @param return_dictionary if \code{TRUE}, a modified dictionary is returned, which only contains words that
#' do exist in the DTM/TDM. The default is \code{FALSE}.
#' @param checks The default is \code{TRUE}. This will check whether \code{x} and \code{dictionary} is valid.
#' For \code{dictionary}, if the input is not a list of characters, the function will manage to convert. You should not set 
#' this to \code{FALSE} unless you do believe that your input is OK.
#'
#' @return if \code{return_dictionary = FALSE}, an object of class DocumentTermMatrix or TermDocumentMatrix is 
#' returned; if \code{TRUE}, a list is returned, the 1st element is the DTM/TDM, and the 2nd
#' element is a named list of words. However, if \code{simple_sum = TRUE}, the DTM/TDM in the above two 
#' situations will be replaced by a vector.
#'
#' @export
#' @examples
#' x <- c(
#'   "Hello, what do you want to drink and eat?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water", 
#'   "eat a cake", 
#'   "eat a piece of pizza"
#' )
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' D1 <- list(
#'   aa <- c("drink", "eat"),
#'   bb <- c("cake", "pizza"),
#'   cc <- c("cup", "bottle")
#' )
#' y1 <- dictionary_dtm(dtm, D1, return_dictionary = TRUE)
#' #
#' # NA, duplicated words, non-existent words, 
#' # non-character elements do not affect the
#' # result.
#' D2 <-list(
#'   has_na <- c("drink", "eat", NA),
#'   this_is_factor <- factor(c("cake", "pizza")),
#'   this_is_duplicated <- c("cup", "bottle", "cup", "bottle"), 
#'   do_not_exist <- c("tiger", "dream")
#' )
#' y2 <- dictionary_dtm(dtm, D2, return_dictionary = TRUE)
#' #
#' # You can read into a data.frame 
#' # dictionary from a csv file.
#' # Each column represents a group.
#' D3 <- data.frame(
#'   aa <- c("drink", "eat", NA, NA),
#'   bb <- c("cake", "pizza", NA, NA),
#'   cc <- c("cup", "bottle", NA, NA),
#'   dd <- c("do", "to", "of", "and")
#' )
#' y3 <- dictionary_dtm(dtm, D3, simple_sum = TRUE)
#' #
#' # If it is a matrix:
#' mt <- t(as.matrix(dtm))
#' y4 <- dictionary_dtm(mt, D3, type = "t", return_dictionary = TRUE)
dictionary_dtm <- function(x, dictionary, type = "dtm", simple_sum = FALSE, return_dictionary = FALSE, checks = TRUE){
    infolocale <- localestart2()
    on.exit(localeend2(infolocale))
	message("CHECKING ARGUMENTS")
	stopifnot(checks %in% c(FALSE, TRUE))
	stopifnot(simple_sum %in% c(FALSE, TRUE))
	stopifnot(return_dictionary %in% c(FALSE, TRUE))	
	dic_class <- class(dictionary)[1]
	if (!dic_class %in% c("data.frame", "list", "matrix", "character"))
		stop("Argument dictionary must be a list, data.frame, matrix or character vector.")
	if (dic_class == "data.frame"){
		dictionary <- as.list(dictionary)
	} else if (dic_class == "matrix"){
		dictionary <- as.list(data.frame(dictionary, stringsAsFactors = FALSE))
	} else if (dic_class == "character"){
		dictionary <- list(c(dictionary))
	}
	dic_len <- length(dictionary)
	stopifnot(length(dictionary) > 0)
	if (checks){
		for (i in 1: dic_len){
			each_of_dic <- dictionary[[i]]
			initial_i_class <- class(each_of_dic)[1]
			if (initial_i_class != "character"){
				each_of_dic <- as.character2(each_of_dic)
			}
			len_dic_i <- length(each_of_dic)
			if (length(len_dic_i) == 0){
				stop(paste("Group ", i, " is NULL or contains no valid word.", sep=""))
			}
			len_which_is_na <- length(which(is.na(each_of_dic)))
			if (len_which_is_na > 0){
				message("---found NA in group ", i)
			}
			# check length after removing NA
			if (len_which_is_na == len_dic_i){
				stop(paste("Group ", i, " is NULL or contains no valid word.", sep=""))
			}
			if (initial_i_class != "character"){
				dictionary[[i]] <- each_of_dic
			}			
		}
	}	
	ori_class <- class(x)[1]
	if (identical(ori_class, "DocumentTermMatrix")){
		truetype <- 1
		all_word <- x$dimnames$Terms
		fullname <- x$dimnames$Docs
	} else if (identical(ori_class, "TermDocumentMatrix")){
		truetype <- 2
		all_word <- x$dimnames$Terms
		fullname <- x$dimnames$Docs
		x <- t(x)
	} else if (identical(ori_class, "matrix")){
		if (!is_character_vector(type, len = 1))
			stop ("When x is matrix, type must tell me its type: dtm or tdm.")
		if (checks){
			if (any(is.na(x)))
				stop("The matrix must not have NA.")
			if (!is.numeric(x))
				stop("The matrix must be numeric.")
			if (any(x < 0))
				stop("Values in x must not be negative.")
			if (nrow(x) == 0 | ncol(x) == 0)
				stop("x must have at least 1 row and 1 column.")
		}
		if (grepl("^d|^D", type)){
			truetype <- 3
			all_word <- colnames(x)
			if (is.null(all_word))
				stop ("colnames as words should not be NULL.")
			fullname <- rownames(x)
			if (is.null(fullname))
				fullname <- as.character(1: nrow(x))
			x <- slam::as.simple_triplet_matrix(x)
		} else if (grepl("^t|^T", type)){
			truetype <- 4
			all_word <- rownames(x)
			if (is.null(all_word))
				stop("rownames as words should not be NULL.")
			fullname <- colnames(x)
			if (is.null(fullname))
				fullname <- as.character(1: ncol(x))
			x <- t(slam::as.simple_triplet_matrix(x))	
		} else {
			stop ("When x is matrix, type must tell me its type: dtm or tdm.")
		}
	} else {
		stop("x must be a DTM, TDM or matrix.")
	}
	message("COMPUTING")
	stm_nrow <- nrow(x)
	summary_stm <- slam::simple_triplet_matrix(i = 1, j = 1, v = 1, nrow = stm_nrow, ncol = dic_len, dimnames = list(Terms = NULL, Docs = NULL))
	for (j in 1: dic_len){
		j_dic <- unique(dictionary[[j]])
		j_dic <- j_dic[!is.na(j_dic)]
		j_dic <- intersect(j_dic, all_word)
		if (length(j_dic) == 0){
			summary_stm[, j] <- rep(0, stm_nrow)
			if (return_dictionary)
				dictionary[[j]] <- character(0)
		} else {
			pos <- match(j_dic, all_word)
			j_stm <- x[, pos]
			j_stm <- slam::row_sums(j_stm)
			summary_stm[, j] <- j_stm
			if (return_dictionary)
				dictionary[[j]] <- j_dic
		}
	}
	if (truetype %in% c(1, 3)){
		message("MAKING DTM")
		summary_stm <- tm::as.DocumentTermMatrix(summary_stm, weighting=tm::weightTf)
	} else {
		message("MAKING TDM")
		summary_stm <- t(tm::as.DocumentTermMatrix(summary_stm, weighting=tm::weightTf))
	}
	dic_code <- zero_paste(1: dic_len)
	dic_group_name <- paste("group", dic_code, sep = "")
	## must first sum up and then give dimnames; otherwise, row_sums and col_sums does not work
	summary_stm$dimnames$Terms <- dic_group_name
	if (simple_sum){
		if (truetype %in% c(1, 3)){ 
			summary_stm <- slam::col_sums(summary_stm)
		} else {
			summary_stm <- slam::row_sums(summary_stm)
		}
		names(summary_stm) <- dic_group_name
	} else {
		summary_stm$dimnames$Docs <- fullname
	}
	message("DONE")	
	if (!return_dictionary){
		return(summary_stm)
	} else {
		names(dictionary) <- dic_group_name
		return(list(freq = summary_stm, used_dictionary = dictionary))
	}
}
