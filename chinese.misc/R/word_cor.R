#' Word Correlation in DTM/TDM
#'
#' Given a DTM/TDM/matrix, the function computes the pearson/spearman/kendall 
#' correlation between pairs of words and filters the values by p value and minimum value of correlation.
#' It is a little more flexible than \code{tm::findAssocs}. 
#'
#' @param x a DocumentTermMatrix, TermDocumentMatrix object, or a matrix. If it is a matrix, 
#' you must specify its type by the argument \code{type}. If it is a matrix, \code{NA} is not allowed, 
#' and rownames/colnames that are taken as words should not be \code{NULL}.
#' @param word a character vector of words that you want to know their correlation in you data. If 
#' it is not a vector, the function will try to coerce. The length of it should not larger than 200. The function
#' only computes for words that do exist in data, and those not in data will not be included.
#' @param type if it starts with "d/D", it represents a DTM; if with "t/T", TDM; others are not valid. This
#' is only used when x is a matrix. The default is "dtm".
#' @param method what index is to be computed? It can only be "pearson", "spearman", or "kendall"
#' (default). The method is passed to \code{stats::cor.test}. The default is "kendall".
#' @param p if the p value of a correlation index is >= this value, the index will be convert to \code{NA}
#' in the correlation matrix. The default is \code{NULL}, which means no filter is done.
#' Note: if both argument p and min are non-Null, their relation is "or" rather than "and".
#' @param min if the correlation index is smaller than this value, it will be convert to \code{NA}.
#' The default is \code{NULL}, which means no filter is done.
#'
#' @return a list. The 1st element is the correlation matrix with diagonal converted to \code{NA}. 
#' The 2nd element is the p value matrix with diagonal converted to \code{NA}.
#'
#' @import utils
#' @export
#' @examples
#' set.seed(1)
#' s <- sample(1:10, 100, replace = TRUE)
#' m <- matrix(s, nrow = 20)
#' myword<- c("alpha", "apple", "cake", "data", "r")
#' colnames(m) <- myword
#' mycor1 <- word_cor(m, myword)
#' mycor2 <- word_cor(m, myword, method = "pearson", min = 0.1, p = 0.4)
#' mt <- t(m)
#' mycor3 <- word_cor(mt, myword, type = "T", method = "spearman", p = 0.5)
word_cor <-
function(x, word, type = "dtm", method = "kendall", p = NULL, min = NULL){
    infolocale <- localestart2()
    on.exit(localeend2(infolocale))
	if (identical(class(x)[1], "DocumentTermMatrix")){
		truetype <- 1
		all_word <- x$dimnames$Terms
	} else if (identical(class(x)[1] , "TermDocumentMatrix")){
		all_word <- x$dimnames$Terms
		truetype <- 2
	} else if (identical(class(x)[1], "matrix")){
		if (!is_character_vector(type, len = 1))
			stop ("When x is matrix, type must tell me its type: dtm or tdm.")
		if (grepl("^d|^D", type)){
			truetype <- 3
			all_word <- colnames(x)
			if (is.null(all_word))
				stop ("colnames as words should not be NULL.")
		} else if (grepl("^t|^T", type)){
			truetype <- 4
			all_word <- rownames(x)
			if (is.null(all_word))
				stop("rownames as words should not be NULL.")
		} else {
			stop ("When x is matrix, type must tell me its type: dtm or tdm.")
		}
	} else {
		stop("x must be among DTM, TDM or matrix.")
	}
	stopifnot(method %in% c("pearson", "kendall", "spearman"))
	if (!is.null(p))
		stopifnot(is.numeric(p), length(p) == 1)
	if (!is.null(min))
		stopifnot(is.numeric(min), length(min) == 1)
	word <- as.character2(word)
	if (length(word) > 200)
		stop("Argument word allows input of not more than 200 words.")
	if (any(is.na(word)))
		stop("Argument word should not have NA.")
	all_word <- sort(all_word)
	word <- intersect(word, all_word)
	word <- sort(unique(word))
	if (length(word) < 2)
		stop("At least 2 words should be left.")
	pos <- match(word, all_word)
	if (truetype %in% c(1, 3)){
		x <- x[, pos]
	} else {
		x <- t(x[pos, ])
	}
	nc <- ncol(x)
	correlation <- matrix(NA, nrow = nc, ncol = nc)
	pvalue <- matrix(NA, nrow = nc, ncol = nc)
	if (truetype %in% c(3, 4)){
		for (i in 1: nc){
			for (j in 1: nc){
				if (i > j){
					cc <- stats::cor.test(x = x[, i], y = x[, j], method = method, exact = FALSE)
					cc1 <- round(cc$estimate, 4)
					correlation[i, j] <- cc1
					correlation[j, i] <- cc1
					cc2 <- round(cc$p.value, 4)
					pvalue[i, j] <- cc2 
					pvalue[j, i] <- cc2 
				}
			}
		}
	} else {
		for (i in 1: nc){
			ii <- as.numeric(as.matrix(x[, i]))
			for (j in 1: nc){
				jj <- as.numeric(as.matrix(x[, j]))
				if (i > j){
					cc <- stats::cor.test(x = ii, y = jj, method = method, exact = FALSE)
					cc1 <- round(cc$estimate, 4)
					correlation[i, j] <- cc1
					correlation[j, i] <- cc1
					cc2 <- round(cc$p.value, 4)
					pvalue[i, j] <- cc2 
					pvalue[j, i] <- cc2 
				}
			}
		}		
	}
	rownames(correlation) <- word
	colnames(correlation) <- word
	rownames(pvalue) <- word
	colnames(pvalue) <- word
	if (!is.null(p) & is.null(min)){
		correlation[pvalue >= p] <- NA
	}
	if (!is.null(min) & is.null(p)){
		correlation[abs(correlation) < min] <- NA
	}
	if (!is.null(min) & !is.null(p)){
		hidethis1 <- pvalue >= p
		hidethis2 <- abs(correlation) < min
		correlation[hidethis1 | hidethis2] <- NA
	}
	return(list(corMatrix = correlation, pMatrix = pvalue))
}
	