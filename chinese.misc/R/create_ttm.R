#' Create Term-Term Matrix (Term-Cooccurrence Matrix)
#'
#' This is a convenient function to create term-term matrix from document-term matrix, term-document
#' matrix, or a matrix that represents one of the two. Sparse matrix is used  
#' to speed up computing.
#' The output can be either a matrix or a sparse matrix.
#'
#' @param x an object of class DocumentTermMatrix or TermDocumentMatrix, or a matrix which has
#' its rownames or colnames as terms.
#' @param type if \code{x} is a matrix, this argument tells whether it is a DTM or a TDM; for the former, 
#' a character starting with "D/d", and for the latter, starting with "T/t".
#' @param tomatrix should be logical, whether to output a matrix result. If \code{TRUE}, a matrix
#' representing a TTM is returned. If \code{FALSE} (default), a list is returned: the first element is 
#' a sparse matrix created by package Matrix, with no words, the second element is a character 
#' vector of these words.
#' @param checks if \code{x} is a matrix, whether to check its validity, that is, whether it is numeric, all 
#' values are 0 or positive, there is no \code{NA}.
#'
#' @export
#' @examples
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' ttm1 <- create_ttm(dtm)
#' ttm2 <- create_ttm(dtm, tomatrix = TRUE)
#' tdm <- t(dtm)
#' ttm3 <- create_ttm(tdm)
#' ttm_sparse <- ttm3[[1]]
#' ttm_ordinary <- as.matrix(ttm_sparse)
#' colnames(ttm_ordinary) <- ttm3[[2]]
#' rownames(ttm_ordinary) <- ttm3[[2]]
#' # You can also use Matrix::writeMM(ttm_sparse, filename) 
#' # to write it on your disk.
create_ttm <-
function(x, type = "dtm", tomatrix = FALSE, checks = TRUE){
    infolocale <- localestart2()
    on.exit(localeend2(infolocale))
	stopifnot(tomatrix %in% c(FALSE, TRUE))
	stopifnot(checks %in% c(FALSE, TRUE))
	ori_class <- class(x)[1]
	if (identical(ori_class, "DocumentTermMatrix")){
		truetype <- 1
		all_word <- x$dimnames$Terms
	} else if (identical(ori_class, "TermDocumentMatrix")){
		all_word <- x$dimnames$Terms
		truetype <- 2
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
		}
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
	if (nrow(x) < 3 | ncol(x) < 3)
		stop("Word number and text number must at least be 3 !")
	x[x > 1] <- 1
	if (truetype == 3){
		x <- Matrix::Matrix(x, sparse = TRUE)
		x <- Matrix::crossprod(x)
	}
	if (truetype == 4){
		x <- Matrix::Matrix(x, sparse = TRUE)
		x <- Matrix::tcrossprod(x)
	}
	if (truetype == 1){
		x <- Matrix::sparseMatrix(i = x$i, j = x$j, x=x$v, dims = c(x$nrow, x$ncol))
		x <- Matrix::crossprod(x)
	}
	if (truetype == 2){
		x <- Matrix::sparseMatrix(i = x$i, j = x$j, x=x$v, dims = c(x$nrow, x$ncol))
		x <- Matrix::tcrossprod(x)
	}
	if (!tomatrix){
		x <- Matrix::tril(x, k = -1)
		colnames(x) <- NULL
		rownames(x) <- NULL
		return(list(value = x, word = all_word))
	} else {
		x <- as.matrix(x)
		colnames(x) <- all_word
		rownames(x) <- all_word
		return(x)
	}
}	
	