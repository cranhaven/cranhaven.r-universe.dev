#' Convert Objects among matrix, dgCMatrix, simple_triplet_matrix, 
#' DocumentTermMatrix, TermDocumentMatrix
#'
#' This is to convert objects conveniently. The three types of matrix are 
#' 1st, "matrix"; 2nd, "dgCMatrix" in package Matrix; 3rd, 
#' "simple_triplet_matrix", "DocumentTermMatrix", "TermDocumentMatrix" 
#' in package slam, tm. 
#' This function is to be used when you read a csv file and 
#' want it to be a dtm; or, when you have a very large dtm and you want 
#' it to be saved or passed to another function that deals with
#' dgCMatrix object. Note, it cannot convert between simple_triplet_matrix
#' on one side, and dtm or tdm on the other.
#' 
#' @param x object of class matrix, dgCMatrix, simple_triplet_matrix, 
#' DocumentTermMatrix, TermDocumentMatrix.
#' @param to to what class do you want to convert \code{x} to. Abbreviations 
#' can be used: "matrix" and "m" mean "matrix": "dgCMatrix" and "M" 
#' mean "dgCMatrix"; "simple_triplet_matrix" and "stm" mean 
#' "simple_triplet_matrix"; "DocumentTermMatrix", "dtm", "DTM" 
#' mean "DocumentTermMatrix"; "TermDocumentMatrix", "tdm", "TDM" 
#' mean "TermDocumentMatrix".
#' @param keep_name whether to keep names or 
#' dimnames, which are, for dtm-like object, 
#' documents and terms. \code{TRUE} by default. 
#' If you set it to \code{FALSE}, 
#' you will lose them. But if you convert dgCMatrix to 
#' dtm or tdm, it is required that the dgCMatrix object
#' has a list of length 2 as dimnames.
#' 
#' @return the object whose class is specified by argument \code{to}.
#' 
#' @export
#' @examples
#' # Make a matrix and convert to a dtm
#' m <- sample(0: 1, 50, replace = TRUE)
#' m <- matrix(m, nrow = 5)
#' colnames(m) <- letters[1: 10]
#' rownames(m) <- as.character(1: 5)
#' dtm <- m3m(m, "dtm")
#' # Convert dtm to dgCMatrix
#' M <- m3m(dtm, "M")
m3m <- function(x, to, keep_name = TRUE){
	INFOLOCALE <- localestart2()
	on.exit(localeend2(INFOLOCALE))
	xclass <- class(x)
	if ("DocumentTermMatrix" %in% xclass){
		xtype <- "DocumentTermMatrix"
	} else if ("TermDocumentMatrix" %in% xclass){
		xtype <- "TermDocumentMatrix"
	} else if (xclass[1] == "simple_triplet_matrix"){
		xtype <- "simple_triplet_matrix"
	} else if (xclass[1] == "matrix"){
		xtype <- "matrix"
	} else if (xclass[1] == "dgCMatrix"){
		xtype <- "dgCMatrix"
	} else if (xclass[1] == "data.frame"){
		stop("Please first convert x into a data matrix.")
	} else {
		stop("x must be of class matrix, DocumentTermMatrix, TermDocumentMatrix, simple_triplet_matrix or dgCMatrix.")
	}
	stopifnot(identical(keep_name, TRUE) | identical(keep_name, FALSE))
	if (! is_character_vector(to, len = 1)) stop("to must be a class name of length 1.")
	if (to %in% c("matrix", "m")){
		to <- "matrix"
	} else if (to %in% c("dgCMatrix", "M")){
		to <- "dgCMatrix"
	} else if (to %in% c("simple_triplet_matrix", "stm")){
		to <-"simple_triplet_matrix"
	} else if (to %in% c("DocumentTermMatrix", "dtm", "DTM")){
		to <- "DocumentTermMatrix"
	} else if (to %in% c("TermDocumentMatrix", "tdm", "TDM")){
		to <- "TermDocumentMatrix"
	} else {
		stop("to must be valid, see the above message.")
	}
	if (xtype == to) {
		stop ("Convert within the same class is meaningless.")
	}
	# dtm, tdm, stm------matrix
	if (xtype %in% c("TermDocumentMatrix", "DocumentTermMatrix") & to == "matrix") {
		x <- as.matrix(x)
		if (keep_name == FALSE){
			dimnames(x) <- NULL
		}
		x
	} else 
	if(xtype == "simple_triplet_matrix" & to == "matrix"){
		if (! length(x$dimnames) %in% c(0, 2)){
			stop("When transforming simple_triplet_matrix, dimnames must either be of length 0 or of length 2.")
		}
		x <- as.matrix(x)
		if (keep_name == FALSE){
			dimnames(x) <- NULL
		}
		x
	} else 
	# matrix------dtm, tdm, stm
	if (xtype == "matrix" & to =="simple_triplet_matrix"){
		if (keep_name == FALSE){
			dimnames(x) <- NULL
		}
		x <- slam::as.simple_triplet_matrix(x)
		x
	} else 
	if (xtype == "matrix" & to == "DocumentTermMatrix"){
		xrn <- rownames(x)
		if (is.null(xrn)) xrn <- as.character(1: nrow(x))
		xcn <- colnames(x)
		if (is.null(xcn)) xcn <- as.character(1: ncol(x))
		dimnames(x) <- NULL
		x <- slam::as.simple_triplet_matrix(x)
		x$dimnames$Docs <- xrn
		x$dimnames$Terms <- xcn
		x <- tm::as.DocumentTermMatrix(x, weighting = tm::weightTf)
		x
	} else 
	if (xtype == "matrix" & to == "TermDocumentMatrix"){
		xrn <- rownames(x)
		if (is.null(xrn)) xrn <- as.character(1: nrow(x))
		xcn <- colnames(x)
		if (is.null(xcn)) xcn <- as.character(1: ncol(x))
		dimnames(x) <- NULL
		x <- slam::as.simple_triplet_matrix(x)
		x$dimnames$Terms <- xrn
		x$dimnames$Docs <- xcn
		x <- tm::as.TermDocumentMatrix(x, weighting = tm::weightTf)
		x
	} else 
	# matrix------dgCMatrix
	if (xtype == "matrix" & to == "dgCMatrix"){
		if (keep_name == FALSE){
			dimnames(x) <- NULL
		}
		x <- Matrix::Matrix(x)
		x
	} else 
	# dgCMatrix------matrix
	if (xtype == "dgCMatrix" & to == "matrix"){
		if (keep_name == FALSE){
			dim_spa <- list(NULL, NULL)
		} else {
			if (length(x@Dimnames) == 2){
				dim_spa <- x@Dimnames
			} else {
				stop("When converting dgCMatrix, length of dimnames must be 2.")
			}
		}	
		x <- as.matrix(x)
		dimnames(x) <- dim_spa
		x
	} else 
	# dtm, tdm, stm------M
	if (xtype == "DocumentTermMatrix" & to == "dgCMatrix"){
		if (keep_name == TRUE){
			dim_spa <- list(Docs = x$dimnames$Docs, Terms = x$dimnames$Terms)
		} else {
			dim_spa <- list(NULL, NULL)
		}
		x <- Matrix::sparseMatrix(i = x$i, j = x$j, x = x$v, dims = c(nrow(x), ncol(x)), dimnames = dim_spa)
		x
	} else 
	if (xtype == "TermDocumentMatrix" & to == "dgCMatrix"){
		if (keep_name == TRUE){
			dim_spa <- list(Terms = x$dimnames$Terms, Docs = x$dimnames$Docs)
		} else {
			dim_spa <- list(NULL, NULL)
		}
		x <- Matrix::sparseMatrix(i = x$i, j = x$j, x = x$v, dims = c(nrow(x), ncol(x)), dimnames = dim_spa)
		x
	} else 
	if (xtype == "simple_triplet_matrix" & to == "dgCMatrix"){
		if (keep_name == TRUE){
			lenxdim <- length(x$dimnames)
			if (lenxdim == 0){
				dim_spa <- list(NULL, NULL)
			} else if (lenxdim == 2){
				dim_spa <- list(x$dimnames[[1]], x$dimnames[[2]])
			} else {
				stop("When converting simple_triplet_matrix to dgCMatrix, length of dimnames must be of 0 or 2.")
			}
		} else {
			dim_spa <- list(NULL, NULL)
		}
		x <- Matrix::sparseMatrix(i = x$i, j = x$j, x = x$v, dims = c(nrow(x), ncol(x)), dimnames = dim_spa)
		x
	} else 
	# dgCMatrix to stm dtm tdm
	if (xtype == "dgCMatrix" & to == "simple_triplet_matrix"){
		if (keep_name == FALSE){
			dim_spa <- list(NULL, NULL)
		} else {
			if (length(x@Dimnames) == 2){
				dim_spa <- x@Dimnames
			} else {
				stop("When converting dgCMatrix, length of dimnames must be 2.")
			}
		}	
		pp <- rep(seq_along(diff(x@p)), diff(x@p))
		x <- slam::simple_triplet_matrix(i = (x@i)+1, j = pp, v = x@x,
				nrow = x@Dim[1], ncol = x@Dim[2], 
				dimnames = dim_spa
				)
		x
	} else 
	if (xtype == "dgCMatrix" & to == "DocumentTermMatrix"){
		if (! length(x@Dimnames) == 2){
			stop("When converting dgCMatrix, length of dimnames must be 2.")
		}
		if (! length(x@Dimnames[[1]]) == x@Dim[1] | ! length(x@Dimnames[[2]]) == x@Dim[2]){
			stop("1st element in Dimnames must be equal to row number, and 2nd to the col number.")
		}
		pp <- rep(seq_along(diff(x@p)), diff(x@p))
		x <- simple_triplet_matrix(i = (x@i)+1, j = pp, v = x@x,
				nrow = x@Dim[1], ncol = x@Dim[2], 
				dimnames = list(Docs = x@Dimnames[[1]], Terms=x@Dimnames[[2]])
				)
		x <- tm::as.DocumentTermMatrix(x, weighting = tm::weightTf)
		x
	} else 
	if (xtype == "dgCMatrix" & to == "TermDocumentMatrix"){
		if (! length(x@Dimnames) == 2){
			stop("When converting dgCMatrix, length of dimnames must be 2.")
		}	
		if (! length(x@Dimnames[[1]]) == x@Dim[1] | ! length(x@Dimnames[[2]]) == x@Dim[2]){
			stop("1st element in Dimnames must be equal to row number, and 2nd to the col number.")
		}
		pp <- rep(seq_along(diff(x@p)), diff(x@p))
		x <- slam::simple_triplet_matrix(i = (x@i)+1, j = pp, v = x@x,
				nrow = x@Dim[1], ncol = x@Dim[2], 
				dimnames = list(Terms=x@Dimnames[[1]], Docs = x@Dimnames[[2]])
				)
		x <- tm::as.TermDocumentMatrix(x, weighting = tm::weightTf)
		x
	} else {
		message(class(x), " to ", to, "\n")
		stop("Cannot convert between these two.")
	}
}
