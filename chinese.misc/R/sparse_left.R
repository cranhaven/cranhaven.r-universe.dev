#' Check How many Words are Left under Certain Sparse Values
#'
#' This function does not really remove sparse words (which is what 
#' \code{tm::removeSparseTerms} does); rather, it only shows how 
#' many words are left when you specify some sparse values. 
#' See Examples.
#' 
#' @param x a DocumentTermMatrix or TermDocumentMatrix object.
#' @param sparse a numeric vector with elements >= 0 and <= 1.
#' 
#' @export
#' @examples
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' y <- sparse_left(dtm, seq(0, 1, 0.1))
#' # Then you can use plot(sort(y, decreasing = TRUE), type = "b") to 
#' # see which sparse value is proper.
sparse_left <- function(x, sparse){
	class1 <- class(x)[1]
    stopifnot(class1 %in% c("DocumentTermMatrix", "TermDocumentMatrix"))
	stopifnot(is.numeric(sparse), all(sparse >= 0), all(sparse <= 1))
	inner_sum_spa <- function(ispa, ta, ndoc) sum(ta > ndoc * (1 - ispa))
	if (class1 == "TermDocumentMatrix"){
		tabled <- table(x$i)
		xn <- x$ncol
	} else {
		tabled <- table(x$j)
		xn <- x$nrow
	}
	y <- unlist(lapply(sparse, inner_sum_spa, ta = tabled, ndoc = xn))	
	names(y) <- sparse
	y
}
