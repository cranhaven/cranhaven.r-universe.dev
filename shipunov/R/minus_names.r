"%-%" <- function(x, y) {
 if (is.null(names(x))) names(x) <- x
 names(x)[!names(x) %in% y]
}
