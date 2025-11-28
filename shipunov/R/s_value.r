S.value <- function(x) {
if(is.list(x)) {
 pos <- grep("p.*value.*", names(x), ignore.case=TRUE, perl=TRUE)[1]
 x <- x[[pos]]
 }
-log2(x)
}
