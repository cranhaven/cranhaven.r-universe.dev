
#' Count NA
#'
#' @param x object
#'
#' @return NA and Not count
#' @export
#'
#' @examples
#' a <- c(1,2,3,1,NA,NA)
#' table_NA(a)
table_NA <- function(x){
    name <- paste0(deparse(substitute(x)),collapse = '')
    x <- as.character(x)
    ck <- is.na(x)
    x[!ck] <- 'Not'
    x[ck] <- 'NA'
    cmd <- sprintf("table(%s,useNA = 'ifany')",name)
    exec(paste0(name,'=x'))
    exec(cmd)
}