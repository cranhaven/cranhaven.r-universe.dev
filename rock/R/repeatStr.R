#' Repeat a string a number of times
#'
#' @param n,str Normally, respectively the frequency with which to
#' repeat the string and the string to repeat; but the order of the
#' inputs can be switched as well.
#'
#' @return A character vector of length 1.
#' @aliases repStr repeatStr
#' @export repStr
#' @export repeatStr
#'
#' @examples ### 10 spaces:
#' repStr(10);
#'
#' ### Three euro symbols:
#' repStr("\u20ac", 3);
repeatStr <- repStr <- function (n = 1, str = " ") {
  if (is.character(n) && is.numeric(str)) {
    ### The input was switched.
    tmp <- n;
    n <- str;
    str <- tmp;
    rm(tmp);
  }
  if (n < 1) {
    return("");
  }
  else if (n == 1) {
    return(str);
  }
  else {
    res <- str;
    for(i in c(1:(n-1))) {
      res <- paste0(res, str);
    }
    return(res);
  }
}
