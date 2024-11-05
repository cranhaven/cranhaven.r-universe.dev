#' Make ID
#' 
#' Make IDs with fixed length and a prefix (e.g., S001, S002, ..., S100).
#' @param n the number of IDs to generate (or a vector of numbers)
#' @param prefix the prefix to the number (default "S")
#' @param digits the number of digits to use for the numeric part. Only used if this is larger than the largest number of digits in n.
#' @param suffix the suffix to the number (default "")
#' 
#' @return a vector of IDs
#' @export
#' 
#' @examples 
#' 
#' make_id(20, "SUBJECT_")
#' make_id(10:30, digits = 3)
#' 
make_id <- function(n = 100, prefix = "S", digits = 0, suffix = "") {
  if (!is.numeric(n)) stop("n must be numeric")
  
  if (length(n) == 1) n <- 1:n
  
  max_digits <- as.character(n) %>% 
    nchar() %>% max() %>% max(digits)
  max_decimal <- as.character(n) %>% 
    # remove whole number parts 
    # (don't use n%%1 because floating point precision)
    sub("(^\\d*\\.|^\\d*$)", "", .) %>% 
    nchar() %>% max()
  fmt <- paste0(prefix, "%0", max_digits, ".", max_decimal, "f", suffix)
  
  sprintf(fmt, n)
}
