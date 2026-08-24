#' Pretty formatting of *p* values
#'
#' @param values The p-values to format.
#' @param digits The number of digits to round to. Numbers smaller
#'   than this number will be shown as <.001 or <.0001 etc.
#' @param spaces Whether to include spaces between symbols,
#'   operators, and digits.
#' @param includeP Whether to include the 'p' and '='-symbol in the
#'   results (the '<' symbol is always included).
#'
#' @return A formatted P value, roughly according to APA style
#'   guidelines. This means that the [noZero] function is used to
#'   remove the zero preceding the decimal point, and p values
#'   that would round to zero given the requested number of digits
#'   are shown as e.g. p<.001.
#' @seealso [formatCI()], [formatR()], [noZero()]
#' @export
#'
#' @examples formatPvalue(cor.test(mtcars$mpg,
#'                       mtcars$disp)$p.value);
#' formatPvalue(cor.test(mtcars$drat,
#'                       mtcars$qsec)$p.value);

formatPvalue <- function(values,
                         digits = 3,
                         spaces=TRUE,
                         includeP = TRUE) {
  missingValues <-
    is.na(values);
  values <-
    ifelse(values < 0,
           0,
           ifelse(values > 1, 1, values));
  pchar <-
    ifelse(includeP, "p = ", "");
  eps <- 10 ^ -digits;
  res <- paste0(pchar,
                ufs::noZero(format.pval(round(values, digits),
                                        eps=eps, digits=digits,
                                        scientific=digits+1)));
  if (spaces) {
    res <- gsub("= <", "< ", res);
  } else {
    res <- gsub("= <", "<", res);
    res <- gsub(" ", "", res);
  }
  res <- ifelse(missingValues, NA, res);
  return(res);
}
