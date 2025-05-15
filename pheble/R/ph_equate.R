#' Equate factors levels.
#'
#' The \code{ph_equate} function ensures that the factor levels in all columns are equal. When classification are heavily biased or inaccurate, they can return new class predictions that do not contain every level in the original data. This can interfere with model evaluation functions e.g. via a confusion matrix.
#'
#' @param df A \code{data.frame} of column-wise class predictions.
#' @param class A \code{factor} value for the observed or classes.
#' @returns A data frame of column-wise class predictions with class levels equal to the observed class.
#' @export
#' @examples
#' ## Make data frame of predicted classes with different levels.
#' ## An internal or external column should contain the observed
#' ## classes with every possible level.
#' obs <- as.factor(c("A", "C", "B", "D", "E"))
#' method_a <- c("A", "B", "B", "C", "D")
#' method_b <- c("A", "C", "B", "D", "C")
#' method_c <- c("A", "C", "B", "B", "C")
#' df <- data.frame(method_a, method_b, method_c)
#' df <- ph_equate(df = df, class = obs)
ph_equate <- function(df, class)
{
    df <- as.data.frame(df)
    if (!is.factor(class))
        stop("Class must be a factor).")
    for (i in 1:ncol(df)) {
        df[, i] <- factor(df[, i],
                          levels = unique(class),
                          ordered = FALSE)
    }
    return(df)
}
