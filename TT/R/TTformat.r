#' @title
#' Format table columns
#' @description
#' Simply wrapper on format... family functions of 'DT' package.
#' For details see: \code{\link[DT]{formatCurrency}}\cr
#' The wrappers are not affecting behavior of original format... functions
#' @inheritParams DT::formatCurrency
#' @return Return formatted 'HTML' widget of 'DataTables'
#' @export
formatCurrency <- function (table, columns, currency = "$", interval = 3, mark = ",",
                            digits = 2, dec.mark = getOption("OutDec"), before = TRUE)
                  {
  columns <- incrementCol(columns)
  DT::formatCurrency(table, columns, currency, interval,
                      mark, digits, dec.mark, before)
}

#' @rdname formatCurrency
#' @export
formatDate <- function (table, columns, method = "toDateString", params = NULL) {
  columns <- incrementCol(columns)
  DT::formatDate(table, columns, method , params)
}

#' @rdname formatCurrency
#' @export
formatPercentage <- function (table, columns, digits = 0, interval = 3, mark = ",",
                              dec.mark = getOption("OutDec")) {
  columns <- incrementCol(columns)
  DT::formatPercentage(table, columns, digits, interval, mark, dec.mark)
}

#' @rdname formatCurrency
#' @export
formatRound <- function(table, columns, digits = 2, interval = 3, mark = ",",
                         dec.mark = getOption("OutDec")){
  columns <- incrementCol(columns)
  DT::formatRound(table, columns, digits, interval, mark, dec.mark)
}

#' @rdname formatCurrency
#' @export
formatSignif <- function(table, columns, digits = 2, interval = 3, mark = ",",
                         dec.mark = getOption("OutDec")){
  columns <- incrementCol(columns)
  DT::formatSignif(table, columns, digits, interval, mark, dec.mark)
}

#' @rdname formatCurrency
#' @export
formatString <- function(table, columns, prefix = "", suffix = ""){
  columns <- incrementCol(columns)
  DT::formatString(table, columns, prefix, suffix)
}

#' @rdname formatCurrency
#' @export
formatStyle <- function(table, columns, valueColumns = columns, target = c("cell", "row"),
                        fontWeight = NULL, color = NULL, backgroundColor = NULL,
                        background = NULL, ...){
  columns <- incrementCol(columns)
  valueColumns <- incrementCol(valueColumns)
  DT::formatStyle(table, columns, valueColumns, target, fontWeight, color, backgroundColor, background, ...)
}

#' @noRd
incrementCol <- function(cols){
  if (inherits(cols, "numeric")) return( cols + 3)
  return (cols)
}
