#' Sum of missing value by row
#' @description NA is treated as missing value.
#' @param data must be dataframe or matrix
#' @return sum of missing value by row
#' @export
#'
#' @examples
#' df = data.frame(x=rep(c(1,NA,2,NA,6,NA),10),
#'                  y=rep(c(1,NA,2),20))
#' NA.row.sums(df)
NA.row.sums <- function(data){
    if (!(is.data.frame(data) | is.matrix(data))) stop('data must be dataframe or matrix')
    rowSums(is.na(data))
}

#' Proportion of missing value by row
#' @description NA is treated as missing value.
#' @param data must be dataframe or matrix
#' @return proportion of missing value by row
#' @export
#'
#' @examples
#' df = data.frame(x=rep(c(1,NA,2,NA,6,NA),10),
#'                  y=rep(c(1,NA,2),20))
#' NA.row.prob(df)
NA.row.prob <- function(data){
    if (!(is.data.frame(data) | is.matrix(data))) stop('data must be dataframe or matrix')
    rowSums(is.na(data))/ncol(data)
}

#' Sum of missing value by column
#' @description NA is treated as missing value.
#' @param data must be dataframe or matrix
#' @return sum of missing value by column
#' @export
#'
#' @examples
#' df = data.frame(x=rep(c(1,NA,2,NA,6,NA),10),
#'                  y=rep(c(1,NA,2),20))
#' NA.col.sums(df)
NA.col.sums <- function(data){
    if (!(is.data.frame(data) | is.matrix(data))) stop('data must be dataframe or matrix')
    colSums(is.na(data))
}

#' Proportion of missing value by column
#' @description NA is treated as missing value.
#' @param data must be dataframe or matrix
#' @return proportion of missing value by column
#' @export
#'
#' @examples
#' df = data.frame(x=rep(c(1,NA,2,NA,6,NA),10),
#'                  y=rep(c(1,NA,2),20))
#' NA.col.prob(df)
NA.col.prob <- function(data){
    if (!(is.data.frame(data) | is.matrix(data))) stop('data must be dataframe or matrix')
    colSums(is.na(data))/nrow(data)
}
#' Sum of missing value in the whole dataframe
#' @description NA is treated as missing value.
#' @param data must be dataframe or matrix
#' @return sum of missing value in the whole dataframe
#' @export
#'
#' @examples
#' df = data.frame(x=rep(c(1,NA,2,NA,6,NA),10),
#'                  y=rep(c(1,NA,2),20))
#' NA.whole.sums(df)
NA.whole.sums <- function(data){
    if (!(is.data.frame(data) | is.matrix(data))) stop('data must be dataframe or matrix')
    sum(colSums(is.na(data)))
}
#' Proportion of missing value in the whole dataframe
#' @description NA is treated as missing value.
#' @param data must be dataframe or matrix
#' @return proportion of missing value in the whole dataframe
#' @export
#'
#' @examples
#' df = data.frame(x=rep(c(1,NA,2,NA,6,NA),10),
#'                  y=rep(c(1,NA,2),20))
#' NA.whole.prob(df)
NA.whole.prob <- function(data){
    if (!(is.data.frame(data) | is.matrix(data))) stop('data must be dataframe or matrix')
    sum(colSums(is.na(data)))/(nrow(data)*ncol(data))
}

