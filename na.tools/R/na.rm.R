#' na.rm 
#' 
#' Removes `NA` values from objects
#' 
#' @param object to remove `NA`s from
#' @param ...	further arguments special methods could require.
#' 
#' @details 
#' 
#' For **vectors** this is the same as [stats::na.omit()] or 
#' [stats::na.exclude()]. It will also work on recursive objects. 
#' 
#' This is predominantly maintained for syntactic convenience since a number of
#' functions have na.omir 
#' 
#' @return
#'   An object of the same class with all `NA` values removed. For 
#'   data.frame and data.table objects entire columns are removed if they 
#'   contain solely `NA` values.
#' 
#' @seealso 
#'   * [stats::na.omit()], [stats::na.exclude()]
#'   * [all_na()]
#'   
#' @md   
#' @importFrom stats na.omit
#' @export

na.rm <- na.omit

