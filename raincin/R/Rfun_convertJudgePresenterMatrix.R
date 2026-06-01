# Rfun_convertJudgePresenterMatrix.R
# J Gou
# 2020-05-12
#
#' @name convertJudgePresenterMatrix
#' @title Judge-Presenter Matrix Converter
#' @description Convert a judge-presenter matrix to a data frame with three variables/columns
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @return A data frame as a long table, where each row is an observation, including the score, the row number and the column number in the \code{jpMat} matrix 
#' @details
#' \enumerate{
#'  \item \code{score}: nonzero and non-NA scores
#'  \item \code{row}: array indices 
#'  \item \code{col}: arry indices
#' } 
#' @author Jiangtao Gou
#' @author Fengqing Zhang
#' @export
#' @import stats
#' 
#' @examples 
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),
#' nrow=6,
#' byrow=TRUE)
#' result <- convertJudgePresenterMatrix(jpMat)
#' print(result)
#' 
convertJudgePresenterMatrix <- function (jpMat) {
  #<https://stackoverflow.com/questions/3192791/find-indices-of-non-zero-elements-in-matrix>
  #<https://www.gormanalysis.com/blog/sparse-matrix-construction-and-use-in-r/>
  output <- data.frame(score = jpMat[which((jpMat !=0) & !(is.na(jpMat)))], which((jpMat !=0) & !(is.na(jpMat)), arr.ind = TRUE) )
  output$row <- factor(output$row)
  output$col <- factor(output$col)
  return(output)
}
