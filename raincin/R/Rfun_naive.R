# Rfun_naive
# J Gou
# 2020-02-24
# 2020-05-14 modified
#
#' @name naive
#' @title Simple Linear Models for Rating and Ranking
#' @description Calculate ratings and provide rankings using Simple Linear regression
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param stats a logical value to indicate whether a linear model should be fitted and the test statistics should be reported
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#' 
#'
#' @author Jiangtao Gou
#' @author Shuyi Wu
#' @export
#' @import stats
#' 
#' @references
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#' 
#' @examples
#' jpMat <- matrix(data=c(5,4,3,0, 5,5,3,1, 0,0,0,5, 0,0,2,0, 4,0,0,3, 1,0,0,4),
#' nrow=6,
#' byrow=TRUE)
#' result <- naive(jpMat)
#' print(result)
#
naive <- function(jpMat, stats=FALSE, ties.method='average') {
  sumPresenter <- colSums(jpMat, na.rm=TRUE)
  countPresenter <- colSums(jpMat!=0, na.rm=TRUE)
  rating <- sumPresenter/countPresenter
  #<https://stats.stackexchange.com/questions/3321/rank-in-r-descending-order/3322>
  #<https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/rank>
  ranking <- rank(-rating, na.last="keep", ties.method)
  result <- list(rating=rating, ranking=ranking)
  if (stats) {
    src <- convertJudgePresenterMatrix(jpMat=jpMat) # score, row, column
    lmodel <- stats::lm(score ~ col, data=src)
    #lmodel <- lme4::lmer(score ~ col, data=src)
    result <- list(rating=rating, ranking=ranking, lmodel=lmodel)
  }
  return(result)
}