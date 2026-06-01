# Rfun_mixedeff
# J Gou
# 2020-05-12
#'
#' @name mixedeff
#' @title Mixed Effects Models for Rating and Ranking
#' @description Calculate ratings and provide rankings using Mixed Effects Modeling
#' @param jpMat a Judge-Presenter matrix, or a User-Movie matrix
#' @param REML a logical value for lme4::lmer
#' @param ties.method a character string specifying how ties are treated, including "average", "first", "last", "random", "max", "min", from base::rank
#'
#' @author Jiangtao Gou
#' @author Fengqing Zhang
#'
#' @export
#' @import stats
#' @import lme4
#'
#' @references
#' Gou, J. and Wu, S. (2020). A Judging System for Project Showcase: Rating and Ranking with Incomplete Information. Technical Report.
#'
#' @examples
#' jpMat <- c(1,3,5,2,6,4,3,8,7)
#' attr(jpMat, "dim") <- c(3,3)
#' mixedeff(jpMat)
#'
mixedeff <- function (jpMat, REML=FALSE, ties.method='average') {
  src <- convertJudgePresenterMatrix(jpMat=jpMat) # score, row, column
  mxmodel <- lme4::lmer(score ~ col + (1|row), data=src, REML=REML)
  beta <- mxmodel@beta
  rating <- beta[1]*rep(1,length(beta)) + c(0, beta[-1])
  ranking <- base::rank(-rating, na.last="keep", ties.method)
  #
  mx <- lme4::lmer(score ~ col + (1|row), data=src, REML=FALSE)
  nomx <- stats::lm(score ~ col, data=src)
  anovaresult <- stats::anova(mx, nomx)
  modelreduction <- (anovaresult$`Pr(>Chisq)`)[2]
  #
  result <- list(rating=rating, ranking=ranking, randeffp=modelreduction, mxmodel=mxmodel, anovaresult=anovaresult)
  return(result)
}
