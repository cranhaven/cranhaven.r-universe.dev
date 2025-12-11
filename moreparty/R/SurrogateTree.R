# @import partykit
#'
#' @importFrom stats predict lm

#' @export

SurrogateTree <- function(object, mincriterion = 0.95, maxdepth=3) {
  response = object@responses
  CLASS = all(response@is_nominal | response@is_ordinal)
  if(CLASS==TRUE) {
    pred <- predict(object, OOB=TRUE, type='prob')
    pred <- do.call('rbind',pred)[,2]
  }
  if(CLASS==FALSE) pred <- predict(object)[,1]
  input = object@data@get("input")
  dt_surro <- data.frame(yhat=pred, input)
  ctree_surro <- partykit::ctree(yhat~., dt_surro, control=partykit::ctree_control(mincriterion=mincriterion, maxdepth=maxdepth))
  pred_surro <- predict(ctree_surro) #[,1]
  r.squared <- summary(lm(pred~pred_surro))$r.squared
  return(list(tree=ctree_surro, r.squared=r.squared))
}


# object=iris.cf
# data(iris)
# iris2 = iris
# iris2$Species = factor(iris$Species == "versicolor")
# iris.cf = party::cforest(Species ~ ., data = iris2,
#                          control = party::cforest_unbiased(mtry = 2, ntree = 50))
# surro <- SurrogateTree(iris.cf)
# surro$r.squared
# plot(surro$tree)
