
library(caret)

wine <- do.call(
  args = list(),
  what = function(){
    data(wine)

    set.seed(1)
    train.index <- createDataPartition(wine$Wine, p = .7, list = FALSE)
    train <- wine[ train.index,]
    test  <- wine[-train.index,]

    cls <- which(colnames(wine) == "Wine")

    #% LABELED
    labeled.index <- createDataPartition(train$Wine, p = .2, list = FALSE)
    train[-labeled.index,cls] <- NA


    list(train = train, test = test,
         cls = cls
    )
  })

