
breast <- do.call(
  args = list(),
  what = function(){
    data(breast)

    set.seed(1)
    train.index <- createDataPartition(breast$Class, p = .7, list = FALSE)
    train <- breast[ train.index,]
    test  <- breast[-train.index,]

    cls <- which(colnames(breast) == "Class")

    #% LABELED
    labeled.index <- createDataPartition(train$Class, p = .2, list = FALSE)
    train[-labeled.index,cls] <- NA


    list(train = train, test = test,
         cls = cls
    )
  })

