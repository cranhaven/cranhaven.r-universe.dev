
tuneSNN <- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
                           data, cv=10, n.nodes, decay, batch.size, epochs){

  data.nn <- data[,c(times, failures, group, cov.quanti, cov.quali)]

  sample_id <- sample(nrow(data.nn))
  folds <- cut(seq(1,nrow(data.nn)), breaks=cv, labels=FALSE)
  folds_id <- folds[sample_id]
  data.nn$folds <- folds_id

  .f  <- as.formula(paste("Surv(", times, ",", failures, ")", "~."))
  .time <- sort(unique(data.nn[,times]))
  .grid <-  expand.grid(n.nodes=n.nodes, decay=decay, batch.size=batch.size,
                        epochs=epochs)

  .CVtune<-vector("list",cv*dim(.grid)[1])

  l<-1
  for (k in 1:cv){
    for (j in 1:dim(.grid)[1]){
      .CVtune[[l]]<-list(train=data.nn[data.nn$folds!=k, ], valid=data.nn[data.nn$folds==k, ], grid=.grid[j,])
      l=l+1
    }
  }

  nn.time.par<-function(xx, times, failures, group, cov.quanti, cov.quali,newtimes){

    n.nodes=xx$grid$n.nodes
    decay=xx$grid$decay
    batch_size=xx$grid$batch.size
    epochs=xx$grid$epochs

    data=xx$train
    newdata=xx$valid

    if(!(is.null(group))){
      .data <- data[,c(times, failures, group, cov.quanti, cov.quali)]}   else{
        .data <- data[,c(times, failures, cov.quanti, cov.quali)] }

    .f  <- as.formula(paste("Surv(", times, ",", failures, ")", "~."))

    .deepsurv <- survivalmodels::deepsurv(.f, data = .data,  verbose = FALSE, num_nodes=n.nodes,
                          weight_decay=decay, num_workers = 0L,batch_size=as.integer(batch_size),
                          epochs=as.integer(epochs))

    .time<-sort(unique(.data[,times]))

    .newdata <- data.frame(newdata[,c(group, cov.quanti, cov.quali)])
    .pred <- predict(.deepsurv, newdata=newdata)
    .time.deepsurv<-as.numeric(dimnames(.pred)[[2]])

    if(!is.null(newtimes)) {
      .pred.deepsurv <- cbind(rep(1, dim(.pred)[1]), .pred)
      .time.deepsurv <- c(0, .time.deepsurv)

      idx=findInterval(newtimes, .time.deepsurv)
      .pred=.pred.deepsurv[,pmax(1,idx)]

      .time <- newtimes
    }

    return(.pred)
  }

  .preFIT<-list()
  .preFIT<-lapply(.CVtune, nn.time.par, times=times, failures=failures, group=group,
                 cov.quanti=cov.quanti, cov.quali=cov.quali,newtimes=.time)

  .FitCV <- replicate(dim(.grid)[1], matrix(NA, nrow = length(data[,times]),
                                            ncol = length(.time)), simplify=F)
  l<-1
  for (k in 1:cv){
    for (j in 1:dim(.grid)[1]){
      .FitCV[[j]][data.nn$folds==k,] <- .preFIT[[l]]
      l<-l+1
    }
  }

  nn.best.measure <- function(prediction.matrix, times, failures, data, prediction.times){
    return(metrics(times=times, failures=failures, data=data, prediction.matrix=prediction.matrix,
                  prediction.times=prediction.times, metric="ci"))
  }

    .measure<-sapply(.FitCV, nn.best.measure, times=times, failures=failures, data=data.nn, prediction.times=.time)

    .res <- data.frame(n.nodes = .grid[,1], decay = .grid[,2], batch.size=.grid[,3],
                       epochs=.grid[,4] , measure = .measure)

    .maxi<-.res[which(.res$measure==max(.res$measure, na.rm=TRUE) & is.na(.res$measure)==FALSE),]
    .maxi<-.maxi[1,]

    return( list(optimal=list(n.nodes=.maxi$n.nodes,
                              decay=.maxi$decay,
                              batch.size=.maxi$batch.size,
                              epochs=.maxi$epochs),
                              results=.res ))
}
