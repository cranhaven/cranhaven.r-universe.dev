
tunePHspline<- function(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL, data,
                          cv=10, k){

.outcome <- paste("Surv(", times, ",", failures, ")")

if(!(is.null(group))){
  if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
    .f <- as.formula( paste(.outcome, "~", group, "+", paste( cov.quanti,  collapse = " + "), " + ", paste(cov.quali, collapse = " + "),
                            collapse = " ") )
  }
  if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
    .f <- as.formula( paste(.outcome, "~", group, "+", paste( cov.quanti, collapse = " + "),collapse = " ") )
  }
  if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
    .f <- as.formula( paste(.outcome, "~", group, "+",paste(cov.quali, collapse = " + "),collapse = " ") )
  }
  if(is.null(cov.quanti)==T & is.null(cov.quali)==T){
    .f <- as.formula( paste(.outcome, "~", group) )
  }
}
else{
  if(is.null(cov.quanti)==F & is.null(cov.quali)==F){
    .f <- as.formula( paste(.outcome, "~", paste( cov.quanti,  collapse = " + "), " + ", paste(cov.quali, collapse = " + "),
                            collapse = " ") )
  }
  if(is.null(cov.quanti)==F & is.null(cov.quali)==T){
    .f <- as.formula( paste(.outcome, "~", paste( cov.quanti, collapse = " + "),collapse = " ") )
  }
  if(is.null(cov.quanti)==T & is.null(cov.quali)==F){
    .f <- as.formula( paste(.outcome, "~",  paste(cov.quali, collapse = " + "),collapse = " ") )
  }
}

.time <- sort(unique(data[,times]))
.grid <-  expand.grid(k=k, u=0)

sample_id <- sample(nrow(data))
folds <- cut(seq(1,nrow(data)), breaks=cv, labels=FALSE)
folds_id <- folds[sample_id]
data$folds <- folds_id

.CVtune<-vector("list",cv*dim(.grid)[1])

l<-1
for (i in 1:cv){
  for (j in 1:dim(.grid)[1]){
    .CVtune[[l]]<-list(train=data[data$folds!=i, ], valid=data[data$folds==i, ], grid=.grid[j,])
    l=l+1
  }
}


ph.spline.par<-function(xx, times, failures, group, cov.quanti, cov.quali, newtimes){

  .k <- xx$grid$k
  .data <- xx$train
  .newdata <- xx$valid

  .flex<-flexsurvspline(.f, data = .data, scale="hazard", k=.k,
                        hessian=F, method="Nelder-Mead")

  .predlist<-summary(.flex, type = "survival", newdata=.newdata, ci = F, se=F )

  .time.ph.spline=.predlist[[1]]$time

  .pred=matrix(nrow=length(.predlist), ncol=length(.time.ph.spline))

  for (i in 1:length(.predlist)){ .pred[i,]=.predlist[[i]]$est }

  if(!is.null(newtimes)) {
    .pred.ph.spline <- cbind(rep(1, dim(.pred)[1]), .pred)
    .time.ph.spline <- c(0, .time.ph.spline)

    idx=findInterval(newtimes, .time.ph.spline)
    .pred=.pred.ph.spline[,pmax(1,idx)]
    .time <- newtimes
  }
  return(.pred)
}

.preFIT<-list()
.preFIT<-lapply(.CVtune, ph.spline.par, times=times, failures=failures, group=group,
                cov.quanti=cov.quanti, cov.quali=cov.quali,newtimes=.time)

.FitCV <- replicate(dim(.grid)[1], matrix(NA, nrow = length(data[,times]),
                                          ncol = length(.time)), simplify=F)
l<-1
for (i in 1:cv){
  for (j in 1:dim(.grid)[1]){
    .FitCV[[j]][data$folds==i,] <- .preFIT[[l]]
    l<-l+1
  }
}

ph.best.measure <- function(prediction.matrix, times, failures, data, prediction.times){
return(metrics(times=times, failures=failures, data=data, prediction.matrix=prediction.matrix,
              prediction.times=prediction.times, metric="ci")) }

.measure<-sapply(.FitCV, ph.best.measure, times=times, failures=failures,
                 data=data, prediction.times=.time)

.res <- data.frame(k = .grid[,1], measure = .measure)

.max<-.res[which(.res$measure==max(.res$measure, na.rm=TRUE) & is.na(.res$measure)==FALSE),]
.max<-.max[1,]

return( list(optimal=list(k=.max$k), results=.res ))
}

