
metrics <- function(times, failures, data, prediction.matrix, prediction.times, metric, pro.time=NULL, ROC.precision=seq(.01, .99, by=.01))
{
  data.times <- data[,times]
  data.failures <- data[,failures]
  obj_surv <- Surv(data.times, data.failures)

  time <- obj_surv[, 1]
  ot <- order(time)
  cens <- obj_surv[ot, 2]
  time <- time[ot]

  #hatcdist <- prodlim(Surv(time, cens) ~ 1, reverse = TRUE)
  #csurv <- predict(hatcdist, times = time, type = "surv")

  .temp <- survfit(Surv(time, cens==0) ~ 1)
  csurv <- summary(.temp, times=time, extend=TRUE)$surv

  csurv[csurv == 0] <- Inf

  #csurv_btime <- predict(hatcdist, times = sort(prediction.times), type = "surv")

  csurv_btime <- summary(.temp, times=sort(prediction.times), extend=TRUE)$surv

  csurv_btime[is.na(csurv_btime)] <- min(csurv_btime, na.rm = TRUE)
  csurv_btime[csurv_btime == 0] <- Inf

  survs <- t(prediction.matrix)[,ot]

  time.pred <- sort(unique(data[,times]))

  timeVector<-sort(unique(prediction.times))

  if(is.null(pro.time)) {pro.time <- median(data.times)}

  switch(metric,
         ci={
           time <- obj_surv[, 1]
           status <- obj_surv[, 2]

           j <- length(timeVector[which(timeVector<=pro.time)])
           predicted <- prediction.matrix[,prediction.times>=pro.time][,1]  #survs[j, ]

           permissible <- 0 # comparable pairs
           concord <- 0 # completely concordance
           par_concord <- 0 # partial concordance

           n <- length(time)
           for (i in 1:(n - 1)) {
             for (j in (i + 1):n) {
               # Exclude incomparable pairs
               if ((time[i] < time[j] &
                    status[i] == 0) | (time[j] < time[i] & status[j] == 0)) {
                 next
               }

               if (time[i] == time[j] & status[i] == 0 & status[j] == 0) {
                 next
               }

               permissible <- permissible + 1

               # survival times unequal
               if (time[i] != time[j]) {
                 if ((time[i] < time[j] &
                      predicted[i] < predicted[j]) |
                     (time[j] < time[i] & predicted[j] < predicted[i])) {
                   concord <- concord + 1
                 } else if (predicted[i] == predicted[j]) {
                   par_concord <- par_concord + 0.5
                 }
               }

               # survival times equal
               if (time[i] == time[j] & status[i] == 1 & status[j] == 1) {
                 if (predicted[i] == predicted[j]) {
                   concord <- concord + 1
                 } else {
                   par_concord <- par_concord + 0.5
                 }
               }
               # one censored one died
               if (time[i] == time[j] &
                   ((status[i] == 1 &
                     status[j] == 0) | (status[i] == 0 & status[j] == 1))) {
                 if ((status[i] == 1 &
                      predicted[i] < predicted[j]) |
                     (status[j] == 1 & predicted[j] < predicted[i])) {
                   concord <- concord + 1
                 } else {
                   par_concord <- par_concord + 0.5
                 }
               }
             }
           }
           RET <- (concord + par_concord) / permissible
         },
         ibs={
           bsc <- sapply(1:length(timeVector), FUN = function(j)
           {
             help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
             help2 <- as.integer(time > timeVector[j])
             return(mean((0 - survs[j, ])^2 * help1 * (1/csurv) + (1 - survs[j, ])^2 * help2 * (1/csurv_btime[j])))
           })

           idx <- 2:length(timeVector)
           RET <- diff(timeVector) %*% ((bsc[idx - 1] + bsc[idx])/2)
           RET <- RET/diff(range(timeVector))
           RET <- as.matrix(RET)
         },
         bs={
           j <- length(timeVector[which(timeVector<=pro.time)])
           help1 <- as.integer(time <= timeVector[j] &  obj_surv[ot,2] == 1)
           help2 <- as.integer(time > timeVector[j])
           bs <- mean((0 - survs[j, ])^2 * help1 * (1/csurv) + (1 - survs[j, ])^2 * help2 * (1/csurv_btime[j]))
           bs <- as.numeric(bs)
           RET <- bs
         },
         ibll={
           survs[which(survs==0)]<-10**-7
           survs[which(survs==1)]<-1-10**-7
           bll <- sapply(1:length(timeVector), FUN = function(j)
           {
             help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
             help2 <- as.integer(time > timeVector[j])
             RET=-mean(log(1 - survs[j, ]) * help1 * (1/csurv) + log( survs[j, ]) * help2 * (1/csurv_btime[j]))
             RET=ifelse(is.nan(RET)==T,0,RET)
             return(RET)
           })

           idx <- 2:length(timeVector)
           RET <- diff(timeVector) %*% ((bll[idx - 1] + bll[idx])/2)
           RET <- RET/diff(range(timeVector))
           RET <- as.matrix(RET)
         },
         bll={
           survs[which(survs==0)]<-10**-7
           survs[which(survs==1)]<-1-10**-7
           j <- length(timeVector[which(timeVector<=pro.time)])
           help1 <- as.integer(time <= timeVector[j] &  obj_surv[ot,2] == 1)
           help2 <- as.integer(time > timeVector[j])
           bll <- -mean(log(1- survs[j, ]) * help1 * (1/csurv) + log(survs[j, ]) * help2 * (1/csurv_btime[j]))
           bll <- as.numeric(bll)
           RET <- bll
         },
         auc={
           .data <- data.frame(times=data[,times], failures=data[,failures],
                               variable=1-prediction.matrix[,prediction.times>=pro.time][,1])
           RET <- roc(times="times", failures="failures", variable="variable",
                           confounders=~1, data=.data,
                           pro.time=pro.time, precision=ROC.precision)$auc
         },
        ribs={
          timeVector <- timeVector[timeVector<=pro.time]
          bsc <- sapply(1:length(timeVector), FUN = function(j)
          {
            help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
            help2 <- as.integer(time > timeVector[j])
            return(mean((0 - survs[j, ])^2 * help1 * (1/csurv) + (1 - survs[j, ])^2 * help2 * (1/csurv_btime[j])))
          })
          idx <- 2:length(timeVector)
          RET <- diff(timeVector) %*% ((bsc[idx - 1] + bsc[idx])/2)
          RET <- RET/diff(range(timeVector))
          RET <- as.matrix(RET)
        },
        ribll={
          timeVector <- timeVector[timeVector<=pro.time]
          bll <- sapply(1:length(timeVector), FUN = function(j)
          {
            help1 <- as.integer(time <= timeVector[j] & obj_surv[ot,2] == 1)
            help2 <- as.integer(time > timeVector[j])
            RET=-mean(log(1 - survs[j, ]) * help1 * (1/csurv) + log( survs[j, ]) * help2 * (1/csurv_btime[j]))
            RET=ifelse(is.nan(RET)==T,0,RET)
            return(RET)
          })

          idx <- 2:length(timeVector)
          RET <- diff(timeVector) %*% ((bll[idx - 1] + bll[idx])/2)
          RET <- RET/diff(range(timeVector))
          RET <- as.matrix(RET)
        },
  )
  return(as.numeric(RET))
}
