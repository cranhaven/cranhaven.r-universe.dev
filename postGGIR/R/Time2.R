#' @title Time of A Certain activity State
#' @description Calculate the total time of being in certain state, e.g. sedentary, active, MVPA, etc.
#'
#' @param x \code{vector} of activity data.
#' @param w \code{vector} of wear flag data with same dimension as \code{x}.
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param smallerthan Find a state that is smaller than a threshold, or greater than or equal to.
#'
#' @return Time
#' @importFrom accelerometry bouts
#'
#' @export
#'
#'
#'

Time2 = function(
  x,
  w,
  thresh,
  smallerthan = TRUE,
  bout.length = 1
){
  if(missing(w)){
    stop("Please input weartime flag vector w with same dimension")
  }

  if(length(x) != length(w)){
    stop("count x and weartime w should have the same length")
  }

  uwear = unique(c(w))
  uwear = as.integer(uwear)
  if (!all(uwear %in% c(0, 1, NA))) {
    stop("weartime w has non 0-1 data")
  }

  x = na.omit(x)
  w = na.omit(w)


  w[w == 0] = NA
  y = create.bouts(counts = x, thresh_lower = thresh, bout_length = bout.length)
  yw = y * w

  if(smallerthan){
    time = sum(yw == 0, na.rm = T)
  }
  if(!smallerthan){
    time = sum(yw == 1, na.rm = T)
  }

  return(time = time)
}

 



#' @title Timne Metrics for Whole Dataset
#' @description This function is a whole dataset wrapper for \code{Time}
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#'
#' @param thresh threshold to binarize the data.
#' @param bout.length minimum duration of defining an active bout; defaults to 1.
#' @param smallerthan Find a state that is smaller than a threshold, or greater than or equal to.
#'
#' @importFrom accelerometry bouts
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{time}{time of certain state}
#'
#' @export
#'
#'


Time_long2 = function(
  count.data,
  weartime,
  thresh,
  smallerthan = TRUE,
  bout.length = 1
){

  n1440<-ncol(count.data)-2
  n1441<- n1440+1
  n1442<- n1440+2
  n2880<- 2*n1440 
  minuteUnit<-n1440/1440

  if(missing(weartime)){
    print("No weartime supplied, calculated based on defualt from 05:00 to 23:00")
    weartime = wear_flag(count.data =  count.data)
  } else {
  if (length(which(count.data[,1]!= weartime[,1] | count.data[,2]!= weartime[,2]))>=1) stop ("Checking IDs between count.data and weartime in Time_long2 function.") 
  }



  mat = cbind(as.matrix(count.data[,-c(1:2)]),as.matrix(weartime[,-c(1:2)]))

  result.list =  apply(mat,1,function(x){
    Time2(x[1:n1440],x[n1441:n2880],thresh = thresh,bout.length = bout.length, smallerthan = smallerthan)
  })

  time_all = as.data.frame(cbind(count.data[,c(1,2)],result.list/minuteUnit))
  names(time_all) = c("ID","Day","time")


  return(time_all = time_all)
}

 




#' @title Timne Metrics for Whole Dataset
#' @description This function is a whole dataset wrapper for \code{Time}
#'
#' @param count.data \code{data.frame} of dimension n*1442 containing the 1440 minute activity data for all n subject days.
#' The first two columns have to be ID and Day.
#' @param weartime \code{data.frame} with dimension of \code{count.data}.
#' The first two columns have to be ID and Day.
#'
#' @param PA.threshold threshold to calculate the time in minutes of sedentary, light, moderate and vigorous activity the data. 
#'
#' @importFrom accelerometry bouts
#'
#' @return A dataframe with some of the following columns
#' \item{ID}{identifier of the person}
#' \item{Day}{indicator of which day of activity it is, can be a numeric vector of sequence 1,2,... or a string of date}
#' \item{time}{time of certain state}
#'
#' @export
#'
#'


PAfun = function(count.data,weartime,PA.threshold=c(50,100,400)){

PA.threshold = c(PA.threshold,Inf)  # for vig time.

sed_all0<-NULL
for (f in 1:length(PA.threshold)){

temp = Time_long2(count.data = count.data, weartime = weartime, thresh =PA.threshold[f], smallerthan = TRUE)
colnames(temp)[3]<-paste(colnames(temp)[3],PA.threshold[f],sep="")
if (f==1) sed_all0<-temp else {
if (length(which(sed_all0[,1]!=temp[,1]))>=1 | length(which(sed_all0[,2]!=temp[,2]))>=1) stop("check ID+Day in Time_long2 function") 
sed_all0<-cbind(sed_all0,temp[,3])  
}
# print(c(f,dim(temp),dim(sed_all0)))
}


sed_all<-sed_all0
for (j in 4:6) sed_all[,j]<-sed_all0[,j]-sed_all0[,j-1]
colnames(sed_all)[3:6]<-c("sed_dur","light_dur","mod_dur","vig_dur")
sed_all[,"MVPA_dur"]<-sed_all[,"mod_dur"] + sed_all[,"vig_dur"]  
sed_all[,"activity_dur"]<-sed_all[,"light_dur"] +sed_all[,"mod_dur"] + sed_all[,"vig_dur"] 

minuteNcol=(ncol(count.data)-2)/1440

for (j in 4:ncol(sed_all)) sed_all[,j]<-sed_all[,j]/minuteNcol

return(sed_all)
} 




########################################################################## 
##########################################################################
create.bouts<-function(counts, thresh_lower, bout_length = 1){

S1<-which(counts>=thresh_lower)
S0<-which(counts<thresh_lower)
bouts<-rep(NA,length(counts))
bouts[S1]<-1
bouts[S0]<-0
if (bout_length>1){
   
  for (i in 2:length(S0)){
      W<-S0[i]-S0[i-1]
      if (W-1<bout_length) bouts[ S0[i-1]:S0[i] ]<-0 #kill 1 in this interval
  }
}

return(bouts)
}

