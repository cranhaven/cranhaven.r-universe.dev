#' @title Relative Amplitude
#' @description This function calcualte relative amplitude, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} vector of activity data
#' @param window since the caculation of M10 and L5 depends on the dimension of data, we need to include
#' window size as an argument.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#'
#' @return RA
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#' @references Junrui Di et al. Joint and individual representation of domains of physical activity, sleep, and circadian rhythmicity. Statistics in Biosciences.
#' 
#'
#'

RA2 = function(
  x,
  window = 1,
  method = c("average","sum")
  ){

  if(length(x) %% 1440/window != 0){
    stop("Window size and length of input vector doesn't match.
         Only use window size that is an integer factor of 1440")
  }

  x_bin = bin_data2(x, window = window, method = method) 
  oneH=length(x_bin)/24 #window size of one hour, oneH=60 for minute level data
  M10 = max(roll(x_bin, 10*oneH),na.rm=TRUE)
  L5 = min(roll(x_bin, 5*oneH),na.rm=TRUE)
 

  M10TIME_num_index= which.max(roll(x_bin, 10 * oneH))  #start location
  L5TIME_num_index = which.min(roll(x_bin, 5 * oneH))  
  M10TIME_num= M10TIME_num_index/oneH
  L5TIME_num = L5TIME_num_index/oneH  
  RA<-(M10 - L5)/(M10 + L5)
  RAoutput<-c(M10TIME_num,L5TIME_num,M10,L5,RA)  
  #print(c(M10TIME_num_index,L5TIME_num_index))
  
  return(RAoutput)
}


roll = function(day.counts,k){
  # take mean for M10 and L5
  # rollapplyr (R package zoo)

  kvec = rollapplyr(as.numeric(day.counts), k, function(x) mean(x,na.rm = T), fill = NA)
  kvec = kvec[!is.na(kvec)]
  return(kvec)
}


 
#' @title Relative Amplitude for the Whole Datset
#' @description This function calcualte relative amplitude, a nonparametric metric
#' of circadian rhtymicity. This function is a whole dataset
#' wrapper for \code{RA}.
#'
#' @param count.data \code{data.frame} of dimension n * (p+2) containing the
#' p dimensional activity data for all n subject days.
#' The first two columns have to be ID and Day. ID can be
#' either \code{character} or \code{numeric}. Day has to be \code{numeric} indicating
#' the sequency of days within each subject.
#' @param window since the caculation of M10 and L5 depends on the dimension of data, we need to include
#' window size as an argument. This function is a whole dataset
#' wrapper for \code{RA}.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#'
#' @return A \code{data.frame} with the following 3 columns
#' \item{ID}{ID}
#' \item{Day}{Day}
#' \item{RA}{RA}
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#'
#'
 

RA_long2 = function(
  count.data,
  window = 1,
  method = c("average","sum")
  ){ 
 
  x = count.data[,-c(1:2)]
  result.list =  t(apply(x,1,RA2, window = window, method = method)) #run RA2 for each row
  ra_all = as.data.frame(cbind(count.data[,c(1,2)],result.list))

  names(ra_all) = c("ID","Day","M10TIME","L5TIME","M10","L5","RA")
  return(ra = ra_all)
}

   

