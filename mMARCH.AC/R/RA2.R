#' @title Relative Amplitude
#' @description This function calcualte relative amplitude, a nonparametric metric
#' reprsenting fragmentation of circadian rhtymicity
#'
#' @param x  \code{vector} vector of activity data
#' @param window since the caculation of M10 and L5 depends on the dimension of data, we need to include
#' window size as an argument.
#' @param method \code{character} of "sum" or "average", function used to bin the data
#' @param noon2noon  \code{logical}  Specify if M10 and L5 were calculated from noon to noon. Default is FALSE.  
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
  method = c("average","sum"),
  noon2noon = FALSE
  ){

  if(length(x) %% 1440/window != 0){
    stop("Window size and length of input vector doesn't match.
         Only use window size that is an integer factor of 1440")
  }

  x_bin = bin_data2(x, window = window, method = method) 
  oneH=length(x_bin)/24 #window size of one hour, oneH=60 for minute level data
  
  M10 = max(roll(x_bin, 10*oneH),na.rm=TRUE)
  L5 = min(roll(x_bin, 5*oneH),na.rm=TRUE)  
  RA<-(M10 - L5)/(M10 + L5)

  M5 = max(roll(x_bin, 5*oneH),na.rm=TRUE)
  M6 = max(roll(x_bin, 6*oneH),na.rm=TRUE) 
  M7 = max(roll(x_bin, 7*oneH),na.rm=TRUE) 
  M8 = max(roll(x_bin, 8*oneH),na.rm=TRUE) 
  M9 = max(roll(x_bin, 9*oneH),na.rm=TRUE)

  L6 = min(roll(x_bin, 6*oneH),na.rm=TRUE)
  L7 = min(roll(x_bin, 7*oneH),na.rm=TRUE)
  L8 = min(roll(x_bin, 8*oneH),na.rm=TRUE)
  L9 = min(roll(x_bin, 9*oneH),na.rm=TRUE)
  L10 = min(roll(x_bin, 10*oneH),na.rm=TRUE)
  
  if (noon2noon) timezero=12 else timezero=0
  M5TIME_num= which.max(roll(x_bin, 5 * oneH))/oneH + 2.5 + timezero 
  M6TIME_num= which.max(roll(x_bin, 6 * oneH))/oneH + 3  + timezero 
  M7TIME_num= which.max(roll(x_bin, 7 * oneH))/oneH + 3.5 + timezero 
  M8TIME_num= which.max(roll(x_bin, 8 * oneH))/oneH + 4  + timezero 
  M9TIME_num= which.max(roll(x_bin, 9 * oneH))/oneH + 4.5  + timezero 
  M10TIME_num= which.max(roll(x_bin, 10 * oneH))/oneH + 5 + timezero 


  L5TIME_num =  which.min(roll(x_bin, 5 * oneH)) /oneH + 2.5  + timezero 
  L6TIME_num =  which.min(roll(x_bin, 6 * oneH)) /oneH + 3   + timezero 
  L7TIME_num =  which.min(roll(x_bin, 7 * oneH)) /oneH + 3.5  + timezero 
  L8TIME_num =  which.min(roll(x_bin, 8 * oneH)) /oneH  + 4  + timezero 
  L9TIME_num =  which.min(roll(x_bin, 9 * oneH)) /oneH + 4.5  + timezero 
  L10TIME_num =  which.min(roll(x_bin, 10 * oneH)) /oneH  + 5 + timezero 
 


  RAoutput<-c(M10TIME_num,L5TIME_num,M10,L5,RA,M5,M5TIME_num,M6,M6TIME_num,M7,M7TIME_num,M8,M8TIME_num,M9,M9TIME_num, 
                                             L6,L6TIME_num,L7,L7TIME_num,L8,L8TIME_num,L9,L9TIME_num, L10,L10TIME_num)
 
  #print(c(M10TIME_num_index,L5TIME_num_index))
  # note: change start time to center time for L5 and M10 window on 4/28/23.
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
#' @param noon2noon  \code{logical}  Specify if M10 and L5 were calculated from noon to noon. Default is FALSE. 
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
  method = c("average","sum"),
  noon2noon = FALSE
  ){ 
 

  if (noon2noon){
     n720=(ncol(count.data)-2)/2
     count.data.nn<-array(NA,dim=dim(count.data))
     for (i in 1:nrow(count.data)){ 
           count.data.nn[i,2+1:n720] <-  as.numeric(count.data[i,(3+n720):ncol(count.data)])
           nextday<-which(count.data[,1]==count.data[i,1] & count.data[,2]==as.Date(count.data[i,2]) +1 )
           if (length(nextday)==1) count.data.nn[i,(3+n720):ncol(count.data.nn)] <- as.numeric(count.data[nextday,2+1:n720] ) 
           # print(c(i,nextday))
      }
      Xmatrix = count.data.nn[,-c(1:2)] 
   }  else Xmatrix = count.data[,-c(1:2)]

  

  result.list =  t(apply(Xmatrix,1,RA2, window = window, method = method, noon2noon=noon2noon)) #run RA2 for each row
  ra_all = as.data.frame(cbind(count.data[,c(1,2)],result.list))
  names(ra_all) = c("ID","Day","M10TIME","L5TIME","M10","L5","RA","M5","M5TIME","M6","M6TIME","M7","M7TIME","M8","M8TIME","M9","M9TIME",
                    "L6","L6TIME","L7","L7TIME","L8","L8TIME","L9","L9TIME","L10","L10TIME") 

  return(ra = ra_all)
}

   





#' @title get subject average of time variables
#' @description  A function for calcualting the average timing of variables (in this case the M10 and L5).  Find the average timing mu that min( sum ( min( (tind_i - mu)^2, (1440 + mu - tind_i )^2 ))) 

#'
#' @param tind \code{numeric} A vector of times which we want to get an average/sd for. The first two columns have to be ID and Day.  
#' @param unit2minute \code{numeric} The ratio of the unit of time and minute. For example, the input unit is hour, the unit2minute = 60. 
#' @param out \code{character} Specify get the mean or sd of the time variables. Default=c("mean","sd") when both mean and sd are calculated.
#'
#' @return mean and sd of the input timing 
#'
#' @importFrom zoo rollapplyr
#'
#' @export
#'
#' @examples
#' x=c(1,1,1,23,23,23) 
#' get_mean_sd_hour(tind=x,  unit2minute=60) 
#' x=12+c(1,1,1,23,23,23) 
#' get_mean_sd_hour(tind=x,  unit2minute=60)   
#' x=c(1:100/5, 20+4:50/200) 
#' get_mean_sd_hour(tind=x,  unit2minute=60)
#' 

 
get_mean_sd_hour <- function(tind, unit2minute=60, out=c("mean","sd")){
        
        
 tind<-tind[which(!is.na(tind))]
 # For GGIR3.1.4 error: L5TIME_num = 39. It should be <36 (tomorrow noon); so return NA for this function.

 if (max(tind)>36) ans<-NA else {
    if(length(tind) == 0){ 
                output=(c(NA, NA))
    } else  {  if(length(tind) == 1){ 
                output=(c(tind, 0))  } else {


 
        # change input to minutes data
        if (unit2minute!=1) tind<-tind*unit2minute
        # tind is from 0 to 24 hours. But L5TIME_NN is 12~36
        if (max(tind)>24*60) {windowshift<-12*60
                                         tind<-tind-12*60 } else windowshift<-0   
       
       
        tmin=0  
        tmax=1440 
        search_len=1441  
        if (max(tind,na.rm=TRUE)>tmax) stop("Wrong tind input with maximum >24 h")
        tgrid <- seq(tmin, tmax, len=search_len)
        dmat  <- outer(tind, tgrid, FUN="-")  #Xij-mu matrix with length(tind)*1441
        dmat  <- pmin(dmat^2, (1440-abs(dmat))^2)
        dsum <- colSums(dmat)
        dmin <- which.min(dsum)
        xmean<-(tgrid[dmin]+ windowshift )/unit2minute  
        xsd <- sqrt( dsum[dmin]/(length(tind)-1) ) /unit2minute 
        output<-c(xmean,xsd)
        }
    }
      
       ans<- output[which(c("mean","sd") %in% out)]
  } #if(max(tind)>36)

       return(ans)
}
 
 


 
