#' @title Make a sleep matrix based on the sleep onset and wake up time 
#' @description Make a sleep matrix (sleep=1 and wake=0) based on the sleep onset and wake up time for the purpose of calculating physical acitivy features during wake up time.  
#'  
#'
#'
#' @param sleepFN \code{charcter} The input file name with path of sleep onset and wake up. By default, we use part4_nightsummary_sleep_full.csv under /results/QC folder from GGIR output. 
#' @param epochOut \code{number}  Epoch size to which acceleration was averaged (seconds) in part 3. Defaut is 60 seconds.  
#' @param impute  \code{logical}  Specify if the missing sleep time was imputed based on the averge sleep onset and wake up time. Default is TRUE.  
#' @param outputFN  \code{character}  The output file name that the nonsleep matrix was wrote to. It includs filename, Date, daysleeper, sleeponset, wakeup, oldDate, sleepwindow, sleepimpute, MIN1, MIN2, ..., MIN1440 for the minutes level data when flag.epochOut=60 seconds.
#
#' @return  Sleep matrix and messages of sleep data. 
#' \item{     duplicatedays}{Duplicate days of sleep data if exists}
#' \item{     sleepproblem}{Invalid sleep data if exists}  
#' \item{     sleep matrix (0/1)}{write the sleep matrix to a csv file specified by outputFN} 
#'
#' 
#'
#'  
#'
#'@export
#'
#'

 

 

makeSleepDataMatrix<-function(sleepFN,epochOut=60,impute=TRUE,outputFN){  
 

n1440<-60*60*24/epochOut 
 
sleepFull<-read.csv(sleepFN,header=1,stringsAsFactors=F) 
 
sleepFull[,"filename"]<- gsub(".RData","",sleepFull[,"filename"]) 
# sleepFull[,"Date"]<- format(as.Date(sleepFull[,"calendar_date"] ), "%Y-%m-%d")  # 3.2.0 change date format
sleepFull[,"Date"]<-  NormalizeGGIRDate(sleepFull[,"calendar_date"] ) 

sleepMatrixH<-sleepFull[,c("filename","Date", "daysleeper", "sleeponset","wakeup")] 
sleepMatrixH[,"oldDate"]<-sleepMatrixH[,"Date"]
part4ids<-paste(sleepMatrixH[,"filename"],sleepMatrixH[,"Date"],sep=" ") 
dupID<-part4ids[which(duplicated(part4ids))]

# 1) check duplicate days in the part4_nightsummary_sleep_full.csv; adjust date based on neighbors
reportSleep.dup<-"No duplicate days were observed"  
if (length(dupID)>=1){
for (i in 1:length(dupID)){
S0<-which(part4ids==dupID[i])
work<-sleepMatrixH[which(sleepMatrixH[,"filename"]==sleepMatrixH[S0[1],"filename"]),]

sleepMatrixH[S0,"problem"]<-"duplicate"
thisday<-sleepMatrixH[S0[1],"Date"]
if ( as.character(as.Date(thisday )+1) %in% work[,"Date"]  &   as.character(as.Date(thisday )-1) %in% work[,"Date"]) { 
  sleepMatrixH[S0, "Date"] <- NA
  print("Warning: found both yesterday and next day") } else {
  if ( as.character(as.Date(thisday )+1) %in% work[,"Date"]) sleepMatrixH[S0[1],"Date"]<-as.character(as.Date(sleepMatrixH[S0[1],"Date"] )-1)
  if ( as.character(as.Date(thisday )-1) %in% work[,"Date"]) sleepMatrixH[S0[2],"Date"]<-as.character(as.Date(sleepMatrixH[S0[1],"Date"] )+1)
  }
} 
reportSleep.dup<-sleepMatrixH[which(sleepMatrixH[,"filename"] %in% sleepMatrixH[which(part4ids %in% dupID),"filename"]),]
}  
S1<-which(sleepMatrixH[,"wakeup"]>48) 
if (length(S1)>=1) reportSleep.gt48<- sleepMatrixH[S1,] else  reportSleep.gt48<-"No wake up > 48 was observed" 
                                                               
 


# (2) define sleep=1/wake=0 according to each row (day)
  # note the first day and last day will miss some sleep information. impute them based on other days
  # remove wakeup>48, sleep for two days
  # if no impute, missing data was set to be 0 as wake time
  # add last day since sleep indexed today and next day.
  #  sleepMatrixH:  "filename"  "Date"  "daysleeper" "sleeponset" "wakeup""oldDate"  "problem",sleepwindow

quantile(sleepMatrixH[,"sleeponset"],na.rm=T)
quantile(sleepMatrixH[,"wakeup"],na.rm=T) 
sleepMatrixH[,"sleepwindow"]<-""

filelist<-unique(sleepMatrixH[,"filename"])
outputsleepMatrixH<-NULL
outputsleepMatrixT<-NULL

mypaste<-function(a,b,sep=",") {
if (length(a)==0 | length(a)>=1 && (is.na(a) | a=="")) c=b else c<-paste(a,b,sep=sep) 
return(c)
}


for (f in 1:length(filelist)){

work<-sleepMatrixH[which(sleepMatrixH[,"filename"]==filelist[f]), ] #8 columns
sleeponset.impute<-ifelse(impute, mean(work[,"sleeponset"]),NA)
wakeup.impute<-ifelse(impute, mean(work[,"wakeup"]),NA)
 
firstday<-c(work[1,1],as.character(as.Date(work[1,"Date"] )-1),0,sleeponset.impute,wakeup.impute,as.character(as.Date(work[1,"oldDate"] )-1), NA,"")
lastday<-c(work[nrow(work),1],as.character(as.Date(work[nrow(work),"Date"] )+1),0,sleeponset.impute,wakeup.impute,as.character(as.Date(work[nrow(work),"oldDate"] )+1),NA,"")

work2<-rbind(firstday,work,lastday)

# print(c(f,dim(work),length(firstday)))

 
tempsleepMatrixT<-array(0,dim=c(nrow(work2),n1440))
for (i in 1:nrow(work2)){
 
start = as.numeric(work2[i,"sleeponset"])
end = as.numeric(work2[i,"wakeup"])

today<-i 
nextday<-which(work2[,"filename"]==work2[i,"filename"] & as.Date(work2[,"Date"] )==as.Date(work2[i,"Date"] )+1) 
 

if (!is.na(start) & !is.na(end) & end<=48) {#read only sleep valid lines
if (start>24) start2<-start-24 else start2=start
if (end>24) end2<-end-24 else end2=end
start3<- ceiling(start2*3600/epochOut)  # sleep hour -> minue or 30 seconds
end3<-floor(end2*3600/epochOut)



# Define sleep intervals based on sleeponset and wakeup: 
if (start<24 & end<24) {
tempsleepMatrixT[today,start3:end3]<-1
work2[today,"sleepwindow"]<-mypaste(work2[today,"sleepwindow"],paste(start3,end3,sep="~") ,sep=",") }

if (start>24 & end>24) {
tempsleepMatrixT[nextday,start3:end3]<-1 
work2[nextday,"sleepwindow"]<-mypaste(work2[nextday,"sleepwindow"],paste(start3,end3,sep="~") ,sep=",")}

if (start<24 & end>24) { 
tempsleepMatrixT[today,start3:n1440]<-1 
tempsleepMatrixT[nextday,1:end3]<-1
work2[today,"sleepwindow"]<-mypaste(work2[today,"sleepwindow"],paste(start3,n1440,sep="~") ,sep=",") 
work2[nextday,"sleepwindow"]<-mypaste(work2[nextday,"sleepwindow"],paste(1,end3,sep="~") ,sep=",") }  
} #if wakeup<48 
} #end ith row

work2[,"sleepimpute"]<-0
if (impute) work2[c(1,2,nrow(work2)),"sleepimpute"]<-1
outputsleepMatrixH<-rbind(outputsleepMatrixH,work2[-1,])
outputsleepMatrixT<-rbind(outputsleepMatrixT,tempsleepMatrixT[-1,]) 

} #end fth subject 

dim(sleepMatrixH)
length(filelist)
dim(outputsleepMatrixH) 
dim(outputsleepMatrixT) 
 
 
colnames(outputsleepMatrixT)<- paste("MIN",1:n1440,sep="")  
outputsleepMatrix.all<-cbind(outputsleepMatrixH,outputsleepMatrixT)   
write.csv(outputsleepMatrix.all,file=outputFN,row.names=FALSE)
 
# this is the sleep matrix. Build awake matrix later based on activity matrix IDs. 
return(list(duplicatedays=reportSleep.dup, sleepproblem= reportSleep.gt48))
}


  