#' @title  Annotating the merged data for all accelerometer files in the GGIR output 
#' @description Annotating the merged ENMO/ANGLEZ data by adding some descriptive variables such as number of valid days and missing pattern.  
#'  
#'
#'  
#' @param studyname  \code{character} Specify the study name that used in the output file names 
#' @param outputdir  \code{character} Directory where the GGIR output was stored.  
#' @param workdir  \code{character} Directory where the output needs to be stored. Note that this directory must exist.
#' @param QCdays.alpha  \code{number}  Minimum required number of valid days in subject specific analysis as a quality control step in part2. Default is 7 days. 
#' @param QChours.alpha  \code{number}  Minimum required number of valid hours in day specific analysis as a quality control step in part2. Default is 16 hours.  
#' @param summaryFN \code{character} Filename with or without directory for sample information in CSV format, which includes summary description of each accelerometer file. Some description will be extracted and merged into the ENMO/ANGLEZ data.  
#' @param epochIn  \code{number}  Epoch size to which acceleration was averaged (seconds) in GGIR output. Defaut is 5 seconds.
#' @param epochOut  \code{number}  Epoch size to which acceleration was averaged (seconds) in part1. Defaut is 60 seconds. 
#' @param useIDs.FN \code{character} Filename with or without directory for sample information in CSV format, which inclues "filename" and "duplicate" in the headlines at least. If duplicate="remove",  the accelerometer files will not be used in the data analysis of part 5-7. Defaut is NULL, which makes all accelerometer files will be used in part 5-7.
#' @param RemoveDaySleeper  \code{logical}  Specify if the daysleeper nights are removed from the calculation of number of valid days for each subject. Default is FALSE. 
#' @param trace  \code{logical}  Specify if the intermediate results is printed when the function was executed. Default is FALSE. 
#' @param Step  \code{number}  Specify which of the varaible need to be cleaned. For example, Step = 1 for the "anglez" variable, and Step = 2 for the "enmo" variable. 
#'
#' @import  xlsx  
#'  
#' @return Files were written to the specified sub-directory, named as flag_ALL_studyname_ENMO.data.Xs.csv and flag_ALL_studyname_ANGLEZ.data.Xs.csv, which Xs is the epoch size to which acceleration was averaged (seconds) in GGIR output.  This excel file includs the following columns, 
#' \item{          filename}{accelerometer file name } 
#' \item{          Date}{date recored from the GGIR part2.summary file }
#' \item{          id}{IDs recored from the GGIR part2.summary file }
#' \item{          calender_date}{date in the format of yyyy-mm-dd }
#' \item{          N.valid.hours}{number of hours with valid data recored from the part2_daysummary.csv file in the GGIR output }
#' \item{          N.hours}{number of hours of measurement recored from the part2_daysummary.csv file in the GGIR output }
#' \item{          weekday}{day of the week-Day of the week }
#' \item{          measurementday}{day of measurement-Day number relative to start of the measurement }
#' \item{          newID}{new IDs defined as the user-defined function of filename2id(), e.g. substrings of the filename } 
#' \item{          Nmiss_c9_c31}{number of NAs from the 9th to 31th column in the part2_daysummary.csv file in the GGIR output} 
#' \item{          missing}{"M" indicates missing for an invalid day, and  "C" indicates completeness for a valid day }
#' \item{          Ndays}{number of days of measurement  }
#' \item{          ith_day}{rank of the measurementday, for example, the value is 1,2,3,4,-3,-2,-1 for measurementday = 1,...,7 }
#' \item{          Nmiss}{number of missing (invalid) days  }
#' \item{          Nnonmiss}{number of non-missing (valid) days } 
#' \item{          misspattern}{indicators of missing/nonmissing for all measurement days at the subject level }
#' \item{          RowNonWear}{number of columnns in the non-wearing matrix }
#' \item{          NonWearMin}{number of minutes of non-wearing}
#' \item{          Nvalid.day}{number of valid days with/without removing daysleeper nights; It is equal to Nnonmiss when RemoveDaySleeper=FALSE.}	
#' \item{          daysleeper}{If 0 then the person is a nightsleeper (sleep period did not overlap with noon) if value=1 then the person is a daysleeper (sleep period did overlap with noon).}  
#' \item{          remove16h7day}{indicator of a key qulity control output. If remove16h7day=1, the day need to be removed. If remove16h7day=0, the day need to be kept.}   
#' \item{          duplicate}{If duplicate="remove",  the accelerometer files will not be used in the data analysis of part5-7.}     
#'
#' 
#' @export 
#'
#'
 


#########################################################################
# create files: flag and impu
#########################################################################  
# (5) data clean
# Just merge summary and enmo, assign flag on remove16h7day column
# Message 1: donot use Row(NA) to do QC since csv already impute the nonwear time. Use part2.summary.
#    idM=part2+duplicate+nonwear
#    useIDs.FN only contribute "duplicate" column ***
# bug=data[-which(--),] get error when which is zero
# part2 summary match the activity data. So we can use part2 in QC.
#########################################################################  

DataShrink<-function(studyname,outputdir,workdir,QCdays.alpha=7,QChours.alpha=16,summaryFN="../summary/part24daysummary.info.csv",epochIn=5,epochOut=60,useIDs.FN=NULL,RemoveDaySleeper=FALSE,trace=FALSE,Step=1){

# remove daysleepers
# remove lines by <16 hours,<7 days (no =)
# keep samples >=7days (each day>=16 hours)
# useIDs.FN is the csv file name including "duplicate" column, remove ids="remove"
# f0=1 ageleZ; f0=1 for enmo
f0<-Step
olddir<-getwd()

setwd(workdir)
on.exit(setwd(workdir)) 
print(getwd())  
   
  listFN<-list.files(workdir,recursive=TRUE)
  csvFN <-listFN[ grep(".csv",listFN )] 
  

  nwFN <-intersect(csvFN[ grep("nonwearscore",csvFN)],
                 csvFN[ grep("900s",csvFN)])  
 
  print(nwFN)
  if (length(nwFN)>1) stop("please choose which nonwear.data should be used?")  



nonwear<-read.csv(nwFN,header=1,stringsAsFactors=F)[,c("filename","Date","RowNonWear")] 
Files <- gsub(pattern= "meta_", replacement = "" ,x= nonwear$filename)
filename2 <- gsub(pattern= ".RData", replacement = "" ,x= Files) 
nonwear[,"filename"]<-filename2  
nonwear[,"NonWearMin"]<-15*nonwear[,"RowNonWear"]


key<-c("ANGLEZ", "ENMO")   
inFN <-paste("All_",studyname,"_",key,".data.csv",sep="") 
outFN1<-paste("flag_","All_",studyname,"_",key,".data.",epochIn,"s.csv",sep="")  
outFN2<-paste("flag_","All_",studyname,"_",key,".data.",epochOut,"s.csv",sep="")  

 

 
 
################################################################ 
#   combine summary and data10m 
#   Date=2015-02-30
#   Nnonmiss= non-missing valid days for each sample
#   bug: read.csv comment.char="# filename" instad of #filename
#   idM=part2+duplicate+nonwear
#   6/1/2020 "calendar_date" was corrected in ggir2.0, and Nmiss_c9_c35
#   If we should remove daysleeper in imputaiton? no?
#   S1<-which(part2ds.nw[,"daysleeper"]==1 | part2ds.nw[,"Nvalid.night"]< QCdays.alpha | part2ds.nw[,"N.valid.hours"]<QChours.alpha ) 
################################################################  
 
print(paste("Begin to read summary file: ",summaryFN,sep=""))
part2ds<-read.csv(summaryFN,header=1,stringsAsFactors=F)
try(colnames(part2ds)[colnames(part2ds)=="calender_date"]<-"calendar_date") 

selC<-c("id","filename","Date","N.valid.hours","N.hours",  "weekday", "measurementday",
"newID" , "Nmiss_c9_c35","missing","Ndays","ith_day","Nmiss",  "Nnonmiss",  "misspattern","daysleeper","sleeponset","wakeup","SleepDurationInSpt"  ) 
 
ds<-part2ds[,selC]  #data summary

# ds[,"Date"]<-substr(ds[,"calendar_date"],1,10)  
part2ds.nw0<-merge(ds,nonwear,by=c("filename","Date"),all=TRUE)
print(table(part2ds.nw0[,"daysleeper"]))


# create Nvalid.day for final imputation...........................
for (f in f0:f0){
 
d<-read.csv(inFN[f],header=1, comment.char = "#",stringsAsFactors=F) 
S0<-which(d[,"filename"]=="# filename") 
if (length(S0)>=1) d<-d[-S0,]
print(paste(c("input data =",dim(d)),collapse=" "))
print(d[1,1]) # skip the # lines in All*csv files.
filename2 <- gsub(pattern= ".RData.csv", replacement = "" ,x= d[,"filename"])    
d[,"filename"]<-filename2

print(paste(f,": read ",ifelse(f==1,"AngleZ","ENMO")," data and prepare headCol",sep=""))
# ------- prepare head info --------
# 16hours7days removing:

S9<-which(paste(part2ds.nw0[, "filename"],part2ds.nw0[,"Date"],sep="@") %in% paste(d[, "filename"],d[,"Date"],sep="@"))
part2ds.nw<- part2ds.nw0[S9,] 
print(table(part2ds.nw[,"daysleeper"]))

#  if removing daysleepers in imputation.-----------------
   part2ds.nw[,"Nvalid.day"]<-NA
   for (k in 1:nrow(part2ds.nw)){
   if (!RemoveDaySleeper) part2ds.nw[k,"Nvalid.day"]<-length(which(part2ds.nw[,"filename"]==part2ds.nw[k,"filename"] & part2ds.nw[,"missing"]=="C"  ))    
   if (RemoveDaySleeper)  part2ds.nw[k,"Nvalid.day"]<-length(which(part2ds.nw[,"filename"]==part2ds.nw[k,"filename"] & part2ds.nw[,"missing"]=="C" & ( is.na(part2ds.nw[,"daysleeper"]) | part2ds.nw[,"daysleeper"]==0)  ))  

   print(c(k,part2ds.nw[k,"Nvalid.day"]))
   }
   print(table(part2ds.nw[,"Nvalid.day"]))
 
part2ds.nw[,"remove16h7day"]<-0  
S1<-which( part2ds.nw[,"Nvalid.day"]< QCdays.alpha | part2ds.nw[,"N.valid.hours"]<QChours.alpha ) 
part2ds.nw[S1,"remove16h7day"]<-1 
table(part2ds.nw[,"remove16h7day"])
dim(part2ds.nw) 

part2ds.nw[,"duplicate"]<-""
if (!is.null(useIDs.FN)) {
useIDs.matrix<-read.csv(useIDs.FN,header=1,stringsAsFactors=F)
S2<-which(part2ds.nw[,"filename"] %in% useIDs.matrix[which(useIDs.matrix[,"duplicate"]=="remove"),"filename"])
part2ds.nw[S2,"duplicate"]<-"remove" 
}
if (trace) print(head(part2ds.nw)) 
#-----------------------------------

dim(d)
d[1:10,1:10]
dmer<-merge(part2ds.nw,d,by=c("filename","Date"),all.x=F,all.y=TRUE,sort=FALSE)
dim(d)
dim(part2ds)
dim(dmer)
if (trace) print(dmer[2,1:40] )

write.csv(dmer,file=outFN1[f],row.names=F)

#################---------------------------------------######################
if (epochIn< epochOut){
  
  print("we will shrink the data")
   
  Nsec<-60/epochIn
  myser<-c(paste(0,0:9,sep=""),10:99)
  timeSer24h<-NULL 
  for (t1 in 1:24)
  for (t2 in 1:60) 
  for (t3 in 1:Nsec)
  timeSer24h<-c(timeSer24h,paste(myser[t1],myser[t2],myser[epochIn*(t3-1)+1],sep=":"))
  length(timeSer24h) 

  Lwindow<-epochOut/epochIn
  Nwindow<-24*3600/epochOut
 
  Data <-read.csv(outFN1[f],header=1,stringsAsFactors=F)
  print(dim(Data))

  StartC<-which(colnames(Data)=="X00.00.00")
  if (!StartC>=1) stop("Check X00.00.00 on the input data")
  Data.trans.imp<-array(NA,dim=dim(Data[,StartC:ncol(Data)]))
  for (j in 1:ncol(Data.trans.imp)) Data.trans.imp[,j]<-as.numeric(Data[,StartC:ncol(Data)][,j])

  Data.head<-Data[,1:(StartC-1)]

  timestamp.switch<-function(x="X00.00.00") {x<-gsub("X","",x)
                                             return(paste(unlist(strsplit(x,"\\.")),collapse=":")) }  
  newColname <- unlist(lapply(colnames(Data[,StartC:ncol(Data)]), timestamp.switch)) 
  colnames(Data.trans.imp)<- newColname

  if ( Nwindow*Lwindow!=ncol(Data.trans.imp)) print ("N*L!=ncol(Data): mismatch due to error or 25hours")
  Data.trans.imp.shrink<-array(NA,dim=c(nrow(Data.trans.imp),Nwindow))
  for (j in 1:Nwindow){
  a= (j-1)*Lwindow+1
  b=a+Lwindow-1 
  abname<-timeSer24h[a:b]
  abname.index<-which(colnames(Data.trans.imp) %in% abname)
  if (length(abname.index)>length(abname) ) { 
      print(c("25hour: ",a,abname[1],length(abname),length(abname.index))) }
  if (nrow(Data.trans.imp)>1 & length(abname.index)>1 ) {
       Data.trans.imp.shrink[,j]<-as.vector(rowMeans(Data.trans.imp[,abname.index],na.rm=TRUE)) } 
  if (nrow(Data.trans.imp)>=1 & length(abname.index)==1 ) {
       Data.trans.imp.shrink[,j]<- Data.trans.imp[,abname.index]  }
  if (nrow(Data.trans.imp)==1 & length(abname.index)>=1 ) {
       Data.trans.imp.shrink[,j]<- mean(Data.trans.imp[,abname.index],na.rm=TRUE) }
  # print(c(j,length(abname.index),Data.trans.imp.shrink[,j]))
  # print(c(j,nrow(Data.trans.imp),length(abname.index)))
 } 
  colnames(Data.trans.imp.shrink)<-timeSer24h[(1:Nwindow-1)*Lwindow+1]
  rownames(Data.trans.imp.shrink)<-rownames(Data.trans.imp)
  Data.after<-cbind(Data.head,Data.trans.imp.shrink)
  if (trace) print(dim(Data.after))
  if (trace) print(Data.after[2,1:40])

  write.csv(Data.after,file=outFN2[f],row.names=F)
  print(paste("Write shrink file into ",outFN2[f],sep=""))
}  #if shrink
 
print("end------------------") 
}  #f
setwd(olddir) 
on.exit(setwd(olddir))  
}
 





