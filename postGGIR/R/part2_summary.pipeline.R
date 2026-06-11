#' @title  Description of all accelerometer files in the GGIR output 
#' @description  Description of all accelerometer files in the GGIR output and this script was executed when mode=2 in the main call.  
#'  
#'
#' 
#' @param bindir  \code{character} Directory where the accelerometer files are stored or list for the purpose of extracting the bin file list. Default=NULL when it is not available and therefore the bin file list is extracted from the /meta/basic folder of the GGIR output.    
#' @param outputdir  \code{character} Directory where the GGIR output was stored.  
#' @param studyname  \code{character} Specify the study name that used in the output file names
#' @param numericID  \code{logical} Specify if the ID is numeric when checking ID errors in part2. Default is FALSE. 
#' @param sortByid  \code{character}  Specify the name of "ID" for each accelerometer file in the report of part2. The value could be "newID","id" and "filename". Defaut is "filename". 
#' @param subdir  \code{character} Sub-directory where the summary output was stored under the current directory. Defaut is "summary". 
#' @param part5FN   \code{character}  Specify which output is used in the GGIR part5 results. Defaut is "WW_L50M125V500_T5A5", which means that part5_daysummary_WW_L50M125V500_T5A5.csv and part5_personsummary_WW_L50M125V500_T5A5.csv are used in the analysis. 
#' @param QChours.alpha  \code{number}  Minimum required number of valid hours in day specific analysis as a quality control step in part2. Default is 16 hours.  
#' @param filename2id  \code{R function}  User defined function for converting filename to sample IDs. Default is NULL.  
#' @param desiredtz \code{charcter}  desired timezone: see also http://en.wikipedia.org/wiki/Zone.tab. Used in g.inspectfile(). Default is "US/Eastern". 
#' @param trace  \code{logical}  Specify if the intermediate results is printed when the function was executed. Default is FALSE.  
#'
#' @import xlsx   
#' @import GGIR  
#' 
#' @return Four files were written to the specified sub-directory
#' \item{      studyname_ggir_output_summary.xlsx}{This excel file includs 9 pages as follows,} 
#'      \item{          page 1}{ List of files in the GGIR output} 
#'      \item{          page 2}{ Summary of files }
#'      \item{          page 3}{ List of duplicate IDs }
#'      \item{          page 4}{ ID errors }
#'      \item{          page 5}{ Number of valid days }
#'      \item{          page 6}{ Table of number of valid/missing days }
#'      \item{          page 7}{ Missing patten }
#'      \item{          page 8}{ Frequency of the missing pattern }
#'      \item{          page 9}{ Description of all accelerometer files}  
#'      \item{          page 10}{ Inspects accelerometer file for key information, including: monitor brand, sample frequency and file header}  
#' \item{      studyname_ggir_output_summary_plot.pdf}{Some plots such as the number of valid days, which were included in the part2a_studyname_postGGIR.report.html file as well.}
#' \item{      part24daysummary.info.csv}{Intermediate results for description of each accelerometer file.}
#' \item{      studyname_samples_remove_temp.csv}{Create studyname_samples_remove.csv file by filling "remove" in the "duplicate" column in this template. If duplicate="remove",  the accelerometer files will not be used in the data analysis of part 5-7.} 
#'
#' @export 
#'
#'
 

  

ggir.summary<-function(bindir=NULL, outputdir,studyname, numericID=FALSE,sortByid="filename",subdir="summary",part5FN="WW_L50M125V500_T5A5",QChours.alpha=16, filename2id=NULL, desiredtz="US/Eastern",trace=FALSE){

olddir<-getwd()
 
if (is.null(filename2id)) {
filename2id<-function(x) {
  y1<-unlist(strsplit(x,"\\_"))[1]
  y2<-unlist(strsplit(y1,"\\."))[1]
  return(y2)
}   
} 


ggir.dir<- outputdir  
workdir<-paste(getwd(),"/",subdir,sep="")
try(system(paste("mkdir ",workdir,sep="")))
setwd(workdir)
on.exit(setwd(workdir)) 
print(workdir)

wg1knife_split<-function(x,k,split="/" ){  #remove folder before bin file
x<- unlist(strsplit(x,split=split))
n<-length(x)
y<-ifelse(k==-1,x[n],x[min(k,n)])
return(y)
}


outFN<-c("c1.filesummary.csv","c2_iderror.info.csv","c3_Nmissing_summary.csv","c4.Nvaliddays.pdf","c4.Nvaliddays.txt",         "c6.dataMissing.pattern.pdf","c9.part24daysummary.info.csv")
BDfn<-paste(studyname,"_ggir_output_summary.xlsx",sep="")
try(system("rm *_ggir_output_summary.xlsx"))
# install.packages("xlsx") 
# library("xlsx")  
##########################################################################################################  
# (1)   file summary in each report
# GGIR:   binary data from 'GENEActiv'   and GENEA devices (not for sale), .csv-export data from 'Actigraph' <https://actigraphcorp.com> devices, and .cwa and .wav-format data from 'Axivity' <https://axivity.com>.  
##########################################################################################################  
   
print("1) file lists starts...")

BD<-NULL #big data as final output  
 
if (!is.null(bindir)){
files0<-list.files(path = bindir,recursive = TRUE)
files1<-unlist(lapply(files0,wg1knife_split,split="/",k=-1))  #remove sub directory
ggirfiletype<-c(".bin",".csv",".cwa",".wav")
S1<-NULL; for (s in 1:length(ggirfiletype)) S1<-c(S1,grep(ggirfiletype[s], files1, fixed=T))
inFN0 <- files1[unique(order(S1))]  
inFN1<-sort(inFN0)
} else { 
files0<-list.files(path = paste(ggir.dir,"/meta/basic",sep="") ,recursive = TRUE) 
inFN0 <-gsub(".RData","",files0)  
inFN0 <-gsub("meta_","",inFN0)  
inFN1<-sort(inFN0) 
}


length(inFN1)  
data.dir2<-paste(ggir.dir,"/results",sep="")   
inFN2<-c("part2_summary","part2_daysummary","part4_summary_sleep_cleaned","part4_nightsummary_sleep_cleaned",
          paste("part5_",c("daysummary","personsummary"),"_",part5FN,sep="") )  
inFN3<-paste(data.dir2,"/",inFN2,".csv",sep="")  
   
#sleepFull.fn<-paste(ggir.dir,"/results/QC/part4_nightsummary_sleep_full.csv",sep="")  #add to impu data.
sleepFull.fn<-inFN3[4]  #use clean file to avoid duplicate days

#######count Nfiles###########################  
ansM<-c(1,"BinFiles",length(inFN1),length(unique(inFN1)),"",paste(length(inFN1)-length(unique(inFN1))," duplicate bin files",sep=""))
BD<-cbind(1:length(unique(inFN1)),unique(inFN1)) 

S<-c("basic","csv","ms2.out","ms3.out","ms4.out","ms5.out")
for (i in 1:length(S)){
xfiles1<-list.files(path = paste(ggir.dir,"/meta/",S[i],sep="")   )
xinFN0 <- xfiles1[grep(".RData", xfiles1, fixed=T)]
xinFN1<-unlist(lapply(xinFN0,wg1knife_split,split="meta_",k=2))  
xinFN1<-unlist(lapply(xinFN1,wg1knife_split,split=".RData",k=1))    
set<-unique(setdiff(inFN1,xinFN1) )  
ansM<-rbind(ansM,c(2,paste("meta-",S[i],sep=""),length(xinFN1),length(unique(xinFN1)),length(set),paste(set,collapse=" | ")))

temp<-rep(".",nrow(BD))
temp[which(BD[,2] %in% set)]<-"X"
BD<-cbind(BD,temp)
if (trace) print(table(temp))
}
 
#######count Nfiles###########################  
 
fn<-list() 
for (i in 1:length(inFN3)){
d<-read.csv(inFN3[i],header=1,stringsAsFactors=F)
colnames(d)[1:10]
fn[[i]]<-d[,"filename"]
fn[[i]]<-unlist(lapply(d[,"filename"],wg1knife_split,split=".RData",k=1))  
set<-unique(setdiff(inFN1,fn[[i]]) )  
ans<-c(i+2,inFN2[i],length(fn[[i]]), length(unique(fn[[i]])),length(set),paste(set,collapse=" | "))
ansM<-rbind(ansM,ans) 

temp<-rep(".",nrow(BD))
temp[which(BD[,2] %in% set)]<-"X"
BD<-cbind(BD,temp)
if (trace) print(table(temp))  
if (i==1) GGIR_version<-as.character(d[1,"GGIR.version"])

} 
 

colnames(ansM)<-c(GGIR_version,"GGIR_folder","Nrow","Nid","Nmiss","Missing_files")
# write.csv(ansM,file=outFN[1],row.names=F)
colnames(BD)<-c("i","filename",S,inFN2) 


p2sum<-read.csv(inFN3[1],header=1,stringsAsFactors=F)[,c(4,5)] #date=2000-11-25, part2_summary
p2sum[,"Date"]<-substr(p2sum[,"start_time"],1,10)
BD<-merge(BD,p2sum[,c(1,3)],by="filename",all=TRUE,sort=F)


write.xlsx(BD, file=BDfn, sheetName = "1_fileList",   col.names = TRUE, row.names = FALSE) 
write.xlsx(ansM, file=BDfn, sheetName = "2_fileSummary",   col.names = TRUE, row.names = FALSE,append=TRUE)

 
##########################################################################################################  
# (2)  read  /part2_daysummary.csv
# bug: colasu has two input for same day on id=6088
##########################################################################################################  
   
print("2) read part2 day summary file")

d<-read.csv(inFN3[2],header=1,stringsAsFactors=F) # part2_daysummary => newID; NA for missing in part2
colnames(d)[which(colnames(d) %in% c("id","ID"))]<-"id"   #6/1/2020 ggir2.0 id->ID

print(c(inFN3[2], dim(d)))
dcopy<-d[order(d[,5],decreasing=TRUE),] # keep good Vhour 
S0<-which(duplicated(paste(dcopy[,2],substr(dcopy[,3],1,10),sep="@")) )   #duplicated(c(1,1,1))= FALSE  TRUE  TRUE
part2.rmDup_ids<-paste(dcopy[S0,2],dcopy[S0,3],sep="@")
 

#  check duplicates and check id match
dim(d)
d[1,1:10]
dim(unique(d[,c(2,3)]))
length(unique(d[,1]))
length(unique(d[,2]))

fn.cd<-paste(d[,2],d[,3],sep="@")
d[,"fn.cd"]<-fn.cd 
d[,"newID"]<-unlist(lapply(d[,2],filename2id)) 
S1<-which(fn.cd %in% part2.rmDup_ids)
if (length(S1)>=1) d<-d[-S1,]   # remove  "6088__027632_2017-02-23 13-05-03.bin@2017-02-06T17:14:55+0100" 

length(unique(fn.cd))
t<-sort(table(fn.cd),decreasing=TRUE)
dupid<-names(t)[which(t>=2)] 
 
dim(d)
dim(unique(d[,c(2,3)]))
length(unique(d[,1]))
length(unique(d[,2]))
length(unique(d[,"newID"]))

idM<-unique(d[,c("filename","newID")])
S2<- which(idM[,2] %in% idM[,2][duplicated(idM[,2])]) 
if (length(S2)>=1) dupIDs<-idM[S2,] else  dupIDs<-"There is no duplicate IDs"
try(write.xlsx(dupIDs, file=BDfn, sheetName = "3_dupIDs",   col.names = TRUE, row.names = FALSE,append=TRUE))

DuplicateIDs<-rep("",nrow(BD))
if (length(S2)>=1) DuplicateIDs[which(BD[,"filename"] %in% dupIDs[,"filename"])]<-"duplicate"
BD<-cbind(BD,DuplicateIDs)


# table(d[,1])
if (numericID) {d.iderror<-d[which(d[,1]!=as.numeric(d[,"newID"])),]} else {  
d.iderror<-d[which( gsub(" ", "", x=d[,1], fixed = TRUE)!=as.character(unlist(d[,"newID"]))),] }
print(typeof(d))
 
#write.csv(unique(d.iderror[,c(1,2,33)]),file=outFN[2],row.names=F)  
write.xlsx(unique(d.iderror[,c("id","filename","newID")]), file=BDfn, sheetName = "4_IDerror",   col.names = TRUE, row.names = FALSE,append=TRUE)
 

IDerrors<-rep("",nrow(BD))
IDerrors[which(BD[,"filename"] %in% d.iderror[,"filename"])]<-"IDerrorInHead"
BD<-cbind(BD,IDerrors)

 
##########################################################################################################  
# (3)   Nmissing days (read  d=part2_daysummary.csv)
##########################################################################################################  
print("3) start to check Nmissing days based on columns 9 to 35.")
mainC=(grep("L5hr",colnames(d))[1]):ncol(d)     # default=9:35=ncol(d) for ggir2.4.0 example data
NmainC<-length(mainC)
idlist<-unique(d[,sortByid])
ansM<-NULL
for (i in 1:length(idlist)){
 if (trace) print(c(i,idlist[i]))
 Swork<-which(d[,sortByid]==idlist[i])
 

 if (length(Swork)>=1){
 work<-d[Swork,] 
 Nmiss<-NULL
 for (j in 1:nrow(work)){  
 Nmiss[j]<-length(which(is.na(work[j,mainC])))
 if (Nmiss[j]<NmainC  & Nmiss[j]>=10)  print(paste("Warning: we found few missingness at ",idlist[i],sep="") )
 
 }#j
 d[Swork,"Nmiss_c9_c35"]<-Nmiss
 

 Nmissline<-length(which(Nmiss==NmainC))
 ans<-c(i,idlist[i],nrow(work),Nmissline,nrow(work)-Nmissline) 
 ansM<-rbind(ansM,ans) 
 }#end if length(Swork) 
}#end i
 
if (trace) print(head(ansM))
ansM<-data.frame(ansM)
colnames(ansM)<-c("i",sortByid,"Ndays","NmissDays","NcompleteDays") 
# write.csv(ansM,file=outFN[3],row.names=F)    
if (trace) print(dim(d))
if (trace) print(length(unique(d[,sortByid])) )
if (trace) print(tail(unique(d[,sortByid])))

write.xlsx(ansM, file=BDfn, sheetName = "5_NvalidDays",   col.names = TRUE, row.names = FALSE,append=TRUE) 
BD<-merge(BD,ansM[,-1],by="filename",all=TRUE,sort=FALSE)

if (trace) print("end this part")

##########################################################################################################  
# (4)   Ndays (read  d=part2_daysummary.csv)
################################################################
##########################################  
print("4) start to check Ndays")
mycbind<-function(A,B){
if (is.null(A)) C<-B else {
C<-array(NA,dim=c(max(nrow(A),nrow(B)),ncol(A)+1+ncol(B)))
C[1:nrow(A),1:ncol(A)]<-A
C[1:nrow(B),ncol(A)+1+1:ncol(B)]<-B
colnames(C)<-c(colnames(A),"space",colnames(B))}
return(C)
}

# write("Days_summurary:",file=outFN[5])
pdf(paste(studyname,"_ggir_output_summary_plot.pdf",sep="")) # ---plot start-----------------
countD<-NULL
for (j in 3:5) {
print(table(ansM[,j]))    
t<-table(ansM[,j]) 
f<-cbind(as.numeric(names(t)),as.numeric(t))  
 

if (nrow(f)>=2) f<-f[order(as.numeric(as.character(f[,1]))),]  
colnames(f)<-c(colnames(ansM)[j],"counts")
 
nf <- length(f[,2])
if (nf>=10){ #plot by removing the highest value
ylim.b<-sort(f[,2],partial=nf-1)[nf-1]
barplot(f[,2],xlab=colnames(f)[1],main=paste(nrow(ansM)," samples",sep=""),names.arg=f[,1],ylab="Frequency")
barplot(f[,2],xlab=colnames(f)[1],main=paste(nrow(ansM)," samples",sep=""),ylim=c(0,ylim.b*1.2),names.arg=f[,1],ylab="Frequency")
}
countD<-mycbind(countD, f)
# write.table(t(f),file=outFN[5],row.names=T,col.names=F,append=TRUE,quote=F)
}
 
write.xlsx(countD, file=BDfn, sheetName = "6_Ndays_table", showNA=FALSE, col.names = TRUE, row.names = FALSE,append=TRUE)
 

##########################################################################################################  
# (7)   Missing pattern based on N.valid.hours
# QChours.alpha=16
##########################################################################################################  
print("5) start to check missing pattern")

missX<-NULL
Nmiss<-d[,"Nmiss_c9_c35"]
print(table(Nmiss,useNA="always")) 
S1<-which(d[,"N.valid.hours"]>=QChours.alpha)
S2<-which(d[,"N.valid.hours"]<QChours.alpha) 
d[,"missing"]<-NA   
d[S1,"missing"]<-"C"  
d[S2,"missing"]<-"M"   
 

  
days.reorder<-function(N,i){
  mid<-N/2
  if (i <= mid) y<-i
  if (i > mid) y<-i-N-1
  return(y)
}
   
t<-table(d[,sortByid])
for (i in 1:nrow(d)){
d[i,"Ndays"]<-t[which(names(t)==d[i,sortByid])] 
d[i,"ith_day"]<-days.reorder(d[i,"Ndays"],d[i,"measurementday"])  
}
 
###########################################################
###########################################################
 
idlist<-unique(d[,sortByid]) 
misspattern<-NULL
Scmc.m<-NULL
for (i in 1:length(idlist)){
 Swork<-which(d[,sortByid]==idlist[i]) 
 work<-d[Swork,]  
 Nmiss<-length(which(work[,"missing"]=="M"))
 Nnonmiss<-length(which(work[,"missing"]=="C")) 
 d[Swork,"Nmiss"]<-Nmiss
 d[Swork,"Nnonmiss"]<-Nnonmiss
 ans<-paste(work[,"missing"],collapse="")
 misspattern<-c(misspattern,ans)
 d[Swork,"misspattern"]<-ans
 if (nrow(work)>=3){
 ans2<-NULL
 for (j in 2:(nrow(work)-1)){
   if ((work[j-1,"missing"]=="C") & (work[j,"missing"]=="M") & (work[j+1,"missing"]=="C")) ans2<-c(ans2,j)}
 Scmc.m<-c(Scmc.m,Swork[ans2])
 #print(length(Scmc.m))
 } #end if work >3 rows
} 

##########################################################################################################  
# (8)   Day sleeper (pay attention to date format for part2 and part4)
#  merge sleep.full report for the purpose of (1) define daysleeper (2) sleep Matrix for PA_dur calculation
# if using clean file, daysleeper=1 : mark; missing will be removed so daysleeper does not matter 
##########################################################################################################  
print(paste("6) Mark day sleeper from ",sleepFull.fn,sep=""))
 
part4<-read.csv(sleepFull.fn,header=1,stringsAsFactors=F)
dim(part4)
dim(unique(part4[,c("filename","calendar_date")]))
if (dim(part4)[1] > dim(unique(part4[,c("filename","calendar_date")]))[1]) stop("Found duplicate days in part4_night_sleep data") 


colnames(part4)[which(colnames(part4) %in% c("id","ID"))]<-"id"   #6/1/2020 ggir2.0 id->ID
part4[,"Date"]<- format(as.Date(part4[,"calendar_date"],"%d/%m/%Y"), "%Y-%m-%d")
part4[,"filename"]<- gsub(".RData","",part4[,"filename"]) 
part4[,"daysleeper_ggir"]<-part4[,"daysleeper"] 
part4[,"daysleeper"]<-0
S4<-which(part4[,"sleeponset"]<36 & part4[,"wakeup"]>36)
if (length(S4)>=1) part4[S4,"daysleeper"]<-1
print(table(part4[,c("daysleeper_ggir","daysleeper")]))

d[,"Date"]<-substr(d[,"calendar_date"],1,10)  
d2 <-merge(d,part4,by=c("filename","Date"),suffix=c("",".2"),all=TRUE)  #filename+ calendar_date 

 
print("   print part2daysummary")
head(d2)
write.csv(d2,file="part24daysummary.info.csv",row.names=F)   #c9.part24daysummary.info.csv
 
 
##########################################################################################################  
# (9)  missing pattern
##########################################################################################################  
print( "7) write to a excel file") 

miss.middle<- misspattern[grep("CMC", misspattern, fixed=T)] 
miss.matrix<-cbind(idlist,misspattern)
colnames(miss.matrix)[1]<-sortByid
Scmc<-which(misspattern %in% miss.middle) 
d.cmc<-d[Scmc.m,]  


write.xlsx(miss.matrix, file=BDfn, sheetName = "7_missingPattern", showNA=FALSE, col.names = TRUE, row.names = FALSE,append=TRUE) 
write.xlsx(sort(table(misspattern),decreasing=TRUE) , file=BDfn, sheetName = "8_missingPattern_table", showNA=FALSE, col.names = TRUE, row.names = FALSE,append=TRUE)


BD<-merge(BD,miss.matrix,by="filename",all=TRUE,sort=FALSE) 
BD[,"filename"]
filename2id
BD[,"newID"]<-unlist(lapply(as.character(unlist(BD[,"filename"])),filename2id)) 


write.xlsx(BD, file=BDfn, sheetName = "9_final", showNA=FALSE, col.names = TRUE, row.names = FALSE,append=TRUE)
print(dim(BD))
BD[,"duplicate"]<-BD[,"DuplicateIDs"]
write.csv(BD, file=paste(studyname, "_samples_remove_temp.csv",sep=""), row.names = FALSE)
print(head(BD))
 
##########################################################################################################  
# (10)  Inspects accelerometer file for monitor brand, sample frequency and file header
########################################################################################################## 
print("8) Inspects monitor brand,frequency")

if (!is.null(bindir)){
inspect<-NULL
RData.files1 <-list.files(path = bindir,recursive = TRUE, full.names = TRUE) 
# RData.files2 <-RData.files1[ grep(".bin",RData.files1)  ] #find .bin.cpgz 
nchar <-nchar(RData.files1)
RData.files2 <-RData.files1[which(substr(RData.files1,nchar-3,nchar) %in% ggirfiletype)]

inspect<-NULL
for ( f in 1:length(RData.files2)){ 
t<-NULL
try(t<-g.inspectfile(datafile= RData.files2[f], desiredtz =  desiredtz ) ) 
if (!is.null(t)){temp<-rbind(c("filename",t$filename), 
            c("monitor brand code",t$monc),
            c("monitor name",t$monn),
            c("data format",t$dformc),
            c("data format code",t$dformn),
            c("sample frequency in Hertz",t$sf)) 
temp<-rbind(temp,cbind(rownames(t$header),as.character(unlist(t$header[,1]))) )
temp2<-t(temp[,2])
colnames(temp2)<-temp[,1]
inspect<-rbind(inspect,temp2) 
}} 
}
if (is.null(bindir)){
part2summary<-read.csv(inFN3[1],header=1,stringsAsFactors=F)
inspect<-part2summary[,c(4,2,3,5:15)]  
inspect[,"monitor.name"]<-inspect[,"device"]
inspect[,"sample.frequency.in.Hertz"]<-inspect[,"samplefreq"] 
inspect[,"Measurement_Period"]<-paste(inspect[,"meas_dur_dys"]*24,"Hours",sep=" ")
}
write.xlsx(inspect, file=BDfn, sheetName = "10_deviceInfo", showNA=FALSE, col.names = TRUE, row.names = FALSE,append=TRUE)

# In part5, we use  monitor.name,sample.frequency.in.Hertz,"Measurement_Period" columns.

########################################################### 
###########################################################

######################  
print( "9) plot missing pattern") 
 
if ( length(which(d[,"missing"]=="C"))>=1 & length(which(d[,"missing"]=="M"))>=1 ){ 
fmissing<-d[which(d[,"missing"]=="M"),] 
fid<-unique(d[,c(sortByid,"Ndays")])

 
hist(d[,"N.valid.hours"],main="N valid hours for all",xlab="Hour")
hist(d[which(d[,"missing"]=="C"),"N.valid.hours"],main="N valid hours for Complete days",xlab="Hour") 
hist(d[which(d[,"missing"]=="M"),"N.valid.hours"],main="N valid hours for missing days",xlab="Hour") 
plot(d[which(d[,"missing"]=="C"),"N.valid.hours"],main="N valid hours for Complete days",ylab="Hour")
plot(d[which(d[,"missing"]=="M"),"N.valid.hours"],main="N valid hours for missing days",ylab="Hour")   
print(paste("The minimum hour for complete days is ",min(d[which(d[,"missing"]=="C"),"N.valid.hours"]),sep=""))
print(paste("The maximum hour for missing days is ",max(d[which(d[,"missing"]=="M"),"N.valid.hours"]),sep=""))

t1<-table(d[which(d[,"measurementday"]==1),"weekday"]) 
t1<-t1[c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")] 
barplot(t1,xlab="First Day",cex.names=0.8)   
hist(fid[,2],xlab="N_valid_days",main="N valid days for all sampes")
barplot(table(fmissing[,"measurementday"]),main="Which day is missing",xlab="measurementday")
t2=table(fmissing[,"ith_day"])
s1<-which(names(t2)>0)
s2<-which(names(t2)<0) 
t2<-t2[c(s1,s2)]
barplot(t2,main="Which day is missing",xlab="ith day")
t5<- table(misspattern) 
t6<-t5[t5>=5] 

 
if (nrow(d.cmc)>=1){
hist(d.cmc[,"N.valid.hours"],xlab="N_valid_hours in missing days",main="missing pattern: ...CMC...") 
par(mai=c(1.02,2.52,0.82,0.42)) # bottom,left,up,right
barplot(t6,main="missing pattern (f>=5)",horiz=TRUE,las=2)
barplot(t6[t6<500],main="missing pattern",horiz=TRUE,las=2)
} 
dev.off()
}

 
setwd(olddir) 
on.exit(setwd(olddir)) 

print("----End summary of ggir output----")
}
 
 

##########################################################################################################  
# ( )   end
##########################################################################################################  





######################################################################################
######################################################################################
  
 
################################################################ 
#  sub functions: plot nonwear vs n.valid.hours
################################################################  
 call.after.plot<-function(studyname,outputdir,workdir,epochOut,trace){
  olddir<-getwd()

  setwd(workdir)
  on.exit(setwd(workdir)) 
  listFN<-list.files(workdir,recursive=TRUE)
  csvFN <-listFN[ grep(".csv",listFN )]  
 
  nwFN <-intersect(csvFN[ grep("nonwearscore",csvFN)],
                 csvFN[ grep("900s",csvFN)])  
 

  print(nwFN)
  if (length(nwFN)>1) stop("please choose which nonwear.data should be used?") 
 
  
  ggir.dir<- outputdir 
  p2FN<-paste(ggir.dir,"/results/part2_daysummary.csv",sep="") 
 

  d1<-read.csv(nwFN,header=1,stringsAsFactors=F)
  d2<-read.csv(p2FN,header=1,stringsAsFactors=F)
  dim(d1)
  dim(d2)
  if (trace) print(head(d1))
  if (trace) print(head(d2))
  d2[,"filename2"]<-paste("meta_",d2[,"filename"],".RData",sep="")
  d2[,"Date"]<-substr(d2[,"calendar_date"],1,10)  # replace calender_date on 6/1/2020
  start<-which(colnames(d1)=="RowNonWear")
  d1[,"RowNonWear23na"]<-rowSums((d1[,(start+1:96)]>=2 | is.na(d1[,start+1:96])),na.rm=TRUE)  
  d1[,"RowNonWear23"]<-rowSums(d1[,(start+1:96)]>=2,na.rm=TRUE)  

  colnames(d1)[colnames(d1)=="filename"]<-"filename2"


d2s<-d2[,c(1:7,ncol(d2)-0:1)]
d1s<-d1[,c(1:4,ncol(d1)-0:1)]

 
d<-merge(d2s,d1s,by=c("filename2","Date"), all=TRUE)


write.csv(d,file="plot.nonwearVSnvalidhours.csv",row.names=F)
pdf("plot.nonwearVSnvalidhours.pdf")
plot(d[,"RowNonWear23na"]/4,d[,"N.valid.hours"],xlab="NonWear Time",ylab="N.valid.hours",main=studyname)
plot(d[,"RowNonWear23"]/4,d[,"N.hours"]-d[,"N.valid.hours"],xlab="NonWear Time",ylab="N.invalid.hours",main=studyname) 
dev.off()

setwd(olddir) 
on.exit(setwd(olddir)) 


}

# Note#################################################################################
#part2 day:
#filename	calendar_date
#__028935_2017-11-28 12-00-38.bin	2017-11-08T01:00:00-0500

#part4 night:
#calendar_date	filename
#26/5/2019	__028935_2017-11-28 12-00-38.bin.RData

#part5 day: 
#filename	window_number	weekday	calendar_date	 
#__028935_2017-11-28 12-00-38.bin.RData	1	Wednesday	4/30/2017
# Note#################################################################################	 

