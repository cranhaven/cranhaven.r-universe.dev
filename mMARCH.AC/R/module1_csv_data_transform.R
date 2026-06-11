
#' @title  Transform the data and merge all accelerometer files in the GGIR output 
#' @description  An accelerometer file was transformed into wide data matrix, in which the rows represent available days and the columns including all timestamps for 24 hours. Further, the wide data was merged together. 
#'  
#'
#' 
#' @param outputdir  \code{character} Directory where the GGIR output was stored.  
#' @param subdir  \code{character} Sub-directory where the summary output was stored under the current directory. Defaut is "data".  
#' @param studyname  \code{character} Specify the study name that used in the output file names
#' @param numericID  \code{logical} Specify if the ID is numeric when checking ID errors in module2. Default is FALSE. 
#' @param sortByid  \code{character}  Specify the name of "ID" for each accelerometer file in the report of module5. The value could be "newID","id" and "filename". Defaut is "filename".  
#' @param f0  \code{number}  File index to start with (default = 1). Index refers to the filenames sorted in increasing order. 
#' @param f1  \code{number}  File index to finish with. Note that file ends with the minimum of f1 and the number of files available. Default = 1000000. 
#' @param epochIn  \code{number}  Epoch size to which acceleration was averaged (seconds) in GGIR output. Defaut is 5 seconds.
#' @param epochOut  \code{number}  Epoch size to which acceleration was averaged (seconds) in module1. Defaut is 600 seconds.  
#' @param DoubleHour  \code{character}  Specify the method of processing the double hours for days that daylight saving time starts and ends for example. In detail, DoubleHour = c("average","earlier","later"). The acceleration data was averaged on double hours when DoulbeHour="average". Only the acceleration data in the earlier occurrence was remained for double hours while the other duplicate data were ignored when DoulbeHour="earlier". Only the acceleration data in the later occurrence was remained for double hours while the other duplicate data were ignored when DoulbeHour="later".  Default is "average".  
#' @param mergeVar  \code{number}  Specify which of the varaible need to be processed and merged. For example, mergeVar = 1 makes that the M$metalong varialbes were read from R data on the directory of /meta/basic under GGIR ourput directory, which includes "nonwearscore","clippingscore","lightmean","lightpeak","temperaturemean" and "EN".  When mergeVar = 2, makes that the "enmo" and "anglez" varialbes were read from csv data on the directory of /meta/csv under GGIR ourput directory.
#'
#'   
#' @return
#' \item{     mergeVar = 1}{Six files were written to the specified sub-directory as follows,}
#' \item{       nonwearscore_studyname_f0_f1_Xs.xlsx}{Data matrix of nonwearscore, where f0 and f1 are the file index to start and finish with and Xs is the epoch size to which acceleration was averaged (seconds) in GGIR output.}  
#' \item{       clippingscore_studyname_f0_f1_Xs.xlsx}{Data matrix of clippingscore} 
#' \item{       lightmean_studyname_f0_f1_Xs.xlsx}{Data matrix of lightmean} 
#' \item{       lightpeak_studyname_f0_f1_Xs.xlsx}{Data matrix of lightpeak} 
#' \item{       temperaturemean_studyname_f0_f1_Xs.xlsx}{Data matrix of temperaturemean} 
#' \item{         EN_studyname_f0_f1_Xs.xlsx}{Data matrix of EN} 
#'
#'
#' \item{     mergeVar = 2}{Two files were written to the specified sub-directory as follows,}
#' \item{       studyname_ENMO.dataf0_f1_Xs.xlsx}{Data matrix of ENMO, where f0 and f1 are the file index to start and finish with and Xs is the epoch size to which acceleration was averaged (seconds) in GGIR output.} 
#' \item{       studyname_ANGLEZ.dataf0_f1_Xs.xlsx}{Data matrix of ANGLEZ}   
#'
#'
#' @export 
#'
#'

 


#######################################################################################################################
#M$metalong$nonwearscore
#Knowlegdg:
# metashort imputed short epoch variables
# rout matrix to clarify when data was imputed for each long epoch time window and
# the reason for imputation. Value = 1 indicates imputation. Columns 1 = monitor
# non wear, column 2 = clipping, column 3 = additional nonwear, column 4 =
# protocol based exclusion and column5 = sum of column 1,2,3 and 4.

#complication
# some data has 25 hours due to summer time change
########################################################################################################################


################################################################ 
#  main functions
################################################################  

ggir.datatransform<-function( outputdir,subdir,studyname, numericID=FALSE,sortByid="newID",f0=1,f1=1000000,epochIn=5,epochOut=5, DoubleHour=c("average","earlier","later"), mergeVar=1){

# merge=1 nonwear merge for Rdata; merge=2,csv.merge
# writedir =sub folder under current directory

 
basic.ggir.dir<-paste(outputdir,"/meta/basic",sep="") 
csv.ggir.dir<-paste(outputdir,"/meta/csv",sep="") 

olddir<-getwd()
if (is.null(subdir)) workdir<-"/data"
workdir<-paste(olddir,"/",subdir,sep="") 
try(dir.create(subdir)) 
setwd(workdir)
on.exit(setwd(workdir)) 
#print(paste("workdir=",workdir,sep=""))


wg1knife_split<-function(x,k,split="/" ){
x<- unlist(strsplit(x,split=split))
n<-length(x)
y<-ifelse(k==-1,x[n],x[min(k,n)])
return(y)
}
RData.files1 <- list.files(basic.ggir.dir,recursive=TRUE)
RData.files2 <-sort(RData.files1[ grep(".RData",RData.files1)  ]) #bin.RData 7/6/21
csvData.files1 <- list.files(csv.ggir.dir,recursive=TRUE)
csvData.files2 <-sort(csvData.files1[ grep(".csv",csvData.files1)  ])
nf<-length(csvData.files2)
nfdigit<-nchar(nf)
FIXzero<-function(x,nfdigit) paste(c(rep("0",nfdigit-nchar(x)),x),collapse="")
   
if (mergeVar==1) inputFN=RData.files2 else inputFN=csvData.files2   
message(length(inputFN))
head(inputFN)   
f1<-min(f1,length(inputFN)) 



f0f1<-paste(FIXzero(f0,nfdigit),FIXzero(f1,nfdigit),sep="_")
outFN<-paste(studyname,c("filesummary",
          paste("",f0f1,"_",900,"s.pdf",sep=""),   #nonwear matrix plot
          paste("ENMO.data",f0f1,"_",epochOut,"s.csv",sep=""),
          paste("ANGLEZ.data",f0f1,"_",epochOut,"s.csv",sep=""), #3,4 change to multiple measure 5/11/22
          paste("",f0f1,"_900s.csv",sep="")  #nonwear matrix
         ) ,sep="_") 

if (mergeVar==1){ #only output nonwear matrix and plot

write.csv(RData.files2,file=paste(outFN[1],"_Rdatalist.csv",sep=""),row.names=F )
Ymetalong<- c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","temperaturemean","EN")
# Note: ggir2.4.0 names(M$metalong)= "timestamp"     "nonwearscore"  "clippingscore" "en" in hbn data (7.14.2021)





for (y in 2:length(Ymetalong)){
outfn.y<-paste(Ymetalong[y],outFN[5],sep="_") 
message(paste(Ymetalong[y],": " ,outfn.y,sep=""))

nonwear.mer<-NULL  
if (y==2) {pdf(paste(Ymetalong[y],outFN[2],sep="_")) ; ifplot=TRUE} else ifplot=FALSE
for (f in f0:f1){
message(paste("module1.",y,": ",Ymetalong[y],"---",f,"---start--------------",RData.files2[f],"------------",sep=""))
T1<-NULL 
T1<-try(single.data.nonwear(Y=Ymetalong[y],ggir.dir=basic.ggir.dir,filename=inputFN[f],epochIn,epochOut,ID=RData.files2[f] ,ifplot=ifplot) )

if (is.null(dim(T1))) message(paste("Fail to extract ",Ymetalong[y]," in the RData in basic folder",sep="")) else {
if (f==f0)  nonwear.mer<-T1 
if (f>f0 )  nonwear.mer<-myrbind(nonwear.mer,T1)  
getwd()
message(paste(c(Ymetalong[y],".mer=",dim(nonwear.mer)),collapse=" "))
}
}#f 
if (y==2) dev.off()
write.csv(nonwear.mer,file=outfn.y,row.names=F)
message("Get nonwear matrix-------- ")

}
}

#2.1 merge csv files for enmo, angleZ, X etc........................................................
if (mergeVar==2){ 
write.csv(csvData.files2,file=paste(outFN[1],"_csvlist.csv",sep=""),row.names=F )
enmo.data.mer<-list() 

for (f in f0:f1){
  message(paste("module2 csv data---",f,"---start--------------",csvData.files2[f],"------------",sep=""))
   
  T2<-NULL
  T2<-single.csvdata.transform(ggir.dir=csv.ggir.dir,filename=inputFN[f],epochIn,epochOut,DoubleHour=DoubleHour)  
 
  for (u in 1:length(T2)){ 
    #print(c(f, u, dim(T2[[u]])))
    #print(colnames(T2[[u]] )[1:10])
    #print(head(T2[[u]] )[,c(1:10, ncol(T2[[u]] )-10:0)])
    if (f==f0)  enmo.data.mer[[u]]<-T2[[u]] 
    if (f>f0)   enmo.data.mer[[u]]<-myrbind(enmo.data.mer[[u]],T2[[u]])   #5/2022 bug, T2[[u]]< 1 day => ncol not match
    message(paste(c("              ",f,"-",u," : ", names(T2)[u],".data.mer=",dim(enmo.data.mer[[u]])),collapse=" ")) 
   }
}# f

for (u in 1:length(T2)) { # u th measure
  outFN.enmo<-paste(studyname,"_", toupper(names(T2)[u]), ".data",f0f1,"_",epochOut,"s.csv",sep="")  
  if (f0!=1) colnames(enmo.data.mer[[u]])[1]<-paste("# ",colnames(enmo.data.mer[[u]])[1],sep="") 
  write.csv(enmo.data.mer[[u]],file=outFN.enmo,row.names=F) 
} #add comment to skip later

   
} #end merge=2

setwd(olddir) 
on.exit(setwd(olddir)) 
message("end----------------------------------------------------------")
}
  
########################################################################################################################
#  subfunctions
########################################################################################################################
#   rbind two matrix with different dimension when one data is the subset of another data in the colnames. Merged matrix have same col names of bigger matrix 

myrbind<-function(X,Y){#Y is subset of X
if (ncol(X)==ncol(Y)) Z<-rbind(X,Y)
if (ncol(X)>ncol(Y)){
newY<-array(NA,dim=c(nrow(Y),ncol(X)))
colnames(newY)<-colnames(X)
for (j in 1:ncol(newY)) {
  sj<-which(colnames(Y)==colnames(X)[j])
  if (length(sj)==1) newY[,j]<-Y[,sj]
}
Z<-rbind(X,newY)
}
if (ncol(X)<ncol(Y)){
newX<-array(NA,dim=c(nrow(X),ncol(Y)))
colnames(newX)<-colnames(Y)
for (j in 1:ncol(newX)) {
  sj<-which(colnames(X)==colnames(Y)[j])
  if (length(sj)==1) newX[,j]<-X[,sj]
}
Z<-rbind(newX,Y)
}

return(Z)
}


# output = 18000 columns for 5 seconds data if 25 hours were observed 
timestamp2matrix<-function(Data2,target=2){ #valid only for dup happens only one hour

     Data2$date <- substr(Data2$timestamp, 1, 10)   
     Data2$time <- substr(Data2$timestamp, 12,19)  
     date.list<-sort(unique(Data2$date))
     time.list<-sort(unique(Data2$time))
     find.pos<-function(mom,son) which(mom==son)
     Data2$date.num<-unlist(lapply(Data2$date,find.pos,son=date.list))
     Data2$time.num<-unlist(lapply(Data2$time,find.pos,son=time.list))
     Data2$data.time<-paste(Data2$date.num,Data2$time.num,sep=".")

     tableTime<- table(Data2$data.time) 
     if (max(tableTime)>=3) stop("wrong: found dup 3 times")
     if (max(tableTime)==2){
     Sdup<-names(tableTime)[which(tableTime>=2)] 
     Sdup.index.rm<-NULL
     for (j in 1:length(Sdup))
     Sdup.index.rm[j]<- which(Data2$data.time %in% Sdup[j])[2]
     Sdup.index.rm<- sort(Sdup.index.rm)
 
     Data2$time.num.25h<- Data2$time.num 
     addColumn=length(time.list)+ 1:length(unique(Data2$time.num[Sdup.index.rm]))
     addColumn.time=sort(unique(Data2$time.num[Sdup.index.rm]))
     for (r in  Sdup.index.rm )
     Data2$time.num.25h[r]<- addColumn[which(addColumn.time==Data2$time.num[r] )]
     } 
     if (max(tableTime)==1){ 
     Data2$time.num.25h<- Data2$time.num   
     } 
     Ntime=max(Data2$time.num.25h) 
   

     z<-array(NA,dim=c(length(date.list),Ntime))
     z.index<-cbind(Data2$date.num,Data2$time.num.25h) 
     z[z.index]<-as.numeric(as.character(unlist(Data2[,target])))
     table(Data2[,target])
     table(z) 
     if (max(tableTime)==1) colnames(z)<-time.list 
     if (max(tableTime)==2) colnames(z) <-c(time.list,time.list[addColumn.time] ) 
     rownames(z)<-date.list
     return(z)
}



######################################################################################
######################################################################################
  


single.data.nonwear<-function(Y,ggir.dir,filename,epochIn,epochOut,ID=filename,ifplot=TRUE){ 
#  ID was the name used in plotting nonwear figures
 
  oldpar <- par(no.readonly = TRUE)



  RData.filename <- paste(ggir.dir,filename,sep="/") 
  M<-NULL
  load(RData.filename)
  names(M) 
  names(M$metashort) 
  names(M$metalong) 
  Yc<-which( tolower( names(M$metalong) ) ==tolower(Y) ) #2.4 EN->en
  if (length(Yc)==0) stop(paste("Warning: Cannot find ",Y," variable in", RData.filename,sep=""))

  ##############################################
  #2 nowearscore
  message(c("nonwear=",length(M$metalong$timestamp),length(M$metalong[[Yc]])) )

  Data2 <- as.data.frame(cbind(M$metalong$timestamp, M$metalong[[Yc]]))
  names(Data2) <- c("timestamp2", Y)
  Data2$timestamp <-paste(substr(Data2$timestamp2, 1, 10), 
                           substr(Data2$timestamp2, 12,19), sep = " ")
  Data2$timestamp <-strptime(as.character(Data2[,"timestamp"], stringAsFactor = FALSE),
                               format = "%Y-%m-%d %H:%M:%S")  
  Data2$timezone <- substr(Data2$timestamp2, 21,24)  
  t=table(Data2[,"timezone"])
  if (length(t)==2) message("There are 25 hours due to summer time saving")
 
  head(Data2)
  dim(Data2)
 ##############################################  
  #3 plot nonwear: Data3,square matrix of data2 
  Data3<-timestamp2matrix(Data2,target=2)
  if (ifplot){
  par(mai=c(1.02,1.52,0.82,0.42)) # bottom,left,up,right
  par(las=1)
  col.light = rainbow(nrow(Data3))
  x.range = 1:ncol(Data3)
  try(plot(x.range, Data3[1,], type = "n", ylim = c(0,nrow(Data3)),xaxt = "n",yaxt = "n", ylab = "", xlab = "Hour", 
  col = col.light, main = paste(ID,": ",Y,sep="")))
 
  pchS<-c("1","2","3","4","5")
  for(i in  1:nrow(Data3)) {
  yi<-nrow(Data3)-i+1
  lines(x=x.range, y=rep(yi,length(x.range)), col = col.light[i], type = "l", lwd = 1,lty=3) 
  for (j in 1:5){
  S<- which(Data3[i,]==j)  
  points(x=S,y=rep(yi,length(S)),pch=pchS[j],col = col.light[i])
  }
 
  } 
  mylabels = c("01:00", "04:00", "08:00", "12:00", "16:00", "20:00", "23:00")
  mylabels.at<-which(colnames(Data3) %in% paste(mylabels,":00",sep=""))
  mylabels = substr(colnames(Data3)[mylabels.at],1,5)
 
  axis(1, at = mylabels.at,labels = mylabels)   
  axis(2, at = 1:nrow(Data3),labels = rownames(Data3)[nrow(Data3):1],las=2) 
 }
 ##############################################
 
  RowNonWear<- rowSums((Data3>=1),na.rm=TRUE)  
  if (ncol(Data3)==100) colnames(Data3)[97:100]<-paste("tc_",colnames(Data3)[97:100],sep="")
  Data3.frame<-  cbind(filename,Date=rownames(Data3),RowNonWear,Data3) 
  on.exit(par(oldpar)) 
  ##########################################################  
  return(Data3.frame)

}
 




###################################################################################### 
# d<-read.csv("/vf/users/guow4/ProjectX2_2021/0_mMARCH_actigraph/VanMeterAnna/GGIR/output_BinFiles/meta/csv/100002_left wrist_034096_2021-07-12 15-22-50.bin.RData.csv",header=1,stringsAsFactors=1)
######################################################################################
  

DoubleHour.align<-function(data,DoubleHour=c("average","earlier","later")){
  # data= t1, t1, t1, t1 (same timestamp) 
  if (DoubleHour=="average"){
   if (nrow(data)>1)   data.align<-as.vector(rowMeans(data,na.rm=TRUE))  
   if (nrow(data)==1)  data.align<-mean(data,na.rm=TRUE)  
  }
  if (DoubleHour=="earlier")  data.align<- data[,1]   
  if (DoubleHour=="later")  data.align<- data[,ncol(data)]   
  return(data.align)
}



single.csvdata.transform<-function(ggir.dir,filename,epochIn,epochOut, DoubleHour=c("average","earlier","later") ){ 

 
  Data.filename <- paste(ggir.dir,filename,sep="/") 
  d<-read.csv(Data.filename,header=1,stringsAsFactors=F)
 
 
  ##############################################
  Nsec<-60/epochIn
  myser<-c(paste(0,0:9,sep=""),10:99)
  timeSer24h<-NULL

  for (t1 in 1:24)
  for (t2 in 1:60) 
  for (t3 in 1:Nsec)
  timeSer24h<-c(timeSer24h,paste(myser[t1],myser[t2],myser[epochIn*(t3-1)+1],sep=":"))
  length(timeSer24h) 
    
  #########################################################
  #########################################################  
  #6  convert to subject*time matrix  
  # d columns: BFEN,ZCX,ZCY,ZCZ,anglex,angley,anglez,ENMO,MAD



outputlist<-list()
for (u in 2:ncol(d)) {
  
  Vname<-colnames(d)[u] 
  data.trans <-timestamp2matrix(d,target=u) #17280 or  + one hour columns for 5s data  
  message(paste("a",u,") start timestamp2matrix() on ",u,"th column: ",Vname," ----------------",sep="")) 
  message(paste(c("raw data=",dim(d)),collapse=" "))  
  message(paste(c("Transform data=",dim(data.trans)),collapse=" ")) 

 
  ######################################################### 
  #6a align the data for double hours or less than one day
 
 if (ncol(data.trans)*epochIn/3600 !=24){ 
  
  message ("Warning: mismatch due to a day with < or > 24 hours (time change) and we will align the data")
  Nwindow<-24*3600/epochIn
  data.trans.align<-array(NA,dim=c(nrow(data.trans),Nwindow))
  tag.print=0 

  for (j in 1:Nwindow){
 
  abname<-timeSer24h[j]
  abname.index<-which(colnames(data.trans) %in% abname)
  if (length(abname.index)>1 & tag.print==0) { 
      tag.print=1   
      message(paste("25 hour: first duplicate timestamp was found at ",abname[1],"with",length(abname.index),"columns",sep=" ")) }

  if (length(abname.index)==0)  data.trans.align[,j]<- NA   
  if ( length(abname.index)==1 ) {
       data.trans.align[,j]<- data.trans[,abname.index]  } 
  if ( length(abname.index)>=2 ) { 
       data.trans.align[,j]<- DoubleHour.align(data=data.trans[,abname.index],DoubleHour=DoubleHour) 
       #print(c(j,data.trans[10,abname.index],data.trans.align[10,j]))
       }

   } 
  colnames(data.trans.align)<-timeSer24h 
  rownames(data.trans.align)<-rownames(data.trans) 
 
  } else  data.trans.align<-data.trans

 

  ######################################################### 
  #6b  shrink the data by taking columns average

  if (epochIn < epochOut ){

  message(paste("we will shrink the data from ",epochIn," seconds to ",epochOut," seconds",sep=""))
  Lwindow<-epochOut/epochIn
  Nwindow<-24*3600/epochOut 

  if ( Nwindow*Lwindow!=ncol(data.trans.align)) stop ("N*L!=ncol(Data): mismatch due to a day with < or > 24 hours (time change)")
  data.trans.align.shrink<-array(NA,dim=c(nrow(data.trans.align),Nwindow))
  for (j in 1:Nwindow){
  a= (j-1)*Lwindow+1
  b=a+Lwindow-1 
  abname<-timeSer24h[a:b]
  abname.index<-which(colnames(data.trans.align) %in% abname) 
  data.trans.align.shrink[,j]<- DoubleHour.align(data=data.trans.align[,abname.index],DoubleHour="average") 
  # print(c(j,data.trans.align[10,abname.index],data.trans.align[10,j])) 

  } 
  colnames(data.trans.align.shrink)<-timeSer24h[(1:Nwindow-1)*Lwindow+1]
  rownames(data.trans.align.shrink)<-rownames(data.trans.align)
 
  } else  data.trans.align.shrink<-data.trans.align

  outputlist[[u-1]]<-cbind(filename,Date=rownames(data.trans.align.shrink),data.trans.align.shrink)
  names(outputlist)[u-1]<-Vname
  message(paste(c("Output Transform data=",dim(data.trans.align.shrink)),collapse=" ")) 

} #end uth Vname
  
 
  return(outputlist)  
}

   



