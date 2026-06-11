
#' @title  Transform the data and merge all accelerometer files in the GGIR output 
#' @description  An accelerometer file was transformed into wide data matrix, in which the rows represent available days and the columns including all timestamps for 24 hours. Further, the wide data was merged together. 
#'  
#'
#' 
#' @param outputdir  \code{character} Directory where the GGIR output was stored.  
#' @param subdir  \code{character} Sub-directory where the summary output was stored under the current directory. Defaut is "data".  
#' @param studyname  \code{character} Specify the study name that used in the output file names
#' @param numericID  \code{logical} Specify if the ID is numeric when checking ID errors in part2. Default is FALSE. 
#' @param sortByid  \code{character}  Specify the name of "ID" for each accelerometer file in the report of part5. The value could be "newID","id" and "filename". Defaut is "filename".  
#' @param f0  \code{number}  File index to start with (default = 1). Index refers to the filenames sorted in increasing order. 
#' @param f1  \code{number}  File index to finish with. Note that file ends with the minimum of f1 and the number of files available. Default = 1000000. 
#' @param epochIn  \code{number}  Epoch size to which acceleration was averaged (seconds) in GGIR output. Defaut is 5 seconds.
#' @param epochOut  \code{number}  Epoch size to which acceleration was averaged (seconds) in part1. Defaut is 600 seconds. 
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

ggir.datatransform<-function( outputdir,subdir,studyname, numericID=FALSE,sortByid="newID",f0=1,f1=1000000,epochIn=5,epochOut=600,mergeVar=1){

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
print(length(inputFN))
head(inputFN)   
f1<-min(f1,length(inputFN)) 



f0f1<-paste(FIXzero(f0,nfdigit),FIXzero(f1,nfdigit),sep="_")
outFN<-c("filesummary",paste("",f0f1,"_",900,"s.pdf",sep=""),   #nonwear matrix 
          paste("ENMO.data",f0f1,"_",epochOut,"s.csv",sep=""),
          paste("ANGLEZ.data",f0f1,"_",epochOut,"s.csv",sep=""), 
          paste("",f0f1,"_900s.csv",sep="")  #nonwear matrix
         )

outFN<-paste(studyname,outFN,sep="_") 


if (mergeVar==1){ #only output nonwear matrix and plot
write.csv(RData.files2,file=paste(outFN[1],"_Rdatalist.csv",sep=""),row.names=F )
Ymetalong<- c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","temperaturemean","EN")
# Note: ggir2.4.0 names(M$metalong)= "timestamp"     "nonwearscore"  "clippingscore" "en" in hbn data (7.14.2021)





for (y in 2:length(Ymetalong)){
outfn.y<-paste(Ymetalong[y],outFN[5],sep="_") 
print(paste(Ymetalong[y],": " ,outfn.y,sep=""))

nonwear.mer<-NULL  
if (y==2) {pdf(paste(Ymetalong[y],outFN[2],sep="_")) ; ifplot=TRUE} else ifplot=FALSE
for (f in f0:f1){
print(paste(f,"---start-----------------------",RData.files2[f],"------------",sep=""))
T1<-NULL 
T1<-try(single.data.nonwear(Y=Ymetalong[y],ggir.dir=basic.ggir.dir,filename=inputFN[f],epochIn,epochOut,ID=RData.files2[f] ,ifplot=ifplot) )

if (is.null(dim(T1))) print(paste("Fail to extract ",Ymetalong[y]," in the RData in basic folder",sep="")) else {
if (f==f0)  nonwear.mer<-T1 
if (f>f0 )  nonwear.mer<-myrbind(nonwear.mer,T1)  
getwd()
print(paste(c("nonwear.mer=",dim(nonwear.mer)),collapse=" "))
}
}#f 
if (y==2) dev.off()
write.csv(nonwear.mer,file=outfn.y,row.names=F)
print("Get nonwear matrix-------- ")

}
}

#2.1 merge csv files
if (mergeVar==2){ 
write.csv(csvData.files2,file=paste(outFN[1],"_csvlist.csv",sep=""),row.names=F )
enmo.data.mer<-NULL
anglez.data.mer<-NULL

for (f in f0:f1){
print(paste(f,"---start-----------------------",csvData.files2[f],"------------",sep=""))
T2<-NULL
T2<-single.csvdata.transform(ggir.dir=csv.ggir.dir,filename=inputFN[f],epochIn,epochOut )  
 
if (f==f0) {enmo.data.mer<-T2$enmo.shrink 
            anglez.data.mer<-T2$anglez.shrink }
 
if (f>f0){enmo.data.mer<-rbind(enmo.data.mer,T2$enmo.shrink) 
      anglez.data.mer<-rbind(anglez.data.mer,T2$anglez.shrink) }
print(paste(c("enmo.data.mer=",dim(enmo.data.mer)),collapse=" "))
print(paste(c("anglez.data.mer=",dim(anglez.data.mer)),collapse=" ")) 
}# f

  if (f0!=1) colnames(enmo.data.mer)[1]<-paste("# ",colnames(enmo.data.mer)[1],sep="")
  if (f0!=1) colnames(anglez.data.mer)[1]<-paste("# ",colnames(anglez.data.mer)[1],sep="") 
  write.csv(enmo.data.mer,file=outFN[3],row.names=F)
  write.csv(anglez.data.mer,file=outFN[4],row.names=F)   
} #end merge=2

setwd(olddir) 
on.exit(setwd(olddir)) 
print("end----------------------------------------------------------")

}
  
########################################################################################################################
#  subfunctions
########################################################################################################################

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
     if (max(tableTime)==1 )colnames(z)<-time.list 
     if (max(tableTime)==2) colnames(z) <-c(time.list,time.list[addColumn.time] ) 
     rownames(z)<-date.list
     return(z)
   }



######################################################################################
######################################################################################
  


single.data.nonwear<-function(Y,ggir.dir,filename,epochIn,epochOut,ID=filename,ifplot=TRUE){ 
#  ID was the name used in plotting nonwear figures
 
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
  print(c("nonwear=",length(M$metalong$timestamp),length(M$metalong[[Yc]])) )

  Data2 <- as.data.frame(cbind(M$metalong$timestamp, M$metalong[[Yc]]))
  names(Data2) <- c("timestamp2", Y)
  Data2$timestamp <-paste(substr(Data2$timestamp2, 1, 10), 
                           substr(Data2$timestamp2, 12,19), sep = " ")
  Data2$timestamp <-strptime(as.character(Data2[,"timestamp"], stringAsFactor = FALSE),
                               format = "%Y-%m-%d %H:%M:%S")  
  Data2$timezone <- substr(Data2$timestamp2, 21,24)  
  t=table(Data2[,"timezone"])
  if (length(t)==2) print("There are 25 hours due to summer time saving")
 
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
  plot(x.range, Data3[1,], type = "n", ylim = c(0,nrow(Data3)),xaxt = "n",yaxt = "n", ylab = "", xlab = "Hour", 
  col = col.light, main = paste(ID,": ",Y,sep=""))
 
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
  
  ##########################################################  
  return(Data3.frame)

}
 




######################################################################################
######################################################################################
  
single.csvdata.transform<-function(ggir.dir,filename,epochIn,epochOut ){ 

 
  Data.filename <- paste(ggir.dir,filename,sep="/") 
  d<-read.csv(Data.filename,header=1,stringsAsFactors=F)
  dim(d)
  head(d)
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
  #6  convert to subject*time matrix  
  
  enmo.trans <-timestamp2matrix(d,target=2)  
  dim(enmo.trans)
  head(enmo.trans[,1:10])
 
  anglez.trans <-timestamp2matrix(d,target=3)  
  dim(anglez.trans)
  head(anglez.trans[,1:10])
  print(paste(c("raw data=",dim(d)),collapse=" "))  
  print(paste(c("Transform data=",dim(enmo.trans)),collapse=" ")) 

  #########################################################  
  ######################################################### 
  #8 rows specific shrink  
  # complication here: 25 hours issue: take average of duplicate hours!!!!!!!-------------------------!!!!!-----time change------
 
  print("we will shrink the data")
 
  Lwindow<-epochOut/epochIn
  Nwindow<-24*3600/epochOut
  tag.print=0 
  for (u in 1:2) {
  if (u==1) Data.trans.imp=enmo.trans else Data.trans.imp=anglez.trans 
  print(ifelse(u==1,"a) ENMO:","b) anglez:"))
 

  if ( Nwindow*Lwindow!=ncol(Data.trans.imp)) print ("N*L!=ncol(Data): mismatch due to error or 25hours")
  Data.trans.imp.shrink<-array(NA,dim=c(nrow(Data.trans.imp),Nwindow))
  for (j in 1:Nwindow){
  a= (j-1)*Lwindow+1
  b=a+Lwindow-1 
  abname<-timeSer24h[a:b]
  abname.index<-which(colnames(Data.trans.imp) %in% abname)
  if (length(abname.index)>length(abname) & tag.print==0) { 
      tag.print=1   
      print(c("25hour: ",a,abname[1],length(abname),length(abname.index))) }
  if (nrow(Data.trans.imp)>1 & length(abname.index)>1 ) {
       Data.trans.imp.shrink[,j]<-as.vector(rowMeans(Data.trans.imp[,abname.index],na.rm=TRUE)) } 
  if (nrow(Data.trans.imp)>=1 & length(abname.index)==1 ) {
       Data.trans.imp.shrink[,j]<- Data.trans.imp[,abname.index]  }
  if (nrow(Data.trans.imp)==1 & length(abname.index)>=1 ) { 
       Data.trans.imp.shrink[,j]<- mean(Data.trans.imp[,abname.index],na.rm=TRUE)  }
  # print(c(j,length(abname.index),Data.trans.imp.shrink[,j]))
  # print(c(j,nrow(Data.trans.imp),length(abname.index)))
  } 
  colnames(Data.trans.imp.shrink)<-timeSer24h[(1:Nwindow-1)*Lwindow+1]
  rownames(Data.trans.imp.shrink)<-rownames(Data.trans.imp)
  dim(Data.trans.imp.shrink)  
  print(head(Data.trans.imp[,1:10]))
  print(head(Data.trans.imp.shrink[,1:10]))

  if (u==1) enmo.shrink<-Data.trans.imp.shrink else anglez.shrink<-Data.trans.imp.shrink  
  }
  head(enmo.shrink[,1:10])

  ans<-list(enmo=cbind(filename,Date=rownames(enmo.trans),enmo.trans),
            anglez=cbind(filename,Date=rownames(enmo.trans),anglez.trans),
            enmo.shrink=cbind(filename,Date=rownames(enmo.trans),enmo.shrink), 
            anglez.shrink=cbind(filename,Date=rownames(anglez.trans),anglez.shrink) ) 
 
  return(ans)  
}

   



