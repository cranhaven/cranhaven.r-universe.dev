options(width=2000) 
argv = commandArgs(TRUE);  
print(argv) 
print(paste("length=",length(argv),sep=""))  
mode<-as.numeric(argv[1])  
print(c("mode =", mode))
 
#########################################################################  
# (user-define 1) you got to redefine this according different study!!!!
######################################################################### 
 
filename2id.1<-function(x) {
  y1<-unlist(strsplit(x,"\\_"))[1]
  y2<-unlist(strsplit(y1,"\\."))[1]
  return(y2)
} 
 
# nimh (use csv file =c("filename","ggirID")) 
filename2id.2<-function(x) {
  d<-read.csv("./postGGIR/inst/example/filename2id.csv",head=1,stringsAsFactors=F)
  y1<-which(d[,"filename"]==x)
  if (length(y1)==0) stop(paste("Missing ",x," in filename2id.csv file",sep=""))
  if (length(y1)>=1) y2<-d[y1[1],"newID"] 
  return(as.character(y2))
} 


#########################################################################  
#  main call
######################################################################### 
  
call.afterggir<-function(mode,rmDup=FALSE,filename2id=filename2id.1){ 

library(postGGIR) 
#################################################   
# (user-define 2) Fill in parameters of your ggir output
################################################# 
 
 
currentdir =
 
studyname =
bindir = 
outputdir = 
 
epochIn = 5
epochOut = 5
flag.epochOut = 60
use.cluster = FALSE
log.multiplier = 9250
QCdays.alpha = 7
QChours.alpha = 16 
useIDs.FN<-NULL 
setwd(currentdir) 
#########################################################################  
#   remove duplicate sample IDs for plotting and feature extraction 
######################################################################### 
if (mode==3 & rmDup){
# step 1: read ./summary/*remove_temp.csv file (output of mode=2)
keep.last<-TRUE #keep the latest visit for each sample
sumdir<-paste(currentdir,"/summary",sep="")  
setwd(sumdir)  
inFN<-paste(studyname,"_samples_remove_temp.csv",sep="")
useIDs.FN<-paste(sumdir,"/",studyname,"_samples_remove.csv",sep="") 


#################################################   
# (user-define 3 as rmDup=TRUE)  create useIDs.FN file
#################################################  
# step 2: create the ./summary/*remove.csv file manually or by R commands
d<-read.csv(inFN,head=1,stringsAsFactors=F)
d<-d[order(d[,"Date"]),]
d<-d[order(d[,"newID"]),]
d[which(is.na(d[,"newID"])),]
S<-duplicated(d[,"newID"],fromLast=keep.last) #keep the last copy for nccr
d[S,"duplicate"]<-"remove"
write.csv(d,file=useIDs.FN,row.names=F)

} 

#########################################################################  
#  maincall
######################################################################### 

setwd(currentdir)  
afterggir(mode=mode,useIDs.FN,currentdir,studyname,bindir,
outputdir,epochIn,epochOut,flag.epochOut,log.multiplier,use.cluster,QCdays.alpha=QCdays.alpha,QChours.alpha=QChours.alpha, filename2id=filename2id)
  

} 
##############################################
call.afterggir(mode)   
############################################## 

#   Note:   call.afterggir(mode=0)
#        mode =0 : creat sw/Rmd file
#        mode =1 : data transform using cluster or not
#        mode =2 : summary
#        mode =3 : clean 
#        mode =4 : impu
 


