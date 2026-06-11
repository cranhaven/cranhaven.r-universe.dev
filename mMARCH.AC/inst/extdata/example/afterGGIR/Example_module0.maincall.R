   

options(width=2000) 
argv = commandArgs(TRUE);  
print(argv) 
print(paste("length=",length(argv),sep=""))  
mode<-as.numeric(argv[1])  
print(c("mode =", mode))
# (Note) Please remove the above lines if you are running this within R console 
#        instead of submitting jobs to a cluster.
 
#########################################################################   
# (user-define 1) you need to redefine this according different study!!!!
######################################################################### 
# example 1 
filename2id.1<-function(x)  unlist(strsplit(x,"\\."))[1] 
 
#  example 2 (use csv file =c("filename","ggirID")) 
filename2id.2<-function(x) {
  d<-read.csv("./mMARCH.AC/inst/extdata/example/filename2id.csv",head=1,stringsAsFactors=F)
  y1<-which(d[,"filename"]==x)
  if (length(y1)==0) stop(paste("Missing ",x," in filename2id.csv file",sep=""))
  if (length(y1)>=1) y2<-d[y1[1],"newID"] 
  return(as.character(y2))
} 


#########################################################################  
#  main call
######################################################################### 
  
mMARCH.AC.shell<-function(mode,filename2id=NULL){ 

library(mMARCH.AC) 
packageVersion("mMARCH.AC")
library(xlsx)
#  ?mMARCH.AC.maincall  # run help to see all argumengts 

#########################################################################  
# (user-define 2) Fill in parameters of your ggir output
########################################################################## 
 

currentdir = "/data/guow4/project0/GGIR/postGGIR/postGGIR_compile/v2/example/afterGGIR"
 
studyname = "Example"
bindir = NULL  
outputdir = "/data/guow4/project0/GGIR/postGGIR/postGGIR_compile/v2/example/GGIR/output_binfile"
part5FN= "WW_L50M100V400_T5A5"  
QCdays.alpha = 0
QChours.alpha = 3  
  

oldwd<-getwd()
setwd(currentdir) 

rmDup=FALSE   # keep all subjects in mMARCH.AC
PA.threshold=c(50,100,400)
#part5FN="WW_L50M100V400_T5A5" 
epochIn = 5
epochOut =  60
use.cluster = FALSE
log.multiplier = 9250
#QCdays.alpha = 7
#QChours.alpha = 16 
QCnights.feature.alpha = c(0,0,0,0)
DoubleHour= "average" 
QC.sleepdur.avg=NULL
QC.nblocks.sleep.avg=NULL
useIDs.FN=NULL 
Rversion="R" 
desiredtz="US/Eastern" 
RemoveDaySleeper=FALSE  
NfileEachBundle=20 
holidayFN=NULL
trace=FALSE
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

#########################################################################  
# (user-define 3 as rmDup=TRUE)  create useIDs.FN file
######################################################################### 
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
#   call afterggir
######################################################################### 

setwd(currentdir)  
mMARCH.AC.maincall(mode=mode,
          useIDs.FN=useIDs.FN,
          currentdir=currentdir,
          studyname=studyname,
          bindir=bindir,
          outputdir=outputdir,
          epochIn=epochIn,
          epochOut=epochOut, 
          log.multiplier=log.multiplier,
          use.cluster=use.cluster,
          QCdays.alpha=QCdays.alpha,
          QChours.alpha=QChours.alpha,
          QCnights.feature.alpha=QCnights.feature.alpha, 
          DoubleHour= DoubleHour,
          QC.sleepdur.avg=QC.sleepdur.avg,
          QC.nblocks.sleep.avg=QC.nblocks.sleep.avg,
          Rversion=Rversion,
          filename2id=filename2id,
          PA.threshold=PA.threshold,
          desiredtz=desiredtz,
          RemoveDaySleeper=RemoveDaySleeper,
          part5FN=part5FN,
          NfileEachBundle=NfileEachBundle,
          holidayFN=holidayFN,
          trace=trace) 

setwd(oldwd)

} 
#########################################################################
         mMARCH.AC.shell(mode)   
######################################################################### 

#   Note:   mMARCH.AC.shell(mode)
#        mode = 0 : creat sw/Rmd file
#        mode = 1 : data transform using cluster or not
#        mode = 2 : summary
#        mode = 3 : clean 
#        mode = 4 : impu
 
 

