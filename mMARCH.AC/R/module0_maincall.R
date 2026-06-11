#' @title Main Call for Data Processing after Runing GGIR for Accelerometer Data 
#' @description This R script will generate all necessary R/Rmd/shell files for data processing after running GGIR for accelerometer data. 
#'  
#'
#'
#' @param mode \code{number} Specify which of the five modules need to be run, e.g. mode = 0 makes that all R/Rmd/sh files are generated for other modules. When mode = 1, all csv files in the GGIR output directory were read, transformed and then merged. When mode = 2, the GGIR output files were checked and summarized in one excel sheet. When mode = 3, the merged data was cleaned according to the number of valid hours on each night and the number of valid days for each subject. When mode = 4, the cleaned data was imputed. 
#' @param useIDs.FN \code{character} Filename with or without directory for sample information in CSV format, which including "filename" and "duplicate" in the headlines at least. If duplicate="remove",  the accelerometer files will not be used in the data analysis of module 5-7. Defaut is NULL, which makes all accelerometer files will be used in module 5-7.
#' @param currentdir  \code{character} Directory where the output needs to be stored. Note that this directory must exist.  
#' @param studyname  \code{character} Specify the study name that used in the output file names
#' @param bindir  \code{character} Directory where the accelerometer files are stored or list   
#' @param outputdir  \code{character} Directory where the GGIR output was stored.  
#' @param epochIn  \code{number}  Epoch size to which acceleration was averaged (seconds) in GGIR output. Defaut is 5 seconds. 
#' @param epochOut  \code{number}  Epoch size to which acceleration was averaged (seconds) in module 3. Defaut is 60 seconds.  
#' @param log.multiplier  \code{number} The coefficient used in the log transformation of the ENMO data, i.e. log( log.multiplier * ENMO + 1), which have been used in module 5-7. Defaut is 9250. 
#' @param use.cluster  \code{logical}  Specify if module1 will be done by parallel computing. Default is TRUE, and the CSV file in GGIR output will be merged for every 20 files first, and then combined for all. 
#' @param QCdays.alpha  \code{number}  Minimum required number of valid days in subject specific analysis as a quality control step in module2. Default is 7 days. 
#' @param QChours.alpha  \code{number}  Minimum required number of valid hours in day specific analysis as a quality control step in module2. Default is 16 hours. 
#' @param QCnights.feature.alpha  \code{number}  Minimum required number of valid nights in day specific mean, SD, weekday mean and weekend mean analysis as a quality control step in the JIVE analysis. Default is  c(0,0,0,0), i.e. no additional data cleaning in this step.   
#' @param DoubleHour  \code{character}  Specify the method of processing the double hours for days that daylight saving time starts and ends for example. In detail, DoubleHour = c("average","earlier","later"). The acceleration data was averaged on double hours when DoulbeHour="average". Only the acceleration data in the earlier occurrence was remained for double hours while the other duplicate data were ignored when DoulbeHour="earlier". Only the acceleration data in the later occurrence was remained for double hours while the other duplicate data were ignored when DoulbeHour="later".  Default is "average". 
#' @param QC.sleepdur.avg \code{number}  As taking the deault value of QC.sleepdur.avg=c(3,12), individuals were excluded with an average sleep duration <3 hour or >12 hour.  
#' @param QC.nblocks.sleep.avg \code{number}  As taking the deault value of QC.nblocks.sleep.avg=c(6,29), individuals were excluded with an average number of nocturnal sleep episodes < 6 or > 29.  
#' @param Rversion \code{character}  R version, eg. "R/3.6.3". Default is "R". 
#' @param filename2id  \code{R function}  User defined function for converting filename to sample IDs. Default is NULL.  
#' @param PA.threshold  \code{number}  Threshold for light, moderate and vigorous physical activity. Default is c(40,100,400).
#' @param PA.threshold2  \code{number}  Second threshold for light, moderate and vigorous physical activity. Default is c(50,100,400). The activity features will end with "_C2" for those that were calculated based on PA.threshold2.
#' @param desiredtz \code{charcter}  desired timezone: see also http://en.wikipedia.org/wiki/Zone.tab. Used in g.inspectfile(). Default is "US/Eastern". Used in g.inspectfile() function to inspect acceleromether file for brand, sample frequency in module 2. 
#' @param RemoveDaySleeper  \code{logical}  Specify if the daysleeper nights are removed from the calculation of number of valid days for each subject. Default is FALSE. 
#' @param part5FN   \code{character}  Specify which output is used in the GGIR part5 results. Defaut is "WW_L50M100V400_T5A5", which means that part5_daysummary_WW_L50M100V400_T5A5.csv and part5_personsummary_WW_L50M100V400_T5A5.csv are used in the analysis. 
#' @param NfileEachBundle  \code{number}  Number of files in each bundle when the csv data were read and processed in a cluster. Default is 20. 
#' @param holidayFN   \code{character}  Specify the holiday file including filename (optional), Date (mm/dd/year) and holiday (1/0) columns. When it is available, the holiday will be marked into the "weekends" group in weekday/weekend specific feature calculations in module7d. Defaut is NULL. 
#' @param trace  \code{logical}  Specify if the intermediate results is printed when the function was executed. Default is FALSE.
#'
#' @importFrom stats na.omit reshape
#' @importFrom dplyr group_by %>%
#' @importFrom dplyr do as_data_frame filter 
#' @importFrom survival survfit Surv
#' @importFrom ineq Gini 
#' @importFrom grDevices  dev.off pdf rainbow 
#' @importFrom  graphics  axis  barplot  boxplot  hist  lines  par  plot  plot.new  points text
#' @importFrom  stats  quantile
#' @importFrom utils head read.csv read.table tail  write.csv
#' @import xlsx      cosinor refund denseFLMM   
#' @import GGIR
#'
#' @return  See mMARCH.AC manual for details.
#'
#' @export
#'  
#'
#'

 


mMARCH.AC.maincall<-function(mode,useIDs.FN=NULL,currentdir,studyname,bindir=NULL, outputdir, epochIn=5, epochOut=60,log.multiplier=9250,use.cluster=TRUE,QCdays.alpha=7,QChours.alpha=16,QCnights.feature.alpha=c(0,0,0,0),  DoubleHour=c("average","earlier","later")[1], QC.sleepdur.avg=c(3,12),QC.nblocks.sleep.avg=c(6,29), Rversion="R", filename2id=NULL, PA.threshold=c(40,100,400), PA.threshold2=c(50,100,400),desiredtz="US/Eastern", RemoveDaySleeper=FALSE, part5FN="WW_L50M100V400_T5A5", NfileEachBundle=20, holidayFN=NULL,trace=FALSE){


# note for sleep clean: Jones et al cleaned sleep data based on Nblocks of sleep and durations of sleep by removing sleep features for individuals with an average sleep duration <3 h or >12 h or with an average number of nocturnal sleep episodes =5 or =30.  Apply to JIVE (part 7c) after subject-avg feature were calculated.

message(paste("mode=",mode,sep="")) 
message(paste("useIDs.FN=",useIDs.FN,sep="")) 
message(paste("log.multiplier=",log.multiplier,sep="")) 
message(paste("use.cluster=",use.cluster,sep=""))  

  
setwd(currentdir) 
on.exit(setwd(currentdir))
 # require(xlsx)
 # requireNamespace(xlsx)

#-------------newfill------------------------ 
nf<- length( list.files(paste(outputdir,"/meta/basic",sep="") ,recursive=TRUE) )
message(paste("There are ",nf," files in /basic folder in total",sep=""))
writedir="data" #for merge csv 

# if use.cluster=TRUE, submit data transform to cluster
Rline.swFN<-"module1a_data.transform.R"
swFN<-"module1b_data.transform.sw"
swFN2<-"module1c_data.transform.merge.sw"
nfeach<-NfileEachBundle # 20 #split in biowulf,~4-5 minutes/file 

#-------------end fill-----------------------
csvdata<-  list.files(paste(outputdir,"/meta/csv",sep="") ,recursive=TRUE,full.names=TRUE)  
message(paste("There are ",length(csvdata)," files in /csv folder in total",sep=""))
Vnames<-toupper(colnames(read.csv(csvdata[1],header=1,nrow=10))[-1])  

######################################################################### 
# (0) check input files 
######################################################################### 
if (mode==1 | mode==2){
message("")
message("To run mMARCH.AC, you need input some GGIR outputs for module1, module2 and module7................")
if (nf==0) warning(paste("No files were found in GGIR foldes: ",outputdir,"/meta/basic",sep=""))
if (length(csvdata)==0) warning(paste("No csv files were found in GGIR foldes: ",outputdir,"/meta/csv",sep=""))

p1.files<-paste(outputdir,c("/meta/basic", "/meta/csv"),sep="") 
p2.files<-c(paste(outputdir,"/results/", c( "part2_daysummary", "part4_nightsummary_sleep_cleaned",
          paste("part5_daysummary" ,"_",part5FN,sep="")),".csv",sep=""),   
          paste(outputdir,"/results/QC/part4_nightsummary_sleep_full.csv", sep="")  ) 
for (i in 1:length(p1.files)) {
   nfiles<-length(list.files(path=p1.files[i]))
   message(paste(i,": Found ",nfiles," input files in ",p1.files[i],sep=""))
}
for (i in 1:length(p2.files)){
   nfiles<-file.exists(p2.files[i])
   message(paste(i+length(p1.files),": ",ifelse(nfiles,"Found","Miss")," input files of ",p2.files[i],sep=""))
} 

if (!is.null(holidayFN) && file.exists(holidayFN) ) message(paste("7: Found holdiday file of ",holidayFN, sep="")) 


part4<-read.csv(p2.files[2],header=1,stringsAsFactors=F)  
           idsdate<-paste(part4[, "filename"],part4[,"calendar_date"],sep=" ")
           idsdate<-idsdate[duplicated(idsdate)] 
           if (length(idsdate)>=1 )  message(paste("8 Warning: found duplicate days in part4_nightsummary_sleep_cleaned data as follows",idsdate,sep=","))  


message("################## End input checking #################")
}
######################################################################### 
# (1a) merge csv by parallel computing 
######################################################################### 

if (0 %in% mode  & use.cluster){
Rline<-c( 
"argv = commandArgs(TRUE);  ", 
"print(argv) ", 
"print(paste(\"length=\",length(argv),sep=\"\"))  ", 
"f0<-as.numeric(argv[1]) ", 
"f1<-as.numeric(argv[2]) ", 
"mergeVar<-as.numeric(argv[3])  ",  
"print(c(\"f0,f1=\",f0,f1,mergeVar))",
"#################################################", 
"library(mMARCH.AC)", 
paste("studyname <- \"",studyname,"\"",sep=""), 
paste("bindir <- \"",bindir,"\"",sep=""), 
paste("outputdir <- \"",outputdir,"\"",sep=""), 
paste("writedir <- \"",writedir,"\"",sep=""), 
paste("epochIn <- ",epochIn, sep=""), 
paste("epochOut <- ",epochOut, sep=""), 
paste("currentdir <- \"",currentdir,"\"",sep=""),  
paste("DoubleHour <- \"",DoubleHour,"\"",sep=""), 
"#################################################",
 
"setwd(currentdir)",
"ggir.datatransform(outputdir=outputdir,subdir=writedir,studyname=studyname, numericID=FALSE,sortByid=\"filename\",f0,f1 ,epochOut=epochIn,DoubleHour=DoubleHour,mergeVar=mergeVar)"
)
swline<-paste("# swarm -g 70 -f ",swFN," --module R --time=50:00:00",sep="")
for (i in 1:ceiling(nf/nfeach)) 
swline[i+1]<-paste("   cd ",currentdir,"; module load R ;   R --no-save --no-restore --args  < ",Rline.swFN,"   ",(i-1)*nfeach+1,"   ",i*nfeach,"   ",2,sep="") 

swline<-c(swline,"","","","#### NonWear from Rdata",paste("   cd ",currentdir,"; module load R ;   R --no-save --no-restore --args  < ",Rline.swFN,"   1  ",nf,"  1",sep="") )


catlines<-paste("   cd ",currentdir,"/",writedir,"; cat  ",studyname,"_",Vnames,".data*.csv > All_",studyname,"_",Vnames,".data.csv",sep="") 

write(Rline,file= Rline.swFN,ncolumns=1)
write(swline,file=  swFN,ncolumns=1)
write(catlines,file=swFN2,ncolumns=1) 
 
message("You need to submit your swarm file to a cluster before running other modes")
}

#########################################################################  
# (1b) csv data transformation 
######################################################################### 
if (1 %in% mode  & !use.cluster){
message("# (1b) csv data transformation without using cluster---------------------------")
message(paste("workdir=",getwd(),sep="")) 
ggir.datatransform(outputdir,subdir=writedir,studyname, numericID=FALSE,sortByid="filename",f0=1,f1=9999,epochIn ,epochOut=epochIn,DoubleHour=DoubleHour,mergeVar=1)
 
message(paste("workdir=",getwd(),sep="")) 
ggir.datatransform(outputdir,subdir=writedir,studyname, numericID=FALSE,sortByid="filename",f0=1,f1=9999,epochIn ,epochOut=epochIn,DoubleHour=DoubleHour,mergeVar=2)
 

#catlines<-paste("   cd ",currentdir,"/",writedir,"; cat  ",studyname,"_",Vnames,".data*.csv > All_",studyname,"_",Vnames,".data.csv",sep="") 
#for (i in 1:length(Vnames)) try(system(catlines[i]))   #not working in windows

datafn<-list.files(path=paste(currentdir,"/",writedir,sep=""))  
top<-unlist(lapply(datafn,function(x) unlist(strsplit(x,"\\_"))[1]))
datafn<-datafn[which(top==studyname)]
for (i in 1:length(Vnames)) {
     A<-datafn[grep(paste(studyname,"_",Vnames[i],".data",sep=""),datafn)]
     if (length(A)>1) warning(paste("Found multiple files as ",A,collapse=" "))
     B<-paste("All_",studyname,"_",Vnames[i],".data.csv",sep="") 
     file.copy(paste(currentdir,"/",writedir,"/",A,sep=""), paste(currentdir,"/",writedir,"/",B,sep=""))
     message(paste("The merged data ",A," was copied to ",B,sep="")) 
} 
message("************End mode 1 for data merge***************")  
}

#########################################################################  
# (2) Get summary of ggir output
######################################################################### 
if (is.null(part5FN)) part5FN=paste("WW_L",PA.threshold[1],"M",PA.threshold[2],"V",PA.threshold[3],"_T5A5",sep="") #MM 11062020 #WW 12/11

if (2 %in% mode ){
message("# (2a) Get summary of ggir output---------------------------") 
 
ggir.summary(bindir=bindir,outputdir=outputdir,studyname=studyname,numericID=FALSE,sortByid="filename",subdir="summary",part5FN=part5FN,QChours.alpha=QChours.alpha,filename2id=filename2id,desiredtz=desiredtz,trace=trace)
message(" Message: please check duplicate ids and make a csv file (duplicate=remove)") 
 
message("# (2b) plot nonwear vs n.valid.hours---------------------------")
message(" please make sure you have only one NonWear.data*.csv in the data folder") 
call.after.plot(studyname=studyname,outputdir=outputdir,workdir= paste(currentdir,"/",writedir,sep=""),epochOut=epochOut,trace=trace) 
}
#########################################################################  
# (3) data Shrink
#  output= flag_All_studyname_ENMO.data.Xs.csv 
######################################################################### 
if (3 %in% mode ){  
message("# (3) data clean for ENMO,ANGLEZ, etc ---------------------------")
 
summaryFN<-paste(currentdir,"/summary/part24daysummary.info.csv",sep="")
DataShrink(studyname,outputdir,workdir= paste(currentdir,"/",writedir,sep=""),QCdays.alpha=QCdays.alpha,QChours.alpha=QChours.alpha,summaryFN,epochIn=epochIn,epochOut=epochOut,useIDs.FN=useIDs.FN, RemoveDaySleeper=RemoveDaySleeper,trace=trace )
 
}


# mode=4: output=impu.flag_All_studyname_ENMO.data.60s.csv 
if (4 %in% mode ){   
message("# (4) data imputation ---------------------------") 
 
#  csvInput<-paste("flag_All_",studyname,"_ENMO.data.",epochOut,"s.csv",sep="") 
data.imputation(workdir= paste(currentdir,"/",writedir,sep=""), csvInput=NULL )  
}  
 
#########################################################################  
# (4) final report.Rmd file for submit later
#########################################################################
if (0 %in% mode ){   

 
L<-list()
L$date_xxx<-paste("date: \"",as.character(Sys.Date() ),"\"",sep="")  

L$currentdir_xxx<-paste("currentdir=\"",currentdir,"\"",sep="")  
L$studyname_xxx<-paste("studyname=\"",studyname,"\"",sep="")  
L$bindir_xxx<-paste("bindir=\"",bindir,"\"",sep="")  
L$outputdir_xxx<-paste("outputdir=\"",outputdir,"\"",sep="")  
L$epochIn_xxx<-paste("epochIn=",epochIn,sep="")  
L$epochOut_xxx<-paste("epochOut=",epochOut,sep="")  
L$epochOut_xxx<-paste("epochOut=",epochOut,sep="")  
L$log.multiplier_xxx<-paste("log.multiplier=",log.multiplier, sep="")  
L$QCdays.alpha_xxx<-paste("QCdays.alpha=",QCdays.alpha, sep="")   
L$QChours.alpha_xxx<-paste("QChours.alpha=",QChours.alpha, sep="")  
L$PA.threshold_xxx<-paste("PA.threshold=c(",paste(PA.threshold,collapse=","),")", sep="")  
L$PA.threshold2_xxx<-paste("PA.threshold2=c(",paste(PA.threshold2,collapse=","),")", sep="")  
L$QCnights.feature.alpha_xxx<-paste("QCnights.feature.alpha=c(",paste(QCnights.feature.alpha,collapse=","),")", sep="")      
L$QC.sleepdur.avg_xxx<-paste("QC.sleepdur.avg=c(",paste(QC.sleepdur.avg,collapse=","),")", sep="")     
L$QC.nblocks.sleep.avg_xxx<-paste("QC.nblocks.sleep.avg=c(",paste(QC.nblocks.sleep.avg,collapse=","),")", sep="")   

L$part5FN_xxx<-paste("part5FN=\"",part5FN,"\"",sep="")  
L$RemoveDaySleeper_xxx<-paste("RemoveDaySleeper=",RemoveDaySleeper,sep="")  #FALSE
if (is.null(holidayFN)) L$holidayFN_xxx<-"holidayFN=NULL" else L$holidayFN_xxx<-paste("holidayFN=\"",holidayFN,"\"",sep="")  

 
edit.report<-function(inFN,outFN4,L){

tail<-c(paste("cd ",currentdir,"   ",sep=""),
paste("R -e \"rmarkdown::render(\'",outFN4,"\'   )\" ",sep=""))
inFN1<-system.file("template", inFN, package = "mMARCH.AC") 
d1<-readLines(inFN1) 
for (i in 1:length(L)){
S1<-which(gsub(" ", "", d1)==as.character(names(L)[i]))
d1[S1]<-L[[i]]
#print(c(i,S1))
}
d1<-c(d1,tail)
write(d1,file=outFN4,ncolumns=1) 
} 

#---------------------------------------------------------------
 
outFN4a<-paste("module5_",studyname,"_Data_process_report.Rmd",sep="")  
edit.report(inFN="module5_Data_process_report.Rmd" ,outFN4a,L) 
#---------------------------------------------------------------
# Make NonWear .Rmd file
 
outFN4b<-paste("module6_",studyname,"_NonWear.report.Rmd",sep="") 
edit.report(inFN="module6_NonWear.report.Rmd" ,outFN4b,L) 
#---------------------------------------------------------------
#  jive1
 
outFN4c<-paste("module7a_",studyname,"_calculate_newfeatures.Rmd",sep="")
edit.report(inFN="module7a_calculate_newfeatures.Rmd" ,outFN4c,L) 
#---------------------------------------------------------------
#  jive2
 
outFN4d<-paste("module7b_",studyname,"_merge_GGIRfeatures.Rmd",sep="")
edit.report(inFN="module7b_merge_GGIRfeatures.Rmd" ,outFN4d,L)   
#--------------------------------------------------------------- 
#  jive4
 
outFN4f<-paste("module7c_",studyname,"_runJIVE.Rmd",sep="")
edit.report(inFN="module7c_runJIVE.Rmd" ,outFN4f,L)  
#---------------------------------------------------------------
#  jive5
 
outFN4g<-paste("module7d_",studyname,"_calculate_WD_WE_avg_features.Rmd",sep="")
edit.report(inFN="module7d_calculate_WD_WE_avg_features.Rmd" ,outFN4g,L)  
 

outFN4_rmd<-c(outFN4a,outFN4b,outFN4c,outFN4d,   outFN4f,outFN4g)  
#########################################################################  
# (6) sw
#########################################################################
sh.head<-c("#!/bin/bash",
"#",
"#$ -cwd",
"#$ -j y",
"#$ -S /bin/bash",
"  source ~/.bash_profile","","")

annoX<-c("#", ifelse(use.cluster,"#","") ,"","","") # slient mode=0~4

swline<-paste( annoX,"   cd ",currentdir,"; module load ",Rversion," ;   R --no-save --no-restore --args  < ",studyname,"_module0.maincall.R  ", 0:4,sep="")  

RversionS<-rep(Rversion,length(outFN4_rmd))
# R# versionS[ length(outFN4_rmd)-1]<-"R/3.6.3"      #for r.jive package  12-8-23; now jive can run on R 4.2.2 now
shlines<-c( "","","",  
           paste("   cd ",currentdir,"; module load ",RversionS," ; R -e \"rmarkdown::render(\'",outFN4_rmd,"\'   )\" ",sep="")   
          )
swline<-c(sh.head,swline,shlines) 

write(swline,file="module9_swarm.sh",ncolumns=1)
 
}
 
message("--------------End main call----------------")
} 
 
 
  

 
