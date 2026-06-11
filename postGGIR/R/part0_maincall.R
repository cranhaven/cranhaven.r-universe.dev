#' @title Main Call for Data Processing after Runing GGIR for Accelerometer Data 
#' @description This R script will generate all necessary R/Rmd/shell files for data processing after running GGIR for accelerometer data. 
#'  
#'
#'
#' @param mode \code{number} Specify which of the five parts need to be run, e.g. mode = 0 makes that all R/Rmd/sh files are generated for other parts. When mode = 1, all csv files in the GGIR output directory were read, transformed and then merged. When mode = 2, the GGIR output files were checked and summarized in one excel sheet. When mode = 3, the merged data was cleaned according to the number of valid hours on each night and the number of valid days for each subject. When mode = 4, the cleaned data was imputed. 
#' @param useIDs.FN \code{character} Filename with or without directory for sample information in CSV format, which including "filename" and "duplicate" in the headlines at least. If duplicate="remove",  the accelerometer files will not be used in the data analysis of part 5-7. Defaut is NULL, which makes all accelerometer files will be used in part 5-7.
#' @param currentdir  \code{character} Directory where the output needs to be stored. Note that this directory must exist.  
#' @param studyname  \code{character} Specify the study name that used in the output file names
#' @param bindir  \code{character} Directory where the accelerometer files are stored or list   
#' @param outputdir  \code{character} Directory where the GGIR output was stored.  
#' @param epochIn  \code{number}  Epoch size to which acceleration was averaged (seconds) in GGIR output. Defaut is 5 seconds.
#' @param epochOut  \code{number}  Epoch size to which acceleration was averaged (seconds) in part1. Defaut is 5 seconds.
#' @param flag.epochOut  \code{number}  Epoch size to which acceleration was averaged (seconds) in part 3. Defaut is 60 seconds.  
#' @param log.multiplier  \code{number} The coefficient used in the log transformation of the ENMO data, i.e. log( log.multiplier * ENMO + 1), which have been used in part 5-7. Defaut is 9250. 
#' @param use.cluster  \code{logical}  Specify if part1 will be done by parallel computing. Default is TRUE, and the CSV file in GGIR output will be merged for every 20 files first, and then combined for all. 
#' @param QCdays.alpha  \code{number}  Minimum required number of valid days in subject specific analysis as a quality control step in part2. Default is 7 days. 
#' @param QChours.alpha  \code{number}  Minimum required number of valid hours in day specific analysis as a quality control step in part2. Default is 16 hours. 
#' @param QCnights.feature.alpha  \code{number}  Minimum required number of valid nights in day specific mean and SD analysis as a quality control step in the JIVE analysis. Default is  c(0,0), i.e. no additional data cleaning in this step.  
#' @param Rversion \code{character}  R version, eg. "R/3.6.3". Default is "R". 
#' @param filename2id  \code{R function}  User defined function for converting filename to sample IDs. Default is NULL.  
#' @param PA.threshold  \code{number}  Threshold for light, moderate and vigorous physical activity. Default is c(50,100,400).
#' @param desiredtz \code{charcter}  desired timezone: see also http://en.wikipedia.org/wiki/Zone.tab. Used in g.inspectfile(). Default is "US/Eastern". Used in g.inspectfile() function to inspect acceleromether file for brand, sample frequency in part 2. 
#' @param RemoveDaySleeper  \code{logical}  Specify if the daysleeper nights are removed from the calculation of number of valid days for each subject. Default is FALSE. 
#' @param part5FN   \code{character}  Specify which output is used in the GGIR part5 results. Defaut is "WW_L50M100V400_T5A5", which means that part5_daysummary_WW_L50M100V400_T5A5.csv and part5_personsummary_WW_L50M100V400_T5A5.csv are used in the analysis. 
#' @param NfileEachBundle  \code{number}  Number of files in each bundle when the csv data were read and processed in a cluster. Default is 20. 
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
#' @return  See postGGIR manual for details.
#'
#' @export
#'  
#'
#'





afterggir<-function(mode,useIDs.FN=NULL,currentdir,studyname,bindir=NULL,
outputdir, epochIn=5,epochOut=5,flag.epochOut=60,log.multiplier=9250,use.cluster=TRUE,QCdays.alpha=7,QChours.alpha=16,QCnights.feature.alpha=c(0,0), Rversion="R",filename2id=NULL,PA.threshold=c(50,100,400),desiredtz="US/Eastern",RemoveDaySleeper=FALSE,part5FN="WW_L50M100V400_T5A5",NfileEachBundle=20,trace=FALSE){

print(paste("mode=",mode,sep="")) 
print(paste("useIDs.FN=",useIDs.FN,sep="")) 
print(paste("log.multiplier=",log.multiplier,sep="")) 
print(paste("use.cluster=",use.cluster,sep=""))  

  
setwd(currentdir) 
on.exit(setwd(currentdir))
#require(xlsx)
  
#-------------newfill------------------------ 
nf<- length( list.files(paste(outputdir,"/meta/basic",sep="") ,recursive=TRUE) )
print(paste("There are ",nf," files in /basic folder in total",sep=""))
writedir="data" #for merge csv 

# if use.cluster=TRUE, submit data transform to cluster
Rline.swFN<-"part1_data.transform.R"
swFN<-"part1_data.transform.sw"
swFN2<-"part1_data.transform.merge.sw"
nfeach<-NfileEachBundle # 20 #split in biowulf,~4-5 minutes/file 

#-------------end fill-----------------------
######################################################################### 
# (0) check input files 
######################################################################### 
print("0: Check input files for part1, part2 and part7................")
p1.files<-paste(outputdir,c("/meta/basic", "/meta/csv"),sep="") 
p2.files<-c(paste(outputdir,"/results/", c( "part2_daysummary", "part4_nightsummary_sleep_cleaned",
          paste("part5_daysummary" ,"_",part5FN,sep="")),".csv",sep=""),   
          paste(outputdir,"/results/QC/part4_nightsummary_sleep_full.csv", sep="")  ) 
for (i in 1:length(p1.files)) {
   nfiles<-length(list.files(path=p1.files[i]))
   print(paste(i,": Found ",nfiles," input files in ",p1.files[i],sep=""))
}
for (i in 1:length(p2.files)){
   nfiles<-file.exists(p2.files[i])
   print(paste(i+length(p1.files),": ",ifelse(nfiles,"Found","Miss")," input files of ",p2.files[i],sep=""))
} 

######################################################################### 
# (1a) merge csv by parallel computing 
######################################################################### 

if (0 %in% mode  & use.cluster){
Rline<-c(
"options(width=2000) ",
"argv = commandArgs(TRUE);  ", 
"print(argv) ", 
"print(paste(\"length=\",length(argv),sep=\"\"))  ", 
"f0<-as.numeric(argv[1]) ", 
"f1<-as.numeric(argv[2]) ", 
"mergeVar<-as.numeric(argv[3])  ",  
"print(c(\"f0,f1=\",f0,f1,mergeVar))",
"#################################################", 
"library(postGGIR)", 
paste("studyname <- \"",studyname,"\"",sep=""), 
paste("bindir <- \"",bindir,"\"",sep=""), 
paste("outputdir <- \"",outputdir,"\"",sep=""), 
paste("writedir <- \"",writedir,"\"",sep=""), 
paste("epochIn <- ",epochIn, sep=""), 
paste("epochOut <- ",epochOut, sep=""), 
paste("currentdir <- \"",currentdir,"\"",sep=""),  
"#################################################",
 
"setwd(currentdir)",
"ggir.datatransform(outputdir=outputdir,subdir=writedir,studyname=studyname, numericID=FALSE,sortByid=\"filename\",f0,f1,epochIn=epochIn,epochOut=epochOut,mergeVar=mergeVar)"
)
swline<-paste("# swarm -g 70 -f ",swFN," --module R --time=50:00:00",sep="")
for (i in 1:ceiling(nf/nfeach)) 
swline[i+1]<-paste("   cd ",currentdir,"; module load R ;   R --no-save --no-restore --args  < ",Rline.swFN,"   ",(i-1)*nfeach+1,"   ",i*nfeach,"   ",2,sep="") 

swline<-c(swline,"","","","#### NonWear from Rdata",paste("   cd ",currentdir,"; module load R ;   R --no-save --no-restore --args  < ",Rline.swFN,"   1  ",nf,"  1",sep="") )


catlines<-paste("   cd ",currentdir,"/",writedir,"; cat  ",studyname,"_",c("ENMO","ANGLEZ"),".data*.csv > All_",studyname,"_",c("ENMO","ANGLEZ"),".data.csv",sep="") 

write(Rline,file= Rline.swFN,ncolumns=1)
write(swline,file=  swFN,ncolumns=1)
write(catlines,file=swFN2,ncolumns=1) 
 
print("You need to submit your swarm file to a cluster before running other modes")
}

#########################################################################  
# (1b) csv data transformation 
######################################################################### 
if (1 %in% mode  & !use.cluster){
print("# (1b) csv data transformation without using cluster---------------------------")
print(paste("workdir=",getwd(),sep="")) 
ggir.datatransform(outputdir,subdir=writedir,studyname, numericID=FALSE,sortByid="filename",f0=1,f1=9999,epochIn ,epochOut=epochOut,mergeVar=1)
 
print(paste("workdir=",getwd(),sep="")) 
ggir.datatransform(outputdir,subdir=writedir,studyname, numericID=FALSE,sortByid="filename",f0=1,f1=9999,epochIn ,epochOut=epochOut,mergeVar=2)


catlines<-paste("   cd ",currentdir,"/",writedir,"; cat  ",studyname,"_",c("ENMO","ANGLEZ"),".data*.csv > All_",studyname,"_",c("ENMO","ANGLEZ"),".data.csv",sep="") 
try(system(catlines[1]))
try(system(catlines[2]))
}

#########################################################################  
# (2) Get summary of ggir output
######################################################################### 
if (is.null(part5FN)) part5FN=paste("WW_L",PA.threshold[1],"M",PA.threshold[2],"V",PA.threshold[3],"_T5A5",sep="") #MM 11062020 #WW 12/11

if (2 %in% mode ){
print("# (2a) Get summary of ggir output---------------------------") 
 
ggir.summary(bindir,outputdir,studyname,numericID=FALSE,sortByid="filename",subdir="summary",part5FN=part5FN,QChours.alpha=,QChours.alpha,filename2id=filename2id,desiredtz=desiredtz,trace=trace)
print(" Message: please check duplicate ids and make a csv file (duplicate=remove)") 
 
print("# (2b) plot nonwear vs n.valid.hours---------------------------")
print(" please make sure you have only one NonWear.data*.csv in the data folder") 
call.after.plot(studyname,outputdir,workdir= paste(currentdir,"/",writedir,sep=""),epochOut,trace) 
}
#########################################################################  
# (3) data Shrink
######################################################################### 
if (3 %in% mode ){  
print("# (3a) data clean for ANGLEZ---------------------------")
 
summaryFN<-paste(currentdir,"/summary/part24daysummary.info.csv",sep="")
DataShrink(studyname,outputdir,workdir= paste(currentdir,"/",writedir,sep=""),QCdays.alpha=QCdays.alpha,QChours.alpha=QChours.alpha,summaryFN,epochIn=epochOut,epochOut=flag.epochOut,useIDs.FN=useIDs.FN, RemoveDaySleeper=RemoveDaySleeper,trace=TRUE,Step=1 )

print("# (3b) data clean for ENMO---------------------------")
DataShrink(studyname,outputdir,workdir= paste(currentdir,"/",writedir,sep=""),QCdays.alpha=QCdays.alpha,QChours.alpha=QChours.alpha,summaryFN,epochIn=epochOut,epochOut=flag.epochOut,useIDs.FN=useIDs.FN,RemoveDaySleeper=RemoveDaySleeper,trace=TRUE,Step=2 )
}

if (4 %in% mode ){   
print("# (4) data imputation ---------------------------") 
 
csvInput<-paste("flag_All_",studyname,"_ENMO.data.",flag.epochOut,"s.csv",sep="") 
data.imputation(workdir= paste(currentdir,"/",writedir,sep=""), csvInput )  
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
L$flag.epochOut_xxx<-paste("flag.epochOut=",flag.epochOut,sep="")  
L$log.multiplier_xxx<-paste("log.multiplier=",log.multiplier, sep="")  
L$QCdays.alpha_xxx<-paste("QCdays.alpha=",QCdays.alpha, sep="")   
L$QChours.alpha_xxx<-paste("QChours.alpha=",QChours.alpha, sep="")  
L$PA.threshold_xxx<-paste("PA.threshold=c(",paste(PA.threshold,collapse=","),")", sep="")  
L$QCnights.feature.alpha_xxx<-paste("QCnights.feature.alpha=c(",paste(QCnights.feature.alpha,collapse=","),")", sep="")     
L$part5FN_xxx<-paste("part5FN=\"",part5FN,"\"",sep="")  
L$RemoveDaySleeper_xxx<-paste("RemoveDaySleeper=",RemoveDaySleeper,sep="")  #FALSE

inFN<-"ggir6_afterGGIR.report.Rmd"
edit.report<-function(inFN,outFN4,L){

tail<-c(paste("cd ",currentdir,"   ",sep=""),
paste("R -e \"rmarkdown::render(\'",outFN4,"\'   )\" ",sep=""))
inFN1<-system.file("template", inFN, package = "postGGIR") 
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
inFN<-"part5_postGGIR.report.Rmd"
outFN4a<-paste("part5_",studyname,"_postGGIR.report.Rmd",sep="")  
edit.report(inFN="part5_postGGIR.report.Rmd" ,outFN4a,L) 
#---------------------------------------------------------------
# Make NonWear .Rmd file
 
outFN4b<-paste("part6_",studyname,"_postGGIR.nonwear.report.Rmd",sep="") 
edit.report(inFN="part6_postGGIR.NonWear.report.Rmd" ,outFN4b,L) 
#---------------------------------------------------------------
#  jive1
 
outFN4c<-paste("part7a_",studyname,"_postGGIR_JIVE_1_somefeatures.Rmd",sep="")
edit.report(inFN="part7a_JIVE_1_featureExtraction.Rmd" ,outFN4c,L) 
#---------------------------------------------------------------
#  jive2
 
outFN4d<-paste("part7b_",studyname,"_postGGIR_JIVE_2_allfeatures.Rmd",sep="")
edit.report(inFN="part7b_JIVE_2_featureMerge.Rmd" ,outFN4d,L)   
#--------------------------------------------------------------- 
#  jive4
 
outFN4f<-paste("part7c_",studyname,"_postGGIR_JIVE_3_runJIVE.Rmd",sep="")
edit.report(inFN="part7c_JIVE_3_runJIVE.Rmd" ,outFN4f,L)  
#---------------------------------------------------------------
#  jive5
 
outFN4g<-paste("part7d_",studyname,"_postGGIR_JIVE_4_somefeatures_weekday.Rmd",sep="")
edit.report(inFN="part7d_JIVE_4_somefeatures_weekday.Rmd" ,outFN4g,L)  
 

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

swline<-paste( annoX,"   cd ",currentdir,"; module load ",Rversion," ;   R --no-save --no-restore --args  < ",studyname,"_part0.maincall.R  ", 0:4,sep="") 

outFN4<-paste(studyname,"_postGGIR.report.Rmd",sep="") 

RversionS<-rep(Rversion,length(outFN4_rmd))
RversionS[ length(outFN4_rmd)-1]<-"R/3.6.3" #for r.jive package
shlines<-c( "","","",  
           paste("   cd ",currentdir,"; module load ",RversionS," ; R -e \"rmarkdown::render(\'",outFN4_rmd,"\'   )\" ",sep="")   
          )
swline<-c(sh.head,swline,shlines) 

write(swline,file="part9_swarm.sh",ncolumns=1)
 
}
 
print("--------------End main call----------------")
} 
 
 
 
##  Data processing
 
  
 #   + make square matrix
 #   + Remove days with valid hours <16
 #   + Remove samples with valid days <= 7
 #   + Shrink activity counts into 10 minute window
 #   + Imputation for NAs
 #   + Remove duplicates
 #   + (Log transformation)
   
 #   + Subject mean  
 #   + Merged with phenotype data
 #   + Log transformation


 
