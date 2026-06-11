


#' @title  Data imputation for the cleaned data with annotation 
#' @description Data imputation for the merged ENMO data with annotation. The missing values were imputated by the average ENMO over all the valid days for each subject. 
#'  
#'
#'  
#' @param workdir  \code{character} Directory where the output needs to be stored. Note that this directory must exist. 
#' @param csvInput \code{character} File name with or without directory for sample information in CSV format. The ENMO data will be read through read.csv(csvInput,header=1) command, and the missing values were imputated by the average ENMO over all the valid days for each subject at each time point. In this package, csvInput =  flag_All_studyname_ENMO.data.Xs.csv.  If csvInput=NULL, all available data from module 3 will be imputed.
#'
#' @import xlsx  
#'  
#' @return Files were written to the specified sub-directory, named as impu.flag_All_studyname_ENMO.data.Xs.csv, which Xs is the epoch size to which acceleration was averaged (seconds) in GGIR output.  This excel file includs the following columns,
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
#' \item{          daysleeper}{If 0 then the person is a nightsleeper (sleep period did not overlap with noon) if value=1 then the person is a daysleeper (sleep period did overlap with noon).}  	
#' \item{          remove16h7day}{indicator of a key qulity control output. If remove16h7day=1, the day need to be removed. If remove16h7day=0, the day need to be kept.}   
#' \item{          duplicate}{If duplicate="remove",  the accelerometer files will not be used in the data analysis of module5.} 
#' \item{          ImpuMiss.b}{number of missing values on the ENMO data before imputation}	
#' \item{          ImpuMiss.a}{number of missing values on the ENMO data after imputation}		
#' \item{          KEEP}{The value is "keep"/"remove", e.g. KEEP="remove" if remove16h7day=1 or duplicate="remove" or ImpuMiss.a>0} 
#'
#' 
#' @export 
#'
#'
 


#########################################################################  
# (6) data imputation 
#########################################################################  
# workdir<-"/gpfs/gsfs3/users/guow4/projectX1_2018/NCCR_geneActiv/GGIR/afterGGIRxx/data"
# csvInput<-"flag_All_NCCR_ENMO.data.5s.csv"

data.imputation<-function(workdir, csvInput=NULL ){ 
 
# use only "remove16h7day" ==1 lines in imputation and leave other lines same without impu.
olddir<-getwd()
# library(xlsx)   
setwd(workdir)
on.exit(setwd(workdir)) 

if (is.null(csvInput)){ 

csvInput <-list.files(pattern = "^flag_All_*") 
if (length(csvInput )==0) stop("No input files such as flag_All_studyname_ENMO.data.csv")  
epoch<-unlist(lapply(csvInput,function(x) gsub("s","",unlist(strsplit(x,"\\."))[3]))) 
epoch<-max(as.numeric(epoch))
csvInput<-csvInput[grep(epoch,csvInput)]  
}  

findkey<-function(x) { #key do not have . but _ !!!
  t1<-unlist(strsplit(x,"\\_")) 
  n<-length(t1)
  t1[n]<-unlist(strsplit(t1[n],"\\."))[1] 
  return(paste(t1[-c(1,2,3)],collapse="_"))
} 
key<-unlist(lapply(csvInput,findkey))  

 
summaryFiles<-list.files("../summary") 
summaryFN1<-summaryFiles[grep("_ggir_output_summary.xlsx",summaryFiles)] 
summaryP2<-read.xlsx(paste("../summary/",summaryFN1,sep=""),header=0,sheetName = "2_fileSummary")  
ggirV<-as.character(gsub("X","",summaryP2[1,1]))
csvInput2<-gsub(".csv",paste(".GGIR",ggirV,".csv",sep=""),csvInput)
outFN<-paste("impu.",csvInput2,sep="")
outFN2<-paste("IDMatrix.",csvInput2,sep="")
 

for (f in 1:length(csvInput)){

message(paste(f," : data imputation for ",key[f]," data and write to ",outFN[f],"-----------------", sep="")) 
Data.trans<-read.csv(csvInput[f],header=1, stringsAsFactors=F)  
message(paste(c("input=",dim(Data.trans)),collapse=" " )) 

Ctop<-which(colnames(Data.trans)=="X00.00.00")-1   
Data.trans.imp<-Data.trans[,-(1:Ctop)]   # without top columns, only minutes
message(paste("We have ",Ctop," anno columns",sep=""))
idM<- Data.trans[,1:Ctop] 
ImpuMiss.b = rowSums(is.na(Data.trans.imp )) 
message(table(ImpuMiss.b) )
message(table(Data.trans[,"remove16h7day"]))
 

Simpu<-which(idM[,"remove16h7day"]==0) # good days used for imputation
SimpuX<-which(idM[,"remove16h7day"]==0 & ImpuMiss.b>=1) # lines need to be imputed
 

if (length(SimpuX)>=1){ # we do imputation for those clean lines with NA 
message("i, row number, number of NAs before and after imputation,  do imputation") 
for (k in 1:length(SimpuX)){
 i=SimpuX[k]
 Sna.col<- which(is.na(Data.trans.imp[i,])) 
 Ssameid<-intersect(Simpu,which(idM[,"filename"]==idM[i,"filename"]))  
 for (j in Sna.col){ 
    Data.trans.imp[i, j] <-mean(Data.trans.imp[Ssameid,j], na.rm = TRUE) 
    #   message(paste(k,i,j, Data.trans[i, Ctop+j], Data.trans.imp[i, j],sep=","))
 }
 message(c(k,i, ImpuMiss.b[i],  sum(is.na(Data.trans.imp[i,])),"impu")) 
}
}
 
ImpuMiss.a = rowSums(is.na(Data.trans.imp )) 
table(ImpuMiss.a) 


message("second step to Make KEEP variable based on 4 items.") 
KEEP<-rep("keep",nrow(idM))
KEEP[which(is.na(idM[,"newID"]) | idM[,"remove16h7day"]==1 | idM[,"duplicate"]=="remove" |  ImpuMiss.a>=1 )]<-"remove" 

idM.print<-cbind(idM,ImpuMiss.b,ImpuMiss.a,KEEP)
if (key[f]=="ENMO") write.csv(idM.print,file=outFN2[f] ,row.names=F)
idM.print[SimpuX,19:ncol(idM.print)] 
idM.print[-SimpuX,19:ncol(idM.print)] 


Data.trans.imp<-cbind(idM.print,Data.trans.imp)
dim(Data.trans)
dim(Data.trans.imp)
head(Data.trans.imp[,1:10]) 
write.csv(Data.trans.imp,file=outFN[f] ,row.names=F)
} # fth input 
setwd(olddir) 
on.exit(setwd(olddir)) 

}
 