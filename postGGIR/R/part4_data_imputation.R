


#' @title  Data imputation for the cleaned data with annotation 
#' @description Data imputation for the merged ENMO data with annotation. The missing values were imputated by the average ENMO over all the valid days for each subject. 
#'  
#'
#'  
#' @param workdir  \code{character} Directory where the output needs to be stored. Note that this directory must exist. 
#' @param csvInput \code{character} File name with or without directory for sample information in CSV format. The ENMO data will be read through read.csv(csvInput,header=1) command, and the missing values were imputated by the average ENMO over all the valid days for each subject at each time point. In this package, csvInput =  flag_All_studyname_ENMO.data.Xs.csv. 
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
#' \item{          duplicate}{If duplicate="remove",  the accelerometer files will not be used in the data analysis of part5.} 
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

data.imputation<-function(workdir, csvInput ){ 
 
# use only "remove16h7day" ==1 lines in imputation and leave other lines same without impu.
olddir<-getwd()

setwd(workdir)
on.exit(setwd(workdir)) 
outFN<-paste("impu.",csvInput,sep="")
outFN2<-paste("IDMatrix.",csvInput,sep="")

Data.trans<-read.csv(csvInput,header=1, stringsAsFactors=F)  
print(paste(c("input=",dim(Data.trans)),collapse=" " ))

Ctop<-which(colnames(Data.trans)=="X00.00.00")-1   
Data.trans.imp<-Data.trans[,-(1:Ctop)]  
print(paste("We have ",Ctop," anno columns",sep=""))
idM<- Data.trans[,1:Ctop] 
ImpuMiss.b = rowSums(is.na(Data.trans.imp )) 
print(table(ImpuMiss.b) )
print(table(Data.trans[,"remove16h7day"]))
 

Simpu<-which(idM[,"remove16h7day"]==0) 
SimpuX<-which(idM[,"remove16h7day"]==0 & ImpuMiss.b>=1) 
 

if (length(SimpuX)>=1){ # we do imputation for those clean lines with NA 
for (k in 1:length(SimpuX)){
 i=SimpuX[k]
 Sna.col<- which(is.na(Data.trans.imp[i,])) 
 Ssameid<-intersect(Simpu,which(idM[,"filename"]==idM[i,"filename"]))  
 for (j in Sna.col){ 
    Data.trans.imp[i, j] <-mean(Data.trans.imp[Ssameid,j], na.rm = TRUE) 
    #   print(paste(k,i,j, Data.trans[i, Ctop+j], Data.trans.imp[i, j],sep=","))
 }
 print(c(k,i, ImpuMiss.b[i]," impu")) 
}
}
 
ImpuMiss.a = rowSums(is.na(Data.trans.imp )) 
table(ImpuMiss.a) 


print("imputation part2 ----Make KEEP variable based on 4 items----------------") 
KEEP<-rep("keep",nrow(idM))
KEEP[which(is.na(idM[,"newID"]) | idM[,"remove16h7day"]==1 | idM[,"duplicate"]=="remove" |  ImpuMiss.a>=1 )]<-"remove" 

idM.print<-cbind(idM,ImpuMiss.b,ImpuMiss.a,KEEP)
write.csv(idM.print,file=outFN2 ,row.names=F)
idM.print[SimpuX,19:ncol(idM.print)] 
idM.print[-SimpuX,19:ncol(idM.print)] 


Data.trans.imp<-cbind(idM.print,Data.trans.imp)
dim(Data.trans)
dim(Data.trans.imp)
head(Data.trans.imp[,1:10]) 
write.csv(Data.trans.imp,file=outFN ,row.names=F)
 
setwd(olddir) 
on.exit(setwd(olddir)) 

}


#bug1: colaus 9660 has csv, but no part2summary, so newID=NA from part2. So  remove it from clean data.
 
