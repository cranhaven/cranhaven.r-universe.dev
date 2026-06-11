#' @title View phenotype variables 
#' @description This R script will generate plot for each variable and write description to a log file. 
#'  
#'
#' 
#' @param inputFN \code{character} Input file name or input data
#' @param outFN  \code{character} Output pdf file name for the plots   
#' @param csv  \code{logical}  Specify if input file is a CSV file. Default is TRUE. 
#' @param sep \code{character} Separator between columns. Default is space. If csv=TRUE, this will not be used. 
#' @param start  \code{number} The location of the first phenotype variable starts in the input file.
#' @param read  \code{logical}  Specify if inputFN is a file name or a data. Default is TRUE when inputFN is a file name.   
#' @param logFN \code{character}  File name of the log file. Default is NULL, while logFN=paste(inputFN,".log",sep="") in the function.
#' @param track \code{logical}   Specify if the intermediate results is printed when the function was executed. Default is TRUE. 
#'
#'
#' @import kableExtra   
#'
#' @return  Files were written to the current directory. One is .pdf file for plots and the other is .log file for variable description. 
#'
#' @export 
#'
#'







############################################################################
      #        #        #        #        #        #        #        #
    #   #    #  #      #  #     #  #     #  #     #  #     #  #     # #
      #        #        #        #        #        #        #        #
############################################################################
 
 
pheno.plot<-function(inputFN,outFN=paste("plot_",inputFN,".pdf",sep=""),csv=TRUE,sep=" ",start=3,read=TRUE,logFN=NULL,track=TRUE){ 

if (is.null(logFN)) logFN<-paste(inputFN,".log",sep="")

if (read){
if (csv) d<-read.csv(inputFN,header=1,stringsAsFactors=F) else d<-read.table(inputFN,header=1,sep=sep,stringsAsFactors=F) 
} else d<-inputFN

if (track) print(head(d)) 
if (track)  print(dim(d))
Nall=nrow(d) 


write(c(as.character(Sys.time()),inputFN,""),file=logFN,ncolumns=1)  
write(paste(1:ncol(d),colnames(d),sep=":    "),file=logFN,ncolumns=1,append=TRUE)  
write("###################################################",file=logFN,ncolumns=1,append=TRUE) 
 
report<-NULL
pdf(outFN)
if (track) print("plot variables one by one...........")
for (j in start:ncol(d)){
if (track) print(paste("drawing pictures on column",j,sep="  "))
par(mfrow=c(2,2))
on.exit(par(mfrow=c(2,2))) 

x=as.numeric(d[,j])
if (track) print(length( which(!is.na(x))) )
maintitle=paste(colnames(d)[j]," (N=",length(which(!is.na(x)))," / ",Nall,")",sep="")

if (track) print(maintitle)


if (length(which(!is.na(x)))>=1) {
 if (track)  print("This is a numerical variable")

u=round(mean(x,na.rm=T),2)
sd=round(sd(x,na.rm=T),2)
t<-round(quantile(x,na.rm=T),2)
t2<-round(table(x,useNA = "always"),2)

if (length(t2)>=10){
 

plot(x,main=colnames(d)[j],ylab="",xlab="")
hist(x,main=maintitle,ylab="",xlab="")  
boxplot(x,main=maintitle,ylab="",xlab="")  

plot(x=1:10,y=rep(NA,10),xlim=c(0,10),ylim=c(0,10),xlab="",ylab="",axes=FALSE) 
text(5,10,labels=maintitle) 
text(2,9,labels=paste("u=",u,sep="")) 
text(2,8,labels=paste("sd=",sd,sep=""))
text(2,6,labels= "Quantile:" )
text(4,5,labels=paste(c(names(t)[1],"=",t[1]),collapse=" "))  
text(4,4,labels=paste(c(names(t)[2],"=",t[2]),collapse=" "))  
text(4,3,labels=paste(c(names(t)[3],"=",t[3]),collapse=" "))   
text(4,2,labels=paste(c(names(t)[4],"=",t[4]),collapse=" "))  
text(4,1,labels=paste(c(names(t)[5],"=",t[5]),collapse=" "))  
cov.j<-c("Quantitative", paste("u=",u," sd=",sd,sep=""),paste(names(t),t,sep="="))
} else {
plot(x,main=maintitle,ylab="",xlab="") 
hist(x,main=maintitle,ylab="",xlab="")  
plot(x=1:10,y=rep(NA,10),xlim=c(0,10),ylim=c(3,11),xlab="",ylab="",axes=FALSE) 
text(5,10,labels=maintitle) 
text(2,9,labels=paste("u=",u,sep="")) 
text(2,8,labels=paste("sd=",sd,sep=""))
text(2,6,labels= "Count:" )
text(4,5,labels=paste(c(names(t2)),collapse="  "))  
text(4,4,labels=paste( t2,collapse=",")) 
plot.new() 
cov.j<-c("Count", paste(names(t2),t2,sep="="))
}
}
if (length( which(!is.na(x)))==0) { 
par(mfrow=c(1,2))
on.exit( par(mfrow=c(1,2))  )

if (track) print("This is a categorical variable") 
t3<- table(d[,j] ,useNA = "always")
n=length(t3)

barplot(t3 ,main=maintitle,ylab="",xlab="")  
plot(x=1:10,y=rep(NA,10),xlim=c(0,10),ylim=c(1,n+2),xlab="",ylab="",axes=FALSE) 
text(5,n+2,labels=maintitle)  
text(2,n+1,labels= "Count:" )
for (k in 1:length(t3))
text(4,n-k+1,labels=paste(c(names(t3)[k],"=",t3[k]),collapse=" "))  
cov.j<-c("Categorical", paste(names(t3),t3,sep="=")) 
}
cov.j.write<-paste(c(j,maintitle,cov.j),collapse=" | ")
write(cov.j.write,file=logFN,ncolumns=1,append=TRUE) 
report<-rbind(report,cov.j.write)
 
}# jth column
dev.off()
rownames(report)<-NULL
return(report)
}

