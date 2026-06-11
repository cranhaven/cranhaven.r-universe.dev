#' @title Create a template shell script of postGGIR   
#' @description Create a template shell script of postGGIR, named as STUDYNAME_part0.maincall.R. 
#'  
#'
#'
#'  
#'
#' 
#'
#' @return  The function will create a template shell script of postGGIR in the current directory, names as STUDYNAME_part0.maincall.R
#'
#' @export 
#'



 



create.postGGIR<-function(){
 
inFN<-system.file("template", "STUDYNAME_part0.maincall.txt", package = "postGGIR") 
 
d<-readLines(inFN)  
write(d,file="STUDYNAME_part0.maincall.R",ncolumns=1)  
print(paste("Write a template shell script: STUDYNAME_part0.maincall.R into ",getwd(),sep=""))
}


