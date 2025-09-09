#' @title Export a pedigree to a file
#' @description Export a pedigree to a file
#' @param ped pedigree
#' @param fname file name
#' @param iMarker index of marker to be used
#' @return pedigree with Mendelian errors eliminated
#' @export
exportPed<-function(ped,fname,iMarker=1){
 a <- ped$pedigree
 b<-c()
 #for(i in 1:ped$nMark){
  alelo <- gsub(".","",c(attr(ped$markerdata[[iMarker]],"alleles")),fixed=TRUE)
  if(is.na(as.integer(alelo[1]))) alelo <- which(alelo%in%alelo)
  alelo <- c(alelo,0)
  b<-cbind(b,t(apply(ped$markerdata[[iMarker]],1,function(x){
          a1<-x[1];a2<-x[2]
          if(a1==0) a1=length(alelo)
          if(a2==0) a2=length(alelo)
          #paste(alelo[a1],alelo[a2],sep="/")
          return(c(alelo[a1],alelo[a2]))
         }))) 
 #}
 write.table(cbind(rep(1,nrow(a)),a[,c("ID","FID","MID","SEX")],b),
            file=fname,sep="\t",quote=F,row.names=FALSE,col.names=FALSE)
}

