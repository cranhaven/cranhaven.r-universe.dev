#' @title index2Genotypes
#' @description index2Genotypes
#' @param ped pedigree
#' @param id individual id
#' @param iMarker marker index
#' @param alleleSet allele set
#' @return genotypes
#' @export
index2Genotypes2.pedtools<-function(ped,id,iMarker,alleleSet){
  origAlleles <- attr(ped$MARKERS[[iMarker]],"alleles")
  al          <- ped$MARKERS[[iMarker]][id,]
  
  #genotipos del padre
  if(al[1]==0){
    al1 <- alleleSet
  }else{         
    al1 <- origAlleles[al[1]]
  }
  if(al[2]==0){
    al2 <- alleleSet
  }else{
    al2 <- origAlleles[al[2]]
  }   
  res  <-unique(apply(expand.grid(al1,al2),1,function(w){paste(sort(w),collapse="/")}))
  return(res)
}
