#' @title getAllelesFromGenotypes
#' @description Get alleles from genotypes
#' @param g genotypes
#' @return alleles
#' @export
getAllelesFromGenotypes<-function(g){
   return(unique(unlist(strsplit(g,"/"))))
}