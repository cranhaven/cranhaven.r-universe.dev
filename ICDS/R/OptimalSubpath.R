jaccardCoef<-function(my_A,my_B){
  intersect_A_B<-intersect(my_A,my_B)
  union_A_B<-union(my_A,my_B)
  jaccardCoef<-length(intersect_A_B)/length(union_A_B)
  return(jaccardCoef)
}

SplitPath <- function(pathdata) {
  pathway<-unique(pathdata[,2])
  ls<-list()
  for (i in 1:length(pathway)) {
    temp<-paste("pathgroup",i,sep=".")
    temp_data<-assign(temp,pathdata[pathdata[,2]==pathway[i],])
    ls[[i]]<-temp_data
  }
  return(ls)
}




MakeJaccardMatrix<- function(subpath) {
  jaccardmatrix<-matrix(data=0,nrow=nrow(subpath),ncol=nrow(subpath))
  for (i in 1:(nrow(subpath))) {
    datai<-unlist(strsplit(subpath[i, 3], split = "/"))
    for (j in 1:nrow(subpath)) {
      dataj<-unlist(strsplit(subpath[j,3], split = "/"))
      jc <- jaccardCoef(datai, dataj)
      jaccardmatrix[i,j]=jc
    }
  }
  return(jaccardmatrix)
}


Conversion01 <- function(x, threshold){
  diag(x) <- 0
  x[which(x >= threshold)] <- 1
  x[which(x < threshold)] <- 0
  return(x)
}

overlapSPMatrix <- function(Matrix, threshold){
  Matrix01 <- Conversion01(Matrix, threshold)
  gg <- graph.adjacency(Matrix01, mode="undirected")
  clusters <- maximal.cliques(gg)
  return(clusters)
}


FindCombineSubPath<- function(clusters) {
  new <- list()
  count <- 0
  while(length(clusters) > 0){
    clusters <- clusters[order(sapply(clusters, function(x) length(x)), decreasing = T)]
    if(length(clusters[[1]]) > 0){
      count <- count + 1
      new[[count]] <- clusters[[1]]
      clusters <- lapply(clusters, function(x) x[!(x %in% clusters[[1]])])
      clusters <- clusters[-1]
    } else {
      clusters <- clusters[-1]
    }
  }
  return(new)
}



CombineSubPath<- function(subpath,zdata,new) {

  Sub_geneID_union<-c()
  Z<-c()
  res_all<-c()
  for (new_num in 1:length(new)) {
    subpath<-as.matrix(subpath)
    for(i in new[[new_num]]){
      Sub_geneID<-unlist(strsplit(subpath[i,"Subgene"],"/"))
      Sub_geneID_union<-union(Sub_geneID_union,Sub_geneID)
    }
    Subgene<-Sub_geneID_union[1]
    for(i in 2:length(Sub_geneID_union)){
      Subgene<-paste(Subgene,Sub_geneID_union[i],sep="/")
    }
    Z<-sum(as.numeric(zdata[match(Sub_geneID_union,rownames(zdata)),2]))/sqrt(length(Sub_geneID_union))
    id<-unique(substr(subpath[,1],1,9))
    subpathwayid<-paste0(id,"_",new_num)
    subpath<-as.data.frame(subpath)
    pathway<-unique(subpath$pathway)
    pathway<-as.character(pathway)
    size<-length(Sub_geneID_union)
    res<-cbind(subpathwayid,pathway,Subgene,size,Z)
    res_all<-rbind(res_all,res)
    Sub_geneID_union<-c()
  }
  colnames(res_all)<-c("SubpathwayID","pathway","Subgene","Size","SubpathwayZScore")
  return(res_all)
}
#' @title opt_subpath
#' @description `opt_subpath` Optimize interested subpathways.If the number of genes shared by the two pathways accounted for more than the Overlap ratio of each pathway genes,then combine two pathways.
#' @param subpathdata interested subpathways
#' @param zz a vector of z-scores
#' @param overlap Overlap ratio of each two pathway genes
#' @return Optimized subpathway:the number of genes shared by any two pathways accounted for less than the Overlap ratio of each pathway genes.
#' @importFrom igraph maximal.cliques
#' @importFrom igraph graph.adjacency
#' @export
#' @examples
#' zz<-GetExampleData("zzz")
#' subpathdata<-GetExampleData("subpathdata")
#' optsubpath<-opt_subpath(subpathdata,zz,overlap=0.6)
opt_subpath <- function(subpathdata,zz,overlap=0.6) {
  subpathsplited<-SplitPath(subpathdata)
  res_data<-list()
  for (i in 1:length(subpathsplited)) {
    temp_data<-subpathsplited[[i]]

    if (!is.matrix(temp_data)) {
      temp_data<-t(temp_data)
    }
    JaccardMatrix<-MakeJaccardMatrix(temp_data)
    clusters <- overlapSPMatrix(JaccardMatrix, overlap)
    new<-FindCombineSubPath(clusters)
    res_data[[i]]<-CombineSubPath(temp_data,zz,new)
  }
  pathcomb<-c()
  for(i in 1:length(res_data)){
    pathcomb<-rbind(pathcomb,res_data[[i]])
  }
  return(pathcomb)
}


