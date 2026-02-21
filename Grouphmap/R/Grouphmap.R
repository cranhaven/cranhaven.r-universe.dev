
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @title Grouphmap: Automated one-step common analysis of Batch expression profile
#' @param Path "File storage path." Two groups are one file, and the control group should before the treatment group.
#' @param conRep Number of repetitions in control group.
#' @param treRep Number of repetitions in treatment group.
#' @param OrgDb org.Mm.eg.db, org.Hs.eg.db, and org.Sc.sgd.db. Please library().
#' @param TYPE "SYMBOL","ENSEMBOL"...
#' @param UP up is TRUE and down is FALSE
#' @param cutree heatmap can be devided multiple modules that make the functional difference and similarity of those group to be obvious
#' @param TOP the numeric.Such as 10 is the top 10 of GO analysis in each file.
#' @importFrom utils read.csv write.csv
#' @importFrom stats model.matrix na.omit
#' @importFrom grDevices colorRampPalette
#' @importFrom org.Mm.eg.db org.Mm.eg.db
#' @return p1
#' @export
#' @examples \donttest{library(org.Mm.eg.db)
#' Path<-system.file("extdata", package = "Grouphmap")
#' ghmap(Path,2,2,org.Mm.eg.db,"ENSEMBL",FALSE,2,10)
#' }
ghmap <- function(Path,conRep,treRep,OrgDb,TYPE,UP=TRUE,cutree,TOP) {
  a = list.files(path =Path,pattern = ".csv")
  n=length(a)
  dir.create(paste(Path,"/go",sep = ""))
  dir.create(paste(Path,"/DEG",sep = ""))
  for (i in 1:n){
    df <- read.csv(file = paste(Path,"/",a[i],sep = ""), header = T, row.names = 1, check.names = F)
    df[is.na(df)]<-0
    dflog2<-log2(df+1)
    group_list=factor(c(rep("con",conRep),rep("tre",treRep)))
    list <- model.matrix(~0+group_list)  #把group设置成一个model matrix
    colnames(list) <- c("con","tre")
    rownames(list)<-colnames(dflog2)
    df.matrix <- limma::makeContrasts("tre-con" , levels = list)
    fit <- limma::lmFit(dflog2, list)
    fit2<-limma::contrasts.fit(fit,df.matrix)
    fit2 <- limma::eBayes(fit2)
    tempOutput <- limma::topTable(fit2,n = Inf,adjust="fdr")
    nrDEG=na.omit(tempOutput)## 去掉数据中有NA的行或列
    write.csv(nrDEG, paste(Path,"/DEG/",i,a[i],sep = ""))
    up_DEG<- subset(nrDEG, nrDEG$P.Value< 0.05 & nrDEG$logFC > 1)
    down_DEG <- subset(nrDEG, nrDEG$P.Value< 0.05 & nrDEG$logFC < -1)
    All_up<-clusterProfiler::enrichGO(gene = rownames (up_DEG),OrgDb, keyType = TYPE,ont = "ALL",pAdjustMethod = "BH",pvalueCutoff = 1, qvalueCutoff = 1,readable = TRUE)
    All_down<-clusterProfiler::enrichGO(gene = rownames (down_DEG),OrgDb, keyType = TYPE,ont = "ALL",pAdjustMethod = "BH",pvalueCutoff = 1, qvalueCutoff = 1,readable = TRUE)
    write.csv(up_DEG,file=paste(Path,"/DEG/","up",i,a[i],sep = ""))
    write.csv(down_DEG,file=paste(Path,"/DEG/","down",i,a[i],sep = ""))
    write.csv(All_up,file=paste(Path,"/go/","up",i,a[i],sep = ""))
    write.csv(All_down,file=paste(Path,"/go/","down",i,a[i],sep = ""))
  }
  if(!UP){
    regul <- "down"
  } else {
    regul <- "up"
  }
  b = list.files(path=paste(Path,"/go/",sep=""),pattern = regul)
  b
  m = length(b)
  go1<-read.csv(file = paste(Path,"/go/",b[1],sep=""), header = T, row.names = 1)
  go1<-go1[c(1:TOP),]
  for (i in 2:m){
    go <- read.csv(file = paste(Path,"/go/",b[i],sep=""), header = T, row.names = 1)
    go<-go[c(1:TOP),]
    go1<-rbind(go1,go)
  }
  go1<-go1[,c(2,3)]
  go1 <- go1[!duplicated(go1$Description),]
  for (i in 1:m){
    go<- read.csv(file = paste(Path,"/go/",b[i],sep=""), header = T, row.names = 1)
    go1<-dplyr::left_join(go1,go,by="Description")
    go1<-go1[,c(1:(i+1),6+i)]
  }
  d<-stringr::str_remove(string = a,pattern = ".csv")
  colnames(go1)<-c("ID","Description",d)
  rownames(go1)<-go1[,2]
  go1<-go1[,-c(1:2)]
  d<--log10(go1)
  d[is.na(d)]<-0
  if(!UP){
    name<- "down"
  } else {
    name <- "up"
  }
  dir.create(paste(Path,"/merge",sep = ""))
  write.csv(d,file=paste(Path,"/merge/","merge",name,".csv",sep = ""))
  if(!UP){
    color <- "blue"
  } else {
    color <- "red"
  }
  p1<-pheatmap::pheatmap(d,treeheight_row = 0,
           fontsize_row  = 10,
           fontsize_col = 10,
           cellheight=10,
           cellwidth=10,
           cluster_rows = T,#行聚类
           cluster_cols = F,#列聚类
           cutree_rows = cutree,
           color =colorRampPalette(c("white",color))(100)
  )
  print(p1)
}

