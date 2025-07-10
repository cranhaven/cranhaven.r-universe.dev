
#' Detecting top discriminators between two groups
#'
#' @description Detects top discriminators that contribute to
#' group separation based on the fixation index (Fst).
#'
#' @param cluster.obj The object which is returned from \code{\link{ipcaps}}.
#' This parameter is used when \code{use.path} is FALSE.
#' @param group1 To specify the first group number to be compared. (also see
#' \code{use.node.number})
#' @param group2 To specify the second group number to be compared. (also see
#' \code{use.node.number})
#' @param bim.file Option: In case that SNP information is not provided to
#' \code{\link{ipcaps}}, an absolute path of SNP information file is required.
#' It needs to be in PLINK format (bim). See more details at:
#' \url{http://zzz.bwh.harvard.edu/plink/data.shtml}.
#' @param use.node.number To specify whether a group number or a node number
#' is be used. If TRUE, a node nubmer is used instead. Default = FALSE.
#' @param num.top A number of top Fst SNPs to be returned. This parameter is
#' used when \code{percentile} is FALSE. Default = 100.
#' @param percentile A percentile for top Fst SNPs to be returned. This parameter
#' is used when \code{percentile} is TRUE. Default = 0.999.
#' @param use.percentile A logical value to indicate whether \code{percentile}
#' is used instead of \code{num.top}. This parameter is used when \code{percentile}
#' is TRUE. Default = FALSE.
#' @param use.path A logical value to indicate whether \code{result.path} is
#' used instead of \code{cluster.obj}. Importantly, \code{result.path} needs to
#' be set. This parameter only work with the IPCAPS's result from version 1.1.7
#' onward. Default = FALSE.
#' @param result.path A path to an result directory of IPCAPS. This parameter is
#' used when \code{use.path} is TRUE. This parameter only work with the IPCAPS's
#' result from version 1.1.7 onward.
#'
#' @return The returned value is a data.frame of SNP information sorting by Fst
#' in descending order, which contains 7 columns, chr, SNP, centimorgans,
#' position, allele1, allele2, and Fst. The column 1-6 are SNP information from
#' the bim file. The column Fst contains estimated Fst between group1 and group2.
#'
#' @export
#'
#' @examples
#'
#' # Importantly, bed file, bim file, and fam file are required
#' # Use the example files embedded in the package
#' BED.file <- system.file("extdata","ipcaps_example.bed",package="IPCAPS")
#' LABEL.file <- system.file("extdata","ipcaps_example_individuals.txt.gz",package="IPCAPS")
#' my.cluster <- ipcaps(bed=BED.file,label.file=LABEL.file,lab.col=2,out=tempdir())
#' table(my.cluster$cluster$label,my.cluster$cluster$group)
#' # 1 2 3 4 5 6
#' # outlier4 5 4 1 0 0 0
#' # pop1 0 0 0 0 250 0
#' # pop2 0 0 0 0 0 250
#' # pop3 0 0 0 250 0 0
#'
#' #Identify top discriminators between groups, for example, group 4 and group 5
#' top.snp <-top.discriminator(my.cluster,4,5)
#' #or, specify the bim file
#' #top.snp <-top.discriminator(my.cluster,4,5,bim.file="ipcaps_example.bim")
#' head(top.snp)
#' # chr SNP centimorgans position allele1 allele2 Fst
#' #V5452 1 marker5452 0 54520000 A T 0.11337260
#' #V2348 1 marker2348 0 23480000 A T 0.11194490
#' #V8244 1 marker8244 0 82440000 A T 0.09556580
#' #V5972 1 marker5972 0 59720000 A T 0.08747794
#' #V3561 1 marker3561 0 35610000 A T 0.08725860
#' #V8419 1 marker8419 0 84190000 A T 0.08293494
#'
#' #Alternatively, specify the previous result directory of IPCAPS and identify
#' #top discriminators between groups, for example, group 4 and group 5
#' previous.res.path <- my.cluster$output.dir
#' top.snp <-top.discriminator(result.path = previous.res.path, use.path = TRUE,
#' group1 = 4, group2 = 5)
#' head(top.snp)
#'
#' #Identify top discriminators between groups, for example, group 4 and group 5
#' top.snp <-top.discriminator(my.cluster,4,5)
#' #or, specify the bim file
#' #top.snp <-top.discriminator(my.cluster,4,5,bim.file="ipcaps_example.bim")
#' dim(top.snp)
#' head(top.snp)

#'
#' #Here, it is possible to select the top Fst SNPs based on a percentile.
#' top.snp <-top.discriminator(my.cluster,4,5, percentile = 0.9,
#' use.percentile = TRUE)
#' dim(top.snp)
#' head(top.snp)
#'
#' #Identify top discriminators between groups, for example, node 7 and node 8
#' top.snp2 <-top.discriminator(my.cluster,7,8,use.node.number=TRUE)
#' head(top.snp2)
#' # chr SNP centimorgans position allele1 allele2 Fst
#' #V5452 1 marker5452 0 54520000 A T 0.11337260
#' #V2348 1 marker2348 0 23480000 A T 0.11194490

top.discriminator <- function(cluster.obj = NULL, group1, group2, bim.file,
                              use.node.number = FALSE, num.top = 100,
                              percentile = 0.9, use.percentile = FALSE,
                              use.path = FALSE , result.path = NULL){

  raw.data <- NULL
  used.path <- NULL

  if (is.null(cluster.obj) && is.null(result.path)){
    cat(paste0("Incorrect parameter, please use the object returned from the function ipcaps as an input\n"))
    return(NULL)
  }

  if (use.path == TRUE){
    used.path = result.path
    if (!dir.exists(used.path)){
      cat(paste0("The result path doesn't exist, please check result.path\n"))
      return(NULL)
    }
    result.filename = file.path(used.path,"RData","result.RData")
    load(result.filename)
  }else{
    used.path = cluster.obj$output.dir
  }


  raw.filename = file.path(used.path,"RData","rawdata.RData")
  if (!file.exists(raw.filename)){
    cat(paste0("Not found the rawdata file: ",raw.filename,"\n"))
    return(NULL)
  }else{
    load(raw.filename)
    if (is.null(snp.info)){
      if (!file.exists(bim.file)){
        cat(paste0("Not found the bim file: ",bim.file,"\n"))
        return(NULL)
      }else{
        snp.info <- read.table(file=bim.file, colClasses=c('factor','factor','factor','factor','factor','factor'))
      }
    }
  }


  if (num.top<0){
    cat(paste0("num.top must be more than zero\n"))
    return(NULL)
  }

  if (use.node.number == FALSE){
    index1 <- cluster.obj$cluster$row.number[which(cluster.obj$cluster$group == group1)]
    index2 <- cluster.obj$cluster$row.number[which(cluster.obj$cluster$group == group2)]
  }else{
    index1 <- cluster.obj$cluster$row.number[which(cluster.obj$cluster$node == group1)]
    index2 <- cluster.obj$cluster$row.number[which(cluster.obj$cluster$node == group2)]
  }

  all.fst <- fst.each.snp.hudson(raw.data,index1,index2)

  snp.with.fst <- cbind(snp.info,all.fst)
  colnames(snp.with.fst) <- c('chr','SNP','centimorgans','position','allele1','allele2','Fst')
  top.fst.snp <- snp.with.fst[order(-snp.with.fst$Fst),]

  if (use.percentile == FALSE){
    ret <- head(top.fst.snp, n=num.top)
  }else{
    qfst = quantile(snp.with.fst$Fst, c(as.double(percentile)), na.rm = T)
    ret = top.fst.snp[which(top.fst.snp$Fst > qfst),]
  }


  return(ret)
}



