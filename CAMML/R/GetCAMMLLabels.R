#
# GetCAMMLLabels.R
#
# @author courtney.t.schiebout.gr@dartmouth.edu
#

#
# Takes in a Seurat Object of single-cell RNA-seq data and a data frame for the gene set or sets that represent genes and their gene weights.
# CAMML will then perform weighted VAM to calculate a score for each cell type in each cell.
# 
# Inputs:
#
# -seurat: The single-cell RNA-seq Seurat Object, post-filtering, normalization, and scaling, with CAMML assay.
# -labels: One of the following: "top1", "top2","top10p", or "top2xmean"
# "top1" will return the highest scoring cell type for each cell
# "top2" will return the top two highest scoring cell types for each cell
# "top10p" will return the highest scoring cell type and any other cell types with scores within 10\% of the highest score for each cell
# "top2xmean" will return all cell types with scores at least twice the mean of all cell type scores for each cell
#
# Output:
#
#   Labels for each cell based on user choice
#   
#


GetCAMMLLabels <- function(seurat, labels = "top1"){
  
  if(! labels %in% c("top1", "top2","top10p","top2xmean")){
    stop("Label option needs to be one of the following: \"top1\", \"top2\",\"top10p\",\"top2xmean\"")
  }
  
  if (missing(seurat)) {
    stop("Missing Seurat Object.")
  }
  
  #initalize all label options
  le <- length(seurat$orig.ident)
  celz <- list(c(1:le))
  celztop2 <- list(c(1:le))
  celzfold <- list(c(1:le))
  celz2 <- list(c(1:le))
  celz3 <- list(c(1:le))
  celz10p <- list(c(1:le))
  
  #find labels
  for (i in 1:le){
    #take data and label
    dif <- data.frame(seurat@assays$CAMML@data[,i])
    dif <- data.frame(dif[order(-dif[,1]), , drop = FALSE])
    #for cells with no scores, skip
    if (max(dif) == 0){
      celztop2[[i]]<- NA
      celzfold[[i]]<- NA
      celz10p[[i]] <- NA
      celz[[i]] <- "none"
      next
    }
    if (length(rownames(dif)) < 2){
      celz[i]<- rownames(dif)[1]
      celztop2[[i]] <- data.frame(dif[1,], row.names=rownames(dif)[1])
      me <- mean(dif[,1])
      fold <- which(dif[,1] > me*2)
      celzfold[[i]] <- data.frame(dif[fold,],row.names=rownames(dif)[fold])
      inde <- which(dif > max(dif*.9))
      celz10p[[i]] <- data.frame(dif[inde,],row.names=rownames(dif)[inde])
      next
    }
    #top cells
    celz[i]<- rownames(dif)[1]
    dif2 <- data.frame(dif[-1,], row.names=rownames(dif)[-1])
    celz2[i] <- rownames(dif2)[1]
    celz3[i] <- rownames(dif2)[2]
    
    #top2
    celztop2[[i]] <- data.frame(dif[c(1:2),], row.names=rownames(dif)[c(1:2)])
    
    #fold change
    me <- mean(dif[,1])
    fold <- which(dif[,1] > me*2)
    celzfold[[i]] <- data.frame(dif[fold,],row.names=rownames(dif)[fold])
    
    #percent dif
    inde <- which(dif > max(dif*.9))
    celz10p[[i]] <- data.frame(dif[inde,],row.names=rownames(dif)[inde])
  }
  
  if (labels == "top1"){
    output <- celz
  }
  
  if (labels == "top2"){
    output <- celztop2
  }
  
  if (labels == "top10p"){
    output <- celz10p
  }
  
  if (labels == "top2xmean"){
    output <- celzfold
  }
  return(output)
}
