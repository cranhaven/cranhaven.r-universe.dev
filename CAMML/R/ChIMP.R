#
# ChIMP.R
#
# @author courtney.t.schiebout.gr@dartmouth.edu
#
#
# Takes in a Seurat Object that has previously been scored with CAMML and weights 
# the cell type scores by discretized CITE-seq markers for each cell type.
# 
# 
# Inputs:
#
# -seurat: a Seurat Object that has previously been run on CAMML
# -citelist: a list of all the surface markers for each cell type, named by their cell type.
# -method: "k" for k means or "q" for quantile
# -cutoff: cutoff value for quantile
# -anyMP: A vector of booleans the length of the number of cell types being evaluated regarding whether the CITE-seq weighting will take any positive marker score (TRUE) or requires all positive marker scores (FALSE)
# -greater: A vector of booleans the length of the number of CITE-seq markers being evaluated designating whether the CITE-seq weighting should support cell typing for the presence or absence of a marker.
#
# Output:
#
#   A Seurat Object with a ChIMP assay of scores for each cell type in each cell.
#   
#

ChIMP <- function(seurat, citelist, method = "k", cutoff = .5, anyMP = rep(T, length(rownames(seurat))), greater = rep(T, length(unlist(citelist)))){
  if (missing(seurat)) {
    stop("Missing Seurat Object.")
  }
  if (!method %in% c("k","q")){
    stop("Method needs to be either \"k\" or \"q\"")
  }
  if (cutoff > 1 | cutoff < 0){
    stop("Quantile cut-off needs to be between 0 and 1.")
  }
  if(length(anyMP) != length(rownames(seurat))){
    stop("Any option needs to equal the number of cell types")
  }
  if (any(! anyMP %in% c(T,F))){
    stop("Any option needs to be TRUE or FALSE")
  }
  if (length(citelist) != length(rownames(seurat))){
    stop("CITE list length needs to equal the number of cell types")
  }
  if (length(unlist(citelist)) != length(greater)){
    stop("Greater or less than designations are needed for each CITE-seq marker")
  }
  
  DefaultAssay(object = seurat) = "CAMML"
  
  ChIMP <- matrix(nrow = length(rownames(seurat)), ncol = length(colnames(seurat)))
  k <- 0
  for (i in 1:length(rownames(seurat))){
    correct <- matrix(nrow = length(colnames(seurat)))
    for (j in 1:length(citelist[[i]])){
      k <- k+1
      ind <- which(rownames(seurat@assays$ADT) == citelist[[i]][j])
      if (length(ind) < 1){
        stop("Marker names not found in Seurat Object")
      }
      if (method == "k"){
        cd8 <- kmeans(seurat@assays$ADT@data[ind,], centers = 2)
        if (greater[k]){
          if (median(seurat@assays$ADT@data[ind,][cd8$cluster==1]) > median(seurat@assays$ADT@data[ind,][cd8$cluster==2])){
            cd8$cluster[cd8$cluster == 2] <- 0
          } else{cd8$cluster <- cd8$cluster-1}
        }
        else{
          if (median(seurat@assays$ADT@data[ind,][cd8$cluster==1]) < median(seurat@assays$ADT@data[ind,][cd8$cluster==2])){
            cd8$cluster[cd8$cluster == 2] <- 0
          } else{cd8$cluster <- cd8$cluster-1}
        }
      correct <- cbind(correct, cd8$cluster)
      }
      if (method == "q"){
        if (greater[k]){
          correct <- cbind(correct, ifelse(seurat@assays$ADT@data[ind,] > quantile(seurat@assays$ADT@data[ind,], cutoff),1,0))
        }
        else{
          correct <- cbind(correct, ifelse(seurat@assays$ADT@data[ind,] < quantile(seurat@assays$ADT@data[ind,], cutoff),1,0))
        }
      }
    }
    correct <- correct[,-1]
    if (anyMP[i]){
      if (length(citelist[[i]])>1){
        mcorrect <- apply(correct, 1, max)
      }
      else{
        mcorrect <- correct
      }
    }
    else{
      if (length(citelist[[i]])>1){
        mcorrect <- apply(correct, 1, min)
      }
      else{
        mcorrect <- correct
      }
    }
    ChIMP[i,] <- seurat@assays$CAMML@data[i,]*mcorrect
  }
  rownames(ChIMP) <- rownames(seurat)
  colnames(ChIMP) <- colnames(seurat)
  assay <- CreateAssayObject(counts = ChIMP)
  seurat[["ChIMP"]] <- assay
  return(seurat)
}
