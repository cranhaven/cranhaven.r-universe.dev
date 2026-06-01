#' @title br_harmony
#'
#' @description use Harmony to align the test data and training data onto a PCA
#'
#' @param train.xx A data matrix where each row is a cell and each column is a gene in training data
#' @param test A data matrix where each row is a cell and each column is a gene  in test data
#' @param lognormalized A logical string indicate if both input data are log-normalized or not. TRUE (default) indicates input data are log-normalized, FALSE indicates input data are raw data.
#'
#' @return A list containing aligned PCs matrix for training data and test data.
#'
#' @importFrom harmony HarmonyMatrix
#' @importFrom Seurat CreateSeuratObject RunPCA ScaleData NormalizeData
#' @noRd
#'
br_harmony=function(train.xx,test,lognormalized){

  meta_data=data.frame(dataset=c(rep("train",nrow(train.xx)),rep("test",nrow(test))))
  obj.pc=CreateSeuratObject(counts = cbind(t(train.xx),t(test)))
  obj.pc <- PercentageFeatureSet(obj.pc, pattern = "^MT-", col.name = "percent.mt")
  obj.pc <- SCTransform(obj.pc, vars.to.regress = "percent.mt", verbose = FALSE)
  obj.pc=FindVariableFeatures(obj.pc)
  obj.pc=RunPCA(obj.pc,npcs = 20)
  
  #pca
  # if(lognormalized==TRUE){
  #   obj.pc=CreateSeuratObject(counts = cbind(t(train.xx),t(test)))
  #   obj.pc=FindVariableFeatures(obj.pc)
  #   obj.pc=ScaleData(obj.pc)
  #   obj.pc=RunPCA(obj.pc,npcs = 20)
  # }else{
  #   obj.pc=CreateSeuratObject(counts = cbind(t(train.xx),t(test)))
  #   obj.pc=NormalizeData(obj.pc,normalization.method = "LogNormalize")
  #   obj.pc=FindVariableFeatures(obj.pc)
  #   obj.pc=ScaleData(obj.pc)
  #   obj.pc=RunPCA(obj.pc,npcs = 20)
  # }


  pc.matrix= obj.pc@reductions$pca@cell.embeddings

  my_harmony_embeddings = HarmonyMatrix(
    data_mat=pc.matrix,
    meta_data=meta_data,
    vars_use="dataset",
    do_pca=FALSE
  )

  train.xx_new=my_harmony_embeddings[meta_data$dataset=="train",]
  test_new=my_harmony_embeddings[meta_data$dataset=="test",]

  oup=list(train=train.xx_new,
           test=test_new)
  return(oup)
}
