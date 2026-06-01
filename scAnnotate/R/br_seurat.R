#' @title br_seurat
#'
#' @description keep the original proportion of drop out, use Seurat to align the test data and training data
#'
#' @param train.xx A data matrix where each row is a cell and each column is a gene in training data
#' @param test A data matrix where each row is a cell and each column is a gene in test data
#' @param lognormalized A logical string indicate if both input data are log-normalized or not. TRUE (default) indicates input data are log-normalized, FALSE indicates input data are raw data.
#'
#' @return A list containing aligned gene expression matrix for training data and test data.
#'
#' @importFrom Seurat CreateSeuratObject SplitObject NormalizeData FindVariableFeatures SelectIntegrationFeatures GetAssayData FindIntegrationAnchors IntegrateData GetAssayData DefaultAssay "DefaultAssay<-"
#' @noRd
#'
br_seurat=function(train.xx,test,lognormalized){
  obj=CreateSeuratObject(counts = cbind(t(train.xx),t(test)))
  obj@meta.data$stim = c(rep("train",nrow(train.xx)),rep("test",nrow(test)))

  # split the dataset into a list of two seurat objects (stim and CTRL)
  obj.list <- SplitObject(obj, split.by = "stim")

  obj.list <- lapply(X = obj.list, FUN = function(x) {
    x <- PercentageFeatureSet(x, pattern = "^MT-", col.name = "percent.mt")
    x <- SCTransform(x, vars.to.regress = "percent.mt", verbose = FALSE)
    x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 500)
  })
  
  # if(lognormalized==TRUE){
  #   # identify variable features for each dataset independently
  #   obj.list <- lapply(X = obj.list, FUN = function(x) {
  #     #x <- NormalizeData(x)
  #     x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 2000)
  #   })
  # }else{
  #   # normalize and identify variable features for each dataset independently
  #   obj.list <- lapply(X = obj.list, FUN = function(x) {
  #     x <- NormalizeData(x,normalization.method = "LogNormalize")
  #     x <- FindVariableFeatures(x, selection.method = "vst", nfeatures = 2000)
  #   })
  # }


  # select features that are repeatedly variable across datasets for integration
  features <- SelectIntegrationFeatures(object.list = obj.list)
  train.xx <- LayerData(obj.list[["train"]], assay = "RNA", layer = "counts")[features,]
  #train.xx=obj.list[["train"]]@assays$RNA@layers$counts[features,]
  train.xx=as.matrix(train.xx)
  #test=obj.list[["test"]]@assays$RNA@counts[features,]
  test=LayerData(obj.list[["train"]], assay = "RNA", layer = "counts")[features,]
  test=as.matrix(test)

  ## Perform integration
  #We then identify anchors using the `FindIntegrationAnchors()` function,
  #which takes a list of Seurat objects as input, and use these anchors to
  #integrate the two datasets together with `IntegrateData()`.
  obj.anchors <- FindIntegrationAnchors(object.list = obj.list, scale=F, anchor.features = features)
  # this command creates an 'integrated' data assay
  obj.combined <- IntegrateData(anchorset = obj.anchors)


  # specify that we will perform downstream analysis on the corrected data note that the
  # original unmodified data still resides in the 'RNA' assay
  DefaultAssay(obj.combined) <- "integrated"

  # Run the standard workflow for visualization and clustering
  #obj.combined <- ScaleData(obj.combined, verbose = FALSE)
  #obj.combined <- RunPCA(obj.combined, npcs = 30, verbose = FALSE)
  #obj.combined <- RunUMAP(obj.combined, reduction = "pca", dims = 1:30)

  # Visualization
  #p1 <- DimPlot(obj.combined, reduction = "umap", group.by = "stim")
  #p2 <- DimPlot(obj.combined, reduction = "umap", group.by = "yy")
  #p1 + p2

  oup.list=SplitObject(obj.combined, split.by = "stim")
  train.xx_new=oup.list[["train"]]@assays$integrated@data
  train.xx_new=as.matrix(train.xx_new)

  train.xx_new[which(train.xx==0)]=0

  test_new=oup.list[["test"]]@assays$integrated@data
  test_new=as.matrix(test_new)

  test_new[which(test==0)]=0

  oup=list(train=train.xx_new,
           test=test_new)
}
