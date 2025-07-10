#' @title SCRIP simulation for clustering analysis
#'
#' @description  Simulate count data for clustering analysis by preserving variably expressed genes
#'
#'@param counts.matrix data matrix required for simulation
#'@param params SplatParams object containing parameters for the simulation
#'@param base_allcellmeans base cell means specified directly for simulating counts
#'@param mode "GP-commonBCV", "BP-commonBCV", "BP", "BGP-commonBCV" and "BGP-trendedBCV"
#'@param nCells number of cells simulated
#'@param nfeatures parameter required for FinalVariable function in Seurat package
#'
#'
#' @return simulated read counts data
#'
#' @export
#' @importFrom Seurat CreateSeuratObject NormalizeData FindVariableFeatures
#' @importFrom utils head
#' @importFrom SingleCellExperiment counts
simu.VEGs <- function(counts.matrix, params=params, base_allcellmeans, mode="GP-trendedBCV", nCells, nfeatures=1000){
  message("Starting simulating SCRIP")
  rownames(counts.matrix) <- paste0("Gene",1:nrow(counts.matrix))
  colnames(counts.matrix) <- paste0("Cell",1:ncol(counts.matrix))

  VEGs_data <- Seurat::CreateSeuratObject(counts = counts.matrix, project = "SCRIP_VEGs", min.cells = 3, min.features = 200)
  seurat_stim <- Seurat::NormalizeData(VEGs_data,
                               normalization.method = "LogNormalize",
                               scale.factor = 10000)
  stim <- Seurat::FindVariableFeatures(object = seurat_stim,
                               selection.method = "vst",
                               nfeatures = nfeatures)
  top <- utils::head(Seurat::VariableFeatures(stim),nfeatures)
  EGcounts <- counts.matrix[top,]

  com_base_cellmeans <- base_allcellmeans
  names(com_base_cellmeans) <- rownames(counts.matrix)

  com_base_cellmeans[top] <- apply(EGcounts,1,mean)

  #### SCRIP ####
  if (mode=="BP-trendedBCV"){
    message("Starting simulating SCRIP BP-trendedBCV model")
    SCRIP.Trend.burst <- SCRIPsimu(data=counts.matrix, params=params, batchCells=nCells,
                                   base_allcellmeans_SC=com_base_cellmeans,
                                   mode="BP-trendedBCV")
    res <- SingleCellExperiment::counts(SCRIP.Trend.burst)
  }

  if (mode=="BP-commonBCV"){
    message("Starting simulating SCRIP BP-commonBCV model")
    SCRIP.common.burst <-  SCRIPsimu(data=counts.matrix, params=params, batchCells=nCells,
                                   base_allcellmeans_SC=com_base_cellmeans,
                                   mode="BP-commonBCV")
    res <- SingleCellExperiment::counts(SCRIP.common.burst)
  }


  if (mode=="GP-trendedBCV"){
    message("Starting simulating SCRIP GP-trendedBCV model")
    SCRIP.Trend.noburst <- SCRIPsimu(data=counts.matrix, params=params, batchCells=nCells,
                                   base_allcellmeans_SC=com_base_cellmeans,
                                   mode="GP-trendedBCV")
    res <- SingleCellExperiment::counts(SCRIP.Trend.noburst)
  }


  if (mode=="GP-commonBCV"){
    message("Starting simulating SCRIP GP-commonBCV model")
    SCRIP.common.noburst <-  SCRIPsimu(data=counts.matrix, params=params, batchCells=nCells,
                                     base_allcellmeans_SC=com_base_cellmeans,
                                     mode="GP-commonBCV")
    res <- SingleCellExperiment::counts(SCRIP.common.noburst)
  }

  return(res)
}




#' @title SCRIP simulation for clustering analysis with multiple cell types
#'
#' @description Simulate count data for clustering analysis by preserving variably expressed genes with multiple cell types
#'
#' @param expre_data data matrix required for simulation
#' @param pheno_data phenotype data information
#' @param CTlist cell types used for simulation
#' @param mode "GP-commonBCV", "BP-commonBCV", "BP", "BGP-commonBCV" and "BGP-trendedBCV"
#' @param nfeatures parameter required for FinalVariable function in Seurat package
#' @param seed seed used for simulation
#'
#' @return simulated read counts data with cell type information
#'
#' @export
#' @importFrom splatter splatEstimate
#' @importFrom stats rgamma

simu_cluster <- function(expre_data, pheno_data, CTlist, mode, nfeatures, seed=2021){
  set.seed(seed)
  params <- splatter::splatEstimate(expre_data)
  parnGene <- params@nGenes
  parshape <- params@mean.shape
  parrate <- params@mean.rate
  base_allcellmeans=stats::rgamma(parnGene, shape=parshape, rate=parrate)

  final <- CT.infor <- NULL
  for (CT in CTlist){
    counts <- expre_data[,which(pheno_data$cellType==CT)]
    message(paste0("starting simulating cell type: ", CT))
    res <- simu.VEGs(counts.matrix = counts,params=params, base_allcellmeans=base_allcellmeans, nCells=ncol(counts), mode=mode, nfeatures = nfeatures)
    final <- cbind(final, res)
    CT.infor <- c(CT.infor, rep(CT, ncol(counts)))
  }
  final.list <- list(final=final, CT.infor=CT.infor)
  return(final.list)
}




