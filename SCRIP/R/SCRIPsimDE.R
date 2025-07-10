#' @title SCRIP simulation for differential expression
#'
#' @description Simulate count data for differential expression analysis using SCRIP
#'
#' @param expre_data data matrix required for simulation
#' @param params SplatParams object containing parameters for the simulation
#' @param nGenes number of genes simulated
#' @param nDE number of differentially expressed genes simulated
#' @param ncells number of cells simulated
#' @param FC fold change rate simulated between two groups
#' @param Dropout_rate factor to control the dropout rate directly
#' @param libsize library size used for simulation
#' @param pre.bcv.df BCV.df enables us to change the variation of BCV values
#' @param bcv.shrink factor to control the BCV levels
#' @param seed seed for simulation
#'
#' @return SummarizedExperiment files from both groups for DE analysis and DE genes index
#'
#' @export
#' @importFrom splatter setParams getParam
simu_DE <- function(expre_data, params, nGenes=NULL, nDE, ncells=NULL, FC, Dropout_rate=NULL,
                    libsize=NULL, pre.bcv.df=NULL, bcv.shrink=1, seed=2021){

  if (is.null(nGenes)==F){
    params1=splatter::setParams(params,update=list("nGenes"=nGenes))

  }
  if (is.null(ncells)==F){
    params1=splatter::setParams(params,update=list("batchCells"=ncells))

  }

  parshape <- params1@mean.shape
  parrate <- params1@mean.rate
  parnGenes <- params1@nGenes
  parnCells <- params1@batchCells

  set.seed(seed)
  base_allcellmeans=rgamma(parnGenes, shape=parshape, rate=parrate)
  DEgene <- sample(1:length(base_allcellmeans),nDE,replace=F)

  base_allcellmeansDE <- base_allcellmeans
  base_allcellmeansDE[DEgene[1:nDE/2]] <- base_allcellmeans[DEgene[1:nDE/2]]*FC
  base_allcellmeansDE[DEgene[(nDE/2+1):nDE]] <- base_allcellmeans[DEgene[(nDE/2+1):nDE]]*1/FC

  sim <- SCRIPsimu(data=expre_data, params=params1, libsize=libsize, method="single",
                   base_allcellmeans_SC=base_allcellmeans, Dropout_rate=Dropout_rate, pre.bcv.df=pre.bcv.df, bcv.shrink=bcv.shrink,
                   mode="GP-trendedBCV")
  exps <- counts(sim)


  simDE <-  SCRIPsimu(data=expre_data, params=params, libsize=libsize, method="single",
                      base_allcellmeans_SC=base_allcellmeansDE,Dropout_rate=Dropout_rate,pre.bcv.df=pre.bcv.df, bcv.shrink=bcv.shrink,
                      mode="GP-trendedBCV")
  expsDE <- counts(simDE)

  counts <- cbind(exps,expsDE)
  colnames(counts) <- paste0("cell",1:ncol(counts))
  rownames(counts) <- paste0("gene",1:nrow(counts))

  genenameDE <- paste0("gene",DEgene)
  res.data <- list(sim,simDE,genenameDE)
}







