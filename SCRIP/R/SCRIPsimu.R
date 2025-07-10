#' @title SCRIP simulation
#'
#' @description  Simulate count data for single cell RNA-sequencing using SCIRP method
#'
#' @param data data matrix required to fit the mean-BCV trend for simulation
#' @param params SplatParams object containing parameters for the simulation
#' @param method "single", "groups" or "paths"
#' @param base_allcellmeans_SC base mean vector provided to help setting DE analysis
#' @param pre.bcv.df BCV.df enables us to change the variation of BCV values
#' @param libsize library size can be provided directly
#' @param bcv.shrink factor to control the BCV levels
#' @param Dropout_rate factor to control the dropout rate directly
#' @param mode "GP-commonBCV", "BP-commonBCV", "BP", "BGP-commonBCV" and "BGP-trendedBCV"
#' @param de.prob the proportion of DE genes
#' @param de.downProb the proportion of down-regulated DE genes
#' @param de.facLoc DE location factor
#' @param de.facScale DE scale factor
#' @param path.skew Controls how likely cells are from the start or end point of the path
#' @param batch.facLoc DE location factor in batch
#' @param batch.facScale DE scale factor in batch
#' @param path.nSteps number of steps between the start point and end point for each path
#' @param ... Other parameters
#' @return SingleCellExperiment file
#' 
#' 
#' @examples
#' data(params_acinar)
#' data(acinar.data)
#' sim_trend = SCRIPsimu(data=acinar.data, params=params_acinar, mode="GP-trendedBCV")
#' 
#' @export
#' @importFrom splatter setParams getParam
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment colData colData
SCRIPsimu=function(data,
                   params,
                   method="single",
                   base_allcellmeans_SC=NULL,
                   pre.bcv.df=NULL,
                   libsize=NULL,
                   bcv.shrink=1,
                   Dropout_rate=NULL,
                   mode="GP-trendedBCV",
                   de.prob=NULL,
                   de.downProb=NULL,
                   de.facLoc=NULL,
                   de.facScale=NULL,
                   path.skew=NULL,
                   batch.facLoc=NULL,
                   batch.facScale=NULL,
                   path.nSteps=NULL, ...){


  params <-  splatter::setParams(params, ...)
  # params <- splatter::expandParams(params)
  # validObject(params)

  seed <- splatter::getParam(params, "seed")
  set.seed(seed)

  # Get the parameters we are going to use
  nGenes <- splatter::getParam(params, "nGenes")
  nBatches <- splatter::getParam(params, "nBatches")
  batch.cells <- splatter::getParam(params, "batchCells")
  nGroups <- splatter::getParam(params, "nGroups")
  group.prob <- splatter::getParam(params, "group.prob")
  nCells <- splatter::getParam(params, "nCells")

  if (nGroups == 1 && method == "groups") {
    warning("nGroups is 1, switching to single mode")
    method <- "single"
  }

  if (!(mode %in% c("GP-commonBCV","GP-trendedBCV","BP","BGP-commonBCV","BGP-trendedBCV"))){
    stop("simulating mode was not typed correctly")
  }

  # Set up name vectors
  cell.names <- paste0("Cell", seq_len(nCells))
  gene.names <- paste0("Gene", seq_len(nGenes))
  batch.names <- paste0("Batch", seq_len(nBatches))
  if (method == "groups") {
    group.names <- paste0("Group", seq_len(nGroups))
  } else if (method == "paths") {
    group.names <- paste0("Path", seq_len(nGroups))
  }

  # Create SingleCellExperiment to store simulation
  cells <-  data.frame(Cell = cell.names)
  rownames(cells) <- cell.names
  features <- data.frame(Gene = gene.names)
  rownames(features) <- gene.names
  sim <- SingleCellExperiment::SingleCellExperiment(rowData = features, colData = cells,
                                                    metadata = list(Params = params))

  S4Vectors::metadata(sim)$method <-  method
  S4Vectors::metadata(sim)$mode <-  mode
  S4Vectors::metadata(sim)$base_allcellmeans_SC <- base_allcellmeans_SC

  if (is.null(de.prob)==T){
    S4Vectors::metadata(sim)$de.prob <- rep(splatter::getParam(params, "de.prob"), nGroups)
  } else {
    S4Vectors::metadata(sim)$de.prob <- de.prob
  }

  if (is.null(de.downProb)==T){
    S4Vectors::metadata(sim)$de.downProb <- rep(splatter::getParam(params, "de.downProb"), nGroups)
  } else {
    S4Vectors::metadata(sim)$de.downProb <- de.downProb
  }

  if (is.null(de.facLoc)==T){
    S4Vectors::metadata(sim)$de.facLoc <- rep(splatter::getParam(params, "de.facLoc"), nGroups)
  } else {
    S4Vectors::metadata(sim)$de.facLoc <- de.facLoc
  }

  if (is.null(de.facScale)==T){
    S4Vectors::metadata(sim)$de.facScale <- rep(splatter::getParam(params, "de.facScale"), nGroups)
  } else {
    S4Vectors::metadata(sim)$de.facScale <- de.facScale
  }

  if (is.null(path.nSteps)==T){
    S4Vectors::metadata(sim)$path.nSteps <- rep(splatter::getParam(params, "path.nSteps"), nGroups)
  } else {
    S4Vectors::metadata(sim)$path.nSteps <- path.nSteps
  }

  if (is.null(path.skew)==T){
    S4Vectors::metadata(sim)$path.skew <- rep(splatter::getParam(params, "path.skew"), nGroups)
  } else {
    S4Vectors::metadata(sim)$path.skew <- path.skew
  }

  S4Vectors::metadata(sim)$path.from <- splatter::getParam(params, "path.from")


  if (is.null(batch.facLoc)==T){
    S4Vectors::metadata(sim)$batch.facLoc <- rep(splatter::getParam(params, "batch.facLoc"),  nBatches)
  } else {
    S4Vectors::metadata(sim)$batch.facLoc <- batch.facLoc
  }

  if (is.null(batch.facScale)==T){
    S4Vectors::metadata(sim)$batch.facScale <- rep(splatter::getParam(params, "batch.facScale"),  nBatches)
  } else {
    S4Vectors::metadata(sim)$batch.facScale <- batch.facScale
  }


  S4Vectors::metadata(sim)$bcv.shrink <-  bcv.shrink
  S4Vectors::metadata(sim)$pre.bcv.df <-  pre.bcv.df
  S4Vectors::metadata(sim)$Dropout_rate <- Dropout_rate

  batches <- lapply(seq_len(nBatches), function(i, b) {rep(i, b[i])},
                    b = batch.cells)
  batches <- unlist(batches)
  SummarizedExperiment::colData(sim)$Batch <- batch.names[batches]

  if (method != "single") {
    groups <- sample(seq_len(nGroups), nCells, prob = group.prob,
                     replace = TRUE)
    SummarizedExperiment::colData(sim)$Group <- factor(group.names[groups], levels = group.names)
  }

  sim <- SCRIPsimLibSizes(sim, params, libsize)
  sim <- SCRIPsimGeneMeans(data, sim, params)

  if (nBatches > 1) {
    sim <- SCRIPsimBatchEffects(sim, params)
  }
  sim <- SCRIPsimBatchCellMeans(sim, params)
  if (method == "single") {
    sim <- SCRIPsimSingleCellMeans(sim, params)
  } else if (method == "groups") {
    sim <- SCRIPsimGroupDE(sim, params)
    sim <- SCRIPsimGroupCellMeans(sim, params)
  } else {
    sim <- SCRIPsimPathDE(sim, params)
    sim <- SCRIPsimPathCellMeans(sim, params)
  }
  sim <- SCRIPsimBCVMeans(data, sim, params)
  sim <- SCRIPsimTrueCounts(sim,params)
  sim <- SCRIPsimDropout(sim, params)


  return(sim)
}




#' @title  Simulate library sizes
#'
#' @description  Simulate expected library sizes. Typically a log-normal distribution is used but there is also the option to use a normal distribution. In this case any negative values are set to half the minimum non-zero value.
#'
#' @param sim SingleCellExperiment to add library size to.
#' @param params SplatParams object with simulation parameters.
#' @param libsize Provide the library size directly instread of using parameters to estimate
#'
#' @return SingleCellExperiment with simulated library sizes.
#'
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment colData colData
#' @importFrom stats rlnorm rnorm
SCRIPsimLibSizes <- function(sim, params, libsize) {

  nCells <- splatter::getParam(params, "nCells")
  lib.loc <- splatter::getParam(params, "lib.loc")
  lib.scale <- splatter::getParam(params, "lib.scale")
  lib.norm <- splatter::getParam(params, "lib.norm")

  if (is.null(libsize)==F){
    exp.lib.sizes = rep(libsize, nCells)
  } else {

    if (lib.norm) {
      exp.lib.sizes <- stats::rnorm(nCells, lib.loc, lib.scale)
      min.lib <- min(exp.lib.sizes[exp.lib.sizes > 0])
      exp.lib.sizes[exp.lib.sizes < 0] <- min.lib / 2
    } else {
      exp.lib.sizes <- stats::rlnorm(nCells, lib.loc, lib.scale)
    }
  }
  SummarizedExperiment::colData(sim)$ExpLibSize <- exp.lib.sizes

  return(sim)
}




#' @title Simulate gene means
#'
#' @description Simulate gene means from a gamma distribution. Also simulates outlier
#' expression factors. Genes with an outlier factor not equal to 1 are replaced
#' with the median mean expression multiplied by the outlier factor.
#'
#' @param data raw dataset.
#' @param sim SingleCellExperiment to add gene means to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with simulated gene means.
#'
#' @importFrom SummarizedExperiment rowData rowData
#' @importFrom stats rgamma median
SCRIPsimGeneMeans <- function(data, sim, params) {

  nGenes <- splatter::getParam(params, "nGenes")
  mean.shape <- splatter::getParam(params, "mean.shape")
  mean.rate <- splatter::getParam(params, "mean.rate")
  out.prob <- splatter::getParam(params, "out.prob")
  out.facLoc <- splatter::getParam(params, "out.facLoc")
  out.facScale <- splatter::getParam(params, "out.facScale")
  base.means.gene <- S4Vectors::metadata(sim)$base_allcellmeans_SC
  mode  <- S4Vectors::metadata(sim)$mode

  # Simulate base gene means
  if (is.null(base.means.gene)==TRUE){
    base.means.gene <- rgamma(nGenes, shape = mean.shape, rate = mean.rate)

    if (mode %in% c("BGP-commonBCV","BGP-trendedBCV","BP")){

      lib.sizes <- colSums(data)
      lib.med <- median(lib.sizes)
      norm.counts <- t(t(data) / lib.sizes * lib.med)
      norm.counts <- norm.counts[rowSums(norm.counts > 0) > 1, ]

      means <- rowMeans(norm.counts)
      means <- means/stats::quantile(means,0.99)
      means[means>1] <- 1
      means.fit <- fitdistrplus::fitdist(means, "beta", method = "mme")

      p <- stats::rbeta(nGenes, unname(means.fit$estimate["shape1"]),  unname(means.fit$estimate["shape2"]))
      s <- base.means.gene
      base.means.gene <- p*s
    }
  }

  # Add expression outliers
  outlier.facs <- getLNormFactors(nGenes, out.prob, 0, out.facLoc,
                                  out.facScale)
  median.means.gene <- median(base.means.gene)
  outlier.means <- median.means.gene * outlier.facs
  is.outlier <- outlier.facs != 1
  means.gene <- base.means.gene
  means.gene[is.outlier] <- outlier.means[is.outlier]

  SummarizedExperiment::rowData(sim)$BaseGeneMean <- base.means.gene
  SummarizedExperiment::rowData(sim)$OutlierFactor <- outlier.facs
  SummarizedExperiment::rowData(sim)$GeneMean <- means.gene

  return(sim)
}




#' @title Simulate batch effects
#'
#' @description Simulate batch effects. Batch effect factors for each batch are produced
#' using \code{\link{getLNormFactors}} and these are added along with updated
#' means for each batch.
#'
#' @param sim SingleCellExperiment to add batch effects to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with simulated batch effects.
#'
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment rowData rowData<-
#' @importFrom S4Vectors metadata
SCRIPsimBatchEffects <- function(sim, params) {

  nGenes <- splatter::getParam(params, "nGenes")
  nBatches <- splatter::getParam(params, "nBatches")
  batch.facLoc <- S4Vectors::metadata(sim)$batch.facLoc
  batch.facScale <- S4Vectors::metadata(sim)$batch.facScale

  for (idx in seq_len(nBatches)) {
    batch.facs <- getLNormFactors(nGenes, 1, 0.5, batch.facLoc[idx],
                                            batch.facScale[idx])
    rowData(sim)[[paste0("BatchFacBatch", idx)]] <- batch.facs
  }

  return(sim)
}






#' @title Simulate batch means
#'
#' @description Simulate a mean for each gene in each cell incorporating batch effect factors.
#'
#' @param sim SingleCellExperiment to add batch means to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with simulated batch means.
#'
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment rowData rowData colData
SCRIPsimBatchCellMeans <- function(sim, params) {

  nBatches <- splatter::getParam(params, "nBatches")
  cell.names <- SummarizedExperiment::colData(sim)$Cell
  gene.names <- SummarizedExperiment::rowData(sim)$Gene
  gene.means <- SummarizedExperiment::rowData(sim)$GeneMean


  if (nBatches > 1) {
      batches <- SummarizedExperiment::colData(sim)$Batch
      batch.names <- unique(batches)

      batch.facs.gene <- SummarizedExperiment::rowData(sim)[, paste0("BatchFac", batch.names)]
      batch.facs.cell <- as.matrix(batch.facs.gene[,as.numeric(factor(batches))])
  } else {
      nCells <- splatter::getParam(params, "nCells")
      nGenes <- splatter::getParam(params, "nGenes")

      batch.facs.cell <- matrix(1, ncol = nCells, nrow = nGenes)
  }
  batch.means.cell <- batch.facs.cell * gene.means


  colnames(batch.means.cell) <- cell.names
  rownames(batch.means.cell) <- gene.names
  SummarizedExperiment::assays(sim)$BatchCellMeans <- batch.means.cell

  return(sim)
}






#' @title Simulate cell means
#'
#' @description Simulate a gene by cell matrix giving the mean expression for each gene in
#' each cell. Cells start with the mean expression for the group they belong to
#' (when simulating groups) or cells are assigned the mean expression from a
#' random position on the appropriate path (when simulating paths). The selected
#' means are adjusted for each cell's expected library size.
#'
#' @param sim SingleCellExperiment to add cell means to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with added cell means.
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment rowData colData assays
SCRIPsimSingleCellMeans <- function(sim, params) {

  nCells <- splatter::getParam(params, "nCells")
  cell.names <- SummarizedExperiment::colData(sim)$Cell
  gene.names <- SummarizedExperiment::rowData(sim)$Gene
  exp.lib.sizes <- SummarizedExperiment::colData(sim)$ExpLibSize
  batch.means.cell <- SummarizedExperiment::assays(sim)$BatchCellMeans

  cell.means.gene <- batch.means.cell
  cell.props.gene <- t(t(cell.means.gene) / colSums(cell.means.gene))
  base.means.cell <- t(t(cell.props.gene) * exp.lib.sizes)

  colnames(base.means.cell) <- cell.names
  rownames(base.means.cell) <- gene.names
  SummarizedExperiment::assays(sim)$BaseCellMeans <- base.means.cell

  return(sim)
}




#' @title Simulate Group CellMeans
#'
#' @description Simulate group cell means
#'
#' @param sim SingleCellExperiment to add cell means to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with added cell means.
#'
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment rowData colData assays assays<-
SCRIPsimGroupCellMeans <- function(sim, params) {

  nGroups <- splatter::getParam(params, "nGroups")
  cell.names <- SummarizedExperiment::colData(sim)$Cell
  gene.names <- SummarizedExperiment::rowData(sim)$Gene
  groups <- SummarizedExperiment::colData(sim)$Group
  group.names <- levels(groups)
  exp.lib.sizes <- SummarizedExperiment::colData(sim)$ExpLibSize
  batch.means.cell <- SummarizedExperiment::assays(sim)$BatchCellMeans

  group.facs.gene <- SummarizedExperiment::rowData(sim)[, paste0("DEFac", group.names)]
  cell.facs.gene <- as.matrix(group.facs.gene[, paste0("DEFac", groups)])
  cell.means.gene <- batch.means.cell * cell.facs.gene
  cell.props.gene <- t(t(cell.means.gene) / colSums(cell.means.gene))
  base.means.cell <- t(t(cell.props.gene) * exp.lib.sizes)

  colnames(base.means.cell) <- cell.names
  rownames(base.means.cell) <- gene.names
  SummarizedExperiment::assays(sim)$BaseCellMeans <- base.means.cell

  return(sim)
}





#' @title Simulate group differential expression
#'
#' @description Simulate differential expression. Differential expression factors for each
#' group are produced using \code{\link{getLNormFactors}} and these are added
#' along with updated means for each group. For paths care is taken to make sure
#' they are simulated in the correct order.
#'
#' @param sim SingleCellExperiment to add differential expression to.
#' @param params splatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with simulated differential expression.
#'
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment rowData<-
SCRIPsimGroupDE <- function(sim, params) {

  nGenes <- splatter::getParam(params, "nGenes")
  nGroups <- splatter::getParam(params, "nGroups")
  means.gene <- SummarizedExperiment::rowData(sim)$GeneMean


  de.prob <- S4Vectors::metadata(sim)$de.prob
  de.downProb <- S4Vectors::metadata(sim)$de.downProb
  de.facLoc <- S4Vectors::metadata(sim)$de.facLoc
  de.facScale <- S4Vectors::metadata(sim)$de.facScale

  for (idx in seq_len(nGroups)) {
    de.facs <- getLNormFactors(nGenes, de.prob[idx], de.downProb[idx],
                                         de.facLoc[idx], de.facScale[idx])
    group.means.gene <- means.gene * de.facs
    rowData(sim)[[paste0("DEFacGroup", idx)]] <- de.facs
  }

  return(sim)
}





#' @title Simulate BCV means
#' @description Simulate means for each gene in each cell that are adjusted to follow a
#' mean-variance trend using Biological Coefficient of Variation taken from
#' and inverse gamma distribution.
#'
#' @param data data are used to fit the mean-BCV trend for simulation
#' @param sim SingleCellExperiment to add BCV means to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with simulated BCV means.
#'
#' @importFrom splatter getParam
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment rowData colData assays
#' @importFrom stats rchisq rgamma
#' @importFrom mgcv gam
#' @importFrom edgeR DGEList estimateDisp cpm
SCRIPsimBCVMeans <- function(data, sim, params){

  counts <- data
  mode  <- S4Vectors::metadata(sim)$mode
  bcv.shrink <- S4Vectors::metadata(sim)$bcv.shrink
  pre.bcv.df <- S4Vectors::metadata(sim)$pre.bcv.df

  nGenes <- splatter::getParam(params, "nGenes")

  cell.names <- SummarizedExperiment::colData(sim)$Cell
  gene.names <- SummarizedExperiment::rowData(sim)$Gene
  nCells <- splatter::getParam(params, "nCells")
  nGenes=splatter::getParam(params,"nGenes")[[1]]
  bcv.common <- splatter::getParam(params, "bcv.common")
  lib.sizes <-  SummarizedExperiment::colData(sim)$ExpLibSize

  if (is.null(pre.bcv.df)==TRUE){
      bcv.df <- splatter::getParam(params, "bcv.df")
  } else {
    bcv.df <- pre.bcv.df
  }

  design <- matrix(1, ncol(counts), 1)
  y <- edgeR::DGEList(counts,remove.zeros = F)
  disps <- edgeR::estimateDisp(y, design = design,trend.method = "locfit")

  base.means.cell <- SummarizedExperiment::assays(sim)$BaseCellMeans
  x=base.means.cell
  if (nGenes < nrow(counts)){
    random <- sample(1:nrow(counts),nGenes)
  } else {
    random <- 1:nrow(counts)
  }
  data_gam <- as.data.frame(cbind(sqrt(disps$trended.dispersion[random]),disps$AveLogCPM[random]))
  colnames(data_gam) <- c("response","predictor")
  formula <- mgcv::gam(response~s(predictor),data=data_gam)
  x_cpm <- edgeR::cpm(x,log=TRUE,prior.counts=1)


  if (mode=="GP-commonBCV"){

    if (is.finite(bcv.df)) {
      bcv <- (bcv.common + (1 / sqrt(x))) * sqrt(bcv.df / rchisq(nGenes, df = bcv.df)) * bcv.shrink
    } else {
      warning("'bcv.df' is infinite. This parameter will be ignored.")
      bcv <- (bcv.common + (1 / sqrt(x))) * bcv.shrink
    }

    means.cell <- matrix(rgamma(
      as.numeric(nGenes) * as.numeric(nCells),
      shape = 1 / (bcv ^ 2), scale = x * (bcv ^ 2)),
      nrow = nGenes, ncol = nCells)

  }


  if (mode=="GP-trendedBCV") {

    bcv=matrix(rep(1,ncol(x_cpm)*nrow(x_cpm)),ncol=ncol(x_cpm))

    for (c in 1:ncol(x_cpm)) {
      newData <- as.data.frame(x_cpm[,c])
      colnames(newData) <- "predictor"
      bcv[,c] <- stats::predict(formula,newData)
    }


    if (is.finite(bcv.df)) {
      bcv <- bcv*sqrt(bcv.df / rchisq(nGenes, df = bcv.df))*bcv.shrink
    } else {
      warning("'bcv.df' is infinite. This parameter will be ignored.")
      bcv <- bcv*1*bcv.shrink
    }

    means.cell <- matrix(rgamma(
      as.numeric(nGenes) * as.numeric(nCells),
      shape = 1 / (bcv ^ 2), scale = x * (bcv ^ 2)),
      nrow = nGenes, ncol = nCells)

  }

  if (mode=="BGP-commonBCV") {

    lambda <- x
    if (is.finite(bcv.df)) {
      bcv <- (bcv.common + (1 / sqrt(x))) * sqrt(bcv.df / rchisq(nGenes, df = bcv.df)) * bcv.shrink
    } else {
      warning("'bcv.df' is infinite. This parameter will be ignored.")
      bcv <- (bcv.common + (1 / sqrt(x))) * bcv.shrink
    }

    means.cell <- matrix(rgamma(
      nGenes * nCells,
      shape = 1 / (bcv ^ 2), scale = lambda * (bcv ^ 2)),
      nrow = nGenes, ncol = nCells)

  }


  if (mode=="BGP-trendedBCV") {

    lambda <- x
    bcv=matrix(rep(1,ncol(x_cpm)*nrow(x_cpm)),ncol=ncol(x_cpm))
    for (c in 1:ncol(x_cpm)) {
      newData <- as.data.frame(x_cpm[,c])
      colnames(newData) <- "predictor"
      bcv[,c] <- stats::predict(formula,newData)
    }

    if (is.finite(bcv.df)) {
      bcv <- bcv*sqrt(bcv.df / rchisq(nGenes, df = bcv.df))*bcv.shrink
    } else {
      warning("'bcv.df' is infinite. This parameter will be ignored.")
      bcv <- bcv*1*bcv.shrink
    }

    means.cell <- matrix(rgamma(
      nGenes * nCells,
      shape = 1 / (bcv ^ 2), scale = lambda * (bcv ^ 2)),
      nrow = nGenes, ncol = nCells)

  }


  if (mode=="BP") {

    means.cell = lambda = x
  }

  colnames(means.cell) <- cell.names
  rownames(means.cell) <- gene.names
  SummarizedExperiment::assays(sim)$CellMeans <- means.cell
  return(sim)
}






#' @title Simulate true counts
#'
#' @description Simulate a true counts matrix. Counts are simulated from a poisson
#' distribution where Each gene in each cell has it's own mean based on the
#' group (or path position), expected library size and BCV.
#'
#' @param sim SingleCellExperiment to add true counts to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with simulated true counts.
#'
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment rowData colData assays assays
#' @importFrom stats rpois
SCRIPsimTrueCounts <- function(sim, params) {

  cell.names <- SummarizedExperiment::colData(sim)$Cell
  gene.names <- SummarizedExperiment::rowData(sim)$Gene
  nGenes <- splatter::getParam(params, "nGenes")
  nCells <- splatter::getParam(params, "nCells")
  cell.means <- SummarizedExperiment::assays(sim)$CellMeans


  true.counts <- matrix(rpois(
    as.numeric(nGenes) * as.numeric(nCells),
    lambda = cell.means),
    nrow = nGenes, ncol = nCells)


  colnames(true.counts) <- cell.names
  rownames(true.counts) <- gene.names

  SummarizedExperiment::assays(sim)$TrueCounts <- true.counts

  return(sim)
}





#' @title Simulate dropout
#'
#' @description A logistic function is used to form a relationship between the expression
#' level of a gene and the probability of dropout, giving a probability for each
#' gene in each cell. These probabilities are used in a Bernoulli distribution
#' to decide which counts should be dropped.
#'
#' @param sim SingleCellExperiment to add dropout to.
#' @param params SplatParams object with simulation parameters.
#'
#' @return SingleCellExperiment with simulated dropout and observed counts.
#'
#' @importFrom splatter getParam
#' @importFrom SummarizedExperiment rowData colData assays assays
#' @importFrom stats rbinom
SCRIPsimDropout <- function(sim, params) {
  dropout.type <- splatter::getParam(params, "dropout.type")
  true.counts <- SummarizedExperiment::assays(sim)$TrueCounts
  dropout.mid <- splatter::getParam(params, "dropout.mid")
  dropout.shape <- splatter::getParam(params, "dropout.shape")
  cell.names <- SummarizedExperiment::colData(sim)$Cell
  gene.names <- SummarizedExperiment::rowData(sim)$Gene
  nCells <- splatter::getParam(params, "nCells")
  nGenes <- splatter::getParam(params, "nGenes")
  nBatches <- splatter::getParam(params, "nBatches")
  nGroups <- splatter::getParam(params, "nGroups")
  cell.means <- SummarizedExperiment::assays(sim)$CellMeans
  Dropout_rate <- S4Vectors::metadata(sim)$Dropout_rate


  if (is.null(Dropout_rate)==F) {
    drop.prob <- Dropout_rate
    drop.prob <- matrix(drop.prob,nrow = nGenes,ncol = nCells)
    keep <- matrix(rbinom(nCells * nGenes, 1, 1 - drop.prob),
                   nrow = nGenes, ncol = nCells)

    counts <- true.counts * keep
    colnames(drop.prob) <- cell.names
    rownames(drop.prob) <- gene.names
    colnames(keep) <- cell.names
    rownames(keep) <- gene.names
    SummarizedExperiment::assays(sim)$DropProb <- drop.prob
    SummarizedExperiment::assays(sim)$Dropout <- !keep

    BiocGenerics::counts(sim) <- counts

  } else {

    switch(dropout.type,
           experiment = {
             if ((length(dropout.mid) != 1) || length(dropout.shape) != 1) {
               stop("dropout.type is set to 'experiment' but dropout.mid ",
                    "and dropout.shape aren't length 1")
             }

             dropout.mid <- rep(dropout.mid, nCells)
             dropout.shape <- rep(dropout.shape, nCells)
           },
           batch = {
             if ((length(dropout.mid) != nBatches) ||
                 length(dropout.shape) != nBatches) {
               stop("dropout.type is set to 'batch' but dropout.mid ",
                    "and dropout.shape aren't length equal to nBatches ",
                    "(", nBatches, ")")
             }

             batches <- as.numeric(factor(SummarizedExperiment::colData(sim)$Batch))
             dropout.mid <- dropout.mid[batches]
             dropout.shape <- dropout.shape[batches]
           },
           group = {
             if ((length(dropout.mid) != nGroups) ||
                 length(dropout.shape) != nGroups) {
               stop("dropout.type is set to 'group' but dropout.mid ",
                    "and dropout.shape aren't length equal to nGroups ",
                    "(", nGroups, ")")
             }

             if ("Group" %in% colnames(SummarizedExperiment::colData(sim))) {
               groups <- as.numeric(SummarizedExperiment::colData(sim)$Group)
             } else {
               stop("dropout.type is set to 'group' but groups have not ",
                    "been simulated")
             }

             dropout.mid <- dropout.mid[groups]
             dropout.shape <- dropout.shape[groups]
           },
           cell = {
             if ((length(dropout.mid) != nCells) ||
                 length(dropout.shape) != nCells) {
               stop("dropout.type is set to 'cell' but dropout.mid ",
                    "and dropout.shape aren't length equal to nCells ",
                    "(", nCells, ")")
             }
           })

    if (dropout.type != "none") {

      # Generate probabilities based on expression
      drop.prob <- sapply(seq_len(nCells), function(idx) {
        eta <- log(cell.means[, idx])
        return(logistic(eta, x0 = dropout.mid[idx], k = dropout.shape[idx]))
      })

      # Decide which counts to keep
      keep <- matrix(rbinom(nCells * nGenes, 1, 1 - drop.prob),
                     nrow = nGenes, ncol = nCells)

      counts <- true.counts * keep

      colnames(drop.prob) <- cell.names
      rownames(drop.prob) <- gene.names
      colnames(keep) <- cell.names
      rownames(keep) <- gene.names

      SummarizedExperiment::assays(sim)$DropProb <- drop.prob
      SummarizedExperiment::assays(sim)$Dropout <- !keep
    } else {
      counts <- true.counts
    }
  }
  BiocGenerics::counts(sim) <- counts

  return(sim)
}



#' @title Sim PathDE
#' @description simulate DE factors for path
#' @param sim SingleCellExperiment to add dropout to.
#' @param params SplatParams object with simulation parameters.
#' @rdname SCRIPsimPathDE
#' @return SingleCellExperiment with DE for path simulation.
#' @importFrom splatter getParam
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment rowData
SCRIPsimPathDE <- function(sim, params) {
  nGenes <- splatter::getParam(params, "nGenes")
  nCells <- splatter::getParam(params, "nCells")


  de.prob <- S4Vectors::metadata(sim)$de.prob
  de.downProb <- S4Vectors::metadata(sim)$de.downProb
  de.facLoc <- S4Vectors::metadata(sim)$de.facLoc
  de.facScale <- S4Vectors::metadata(sim)$de.facScale
  path.from <- S4Vectors::metadata(sim)$path.from

  path.order <- getPathOrder(path.from)
  for (path in path.order) {
    from <- path.from[path]
    # if (from == 0) {
    #   means.gene <- SummarizedExperiment::rowData(sim)$GeneMean
    # } else {
    #   means.gene <- SummarizedExperiment::rowData(sim)[[paste0("GeneMeanPath", from)]]
    # }

    de.facs <- getLNormFactors(nGenes, de.prob[path], de.downProb[path],
                                de.facLoc[path], de.facScale[path])

    # path.means.gene <- means.gene * de.facs
    SummarizedExperiment::rowData(sim)[[paste0("DEFacPath", path)]] <- de.facs
  }

  return(sim)
}


#' @title sim PathCellMeans
#' @description  simulate cell means for path
#' @param sim SingleCellExperiment to add dropout to.
#' @param params SplatParams object with simulation parameters.
#' @return SingleCellExperiment with cell means for path simulation.
#' @rdname splatSimCellMeans
#' @importFrom SummarizedExperiment rowData colData colData<- assays assays<-
#' @importFrom stats rbinom
SCRIPsimPathCellMeans <- function(sim, params) {

  nGenes <- splatter::getParam(params, "nGenes")
  nCells <- splatter::getParam(params, "nCells")
  nGroups <- splatter::getParam(params, "nGroups")
  cell.names <- SummarizedExperiment::colData(sim)$Cell
  gene.names <- SummarizedExperiment::rowData(sim)$Gene
  path.from <- S4Vectors::metadata(sim)$path.from
  path.nSteps <- S4Vectors::metadata(sim)$path.nSteps
  path.skew <- S4Vectors::metadata(sim)$path.skew
  path.nonlinearProb <- splatter::getParam(params, "path.nonlinearProb")
  path.sigmaFac <- splatter::getParam(params, "path.sigmaFac")
  groups <- SummarizedExperiment::colData(sim)$Group
  exp.lib.sizes <- SummarizedExperiment::colData(sim)$ExpLibSize
  batch.means.cell <- SummarizedExperiment::assays(sim)$BatchCellMeans

  group.sizes <- table(groups)

  # Generate non-linear path factors
  for (idx in seq_along(path.from)) {
    # Select genes to follow a non-linear path
    is.nonlinear <- as.logical(rbinom(nGenes, 1, path.nonlinearProb))
    sigma.facs <- rep(0, nGenes)
    sigma.facs[is.nonlinear] <- path.sigmaFac
    rowData(sim)[[paste0("SigmaFacPath", idx)]] <- sigma.facs
  }

  # Generate non-linear path factors
  for (idx in seq_along(path.from)) {
    # Select genes to follow a non-linear path
    is.nonlinear <- as.logical(rbinom(nGenes, 1, path.nonlinearProb))
    sigma.facs <- rep(0, nGenes)
    sigma.facs[is.nonlinear] <- path.sigmaFac
    rowData(sim)[[paste0("SigmaFacPath", idx)]] <- sigma.facs
  }

  # Generate paths. Each path is a matrix with path.nSteps columns and
  # nGenes rows where the expression from each genes changes along the path.
  path.steps <- lapply(seq_along(path.from), function(idx) {
    from <- path.from[idx]
    # Find the factors at the starting position
    if (from == 0) {
      facs.start <- rep(1, nGenes)
    } else {
      facs.start <- rowData(sim)[[paste0("DEFacPath", from)]]
    }
    # Find the factors at the end position
    facs.end <- rowData(sim)[[paste0("DEFacPath", idx)]]

    # Get the non-linear factors
    sigma.facs <- rowData(sim)[[paste0("SigmaFacPath", idx)]]

    # Build Brownian bridges from start to end
    steps <- buildBridges(facs.start, facs.end, n = path.nSteps[idx],
                          sigma.fac = sigma.facs)

    return(t(steps))
  })

  # Randomly assign a position in the appropriate path to each cell
  path.probs <- lapply(seq_len(nGroups), function(idx) {
    probs <- seq(path.skew[idx], 1 - path.skew[idx],
                 length = path.nSteps[idx])
    probs <- probs / sum(probs)
    return(probs)
  })

  steps <- vapply(factor(groups), function(path) {
    step <- sample(seq_len(path.nSteps[path]), 1, prob = path.probs[[path]])
  }, c(Step = 0))

  # Collect the underlying expression levels for each cell
  cell.facs.gene <- lapply(seq_len(nCells), function(idx) {
    path <- factor(groups)[idx]
    step <- steps[idx]
    cell.means <- path.steps[[path]][, step]
  })
  cell.facs.gene <- do.call(cbind, cell.facs.gene)


  # Adjust expression based on library size
  cell.means.gene <- batch.means.cell * cell.facs.gene
  cell.props.gene <- t(t(cell.means.gene) / colSums(cell.means.gene))
  base.means.cell <- t(t(cell.props.gene) * exp.lib.sizes)

  colnames(base.means.cell) <- cell.names
  rownames(base.means.cell) <- gene.names

  SummarizedExperiment::colData(sim)$Step <- steps
  SummarizedExperiment::assays(sim)$BaseCellMeans <- base.means.cell

  return(sim)
}








#' @title Get log-normal factors
#'
#' @description Randomly generate multiplication factors from a log-normal distribution.
#'
#' @param n.facs Number of factors to generate.
#' @param sel.prob Probability that a factor will be selected to be different
#'        from 1.
#' @param neg.prob Probability that a selected factor is less than one.
#' @param fac.loc Location parameter for the log-normal distribution.
#' @param fac.scale Scale factor for the log-normal distribution.
#'
#' @return Vector containing generated factors.
#'
#' @importFrom stats rbinom rlnorm
getLNormFactors <- function(n.facs, sel.prob, neg.prob, fac.loc, fac.scale) {

  is.selected <- as.logical(rbinom(n.facs, 1, sel.prob))
  n.selected <- sum(is.selected)
  dir.selected <- (-1) ^ rbinom(n.selected, 1, neg.prob)
  facs.selected <- rlnorm(n.selected, fac.loc, fac.scale)
  # Reverse directions for factors that are less than one
  dir.selected[facs.selected < 1] <- -1 * dir.selected[facs.selected < 1]
  factors <- rep(1, n.facs)
  factors[is.selected] <- facs.selected ^ dir.selected

  return(factors)
}






#' @title Get path order
#'
#' @description Identify the correct order to process paths so that preceding paths have
#' already been simulated.
#'
#' @param path.from vector giving the path endpoints that each path originates
#'        from.
#'
#' @return Vector giving the order to process paths in.
getPathOrder <- function(path.from) {

  # Transform the vector into a list of (from, to) pairs
  path.pairs <- list()
  for (idx in seq_along(path.from)) {
    path.pairs[[idx]] <- c(path.from[idx], idx)
  }

  # Determine the processing order
  # If a path is in the "done" vector any path originating here can be
  # completed
  done <- 0
  while (length(path.pairs) > 0) {
    path.pair <- path.pairs[[1]]
    path.pairs <- path.pairs[-1]
    from <- path.pair[1]
    to <- path.pair[2]
    if (from %in% done) {
      done <- c(done, to)
    } else {
      path.pairs <- c(path.pairs, list(path.pair))
    }
  }

  # Remove the origin from the vector
  done <- done[-1]

  return(done)
}


#' @title Brownian bridge
#'
#' @description Calculate a smoothed Brownian bridge between two points. A Brownian bridge is
#' a random walk with fixed end points.
#'
#' @param x starting value.
#' @param y end value.
#' @param N number of steps in random walk.
#' @param n number of points in smoothed bridge.
#' @param sigma.fac multiplier specifying how extreme each step can be.
#'
#' @return Vector of length n following a path from x to y.
#'
#' @importFrom stats runif rnorm spline
bridge <- function (x = 0, y = 0, N = 5, n = 100, sigma.fac = 0.8) {

  dt <- 1 / (N - 1)
  t <- seq(0, 1, length = N)
  sigma2 <- runif(1, 0, sigma.fac * mean(c(x, y)))
  X <- c(0, cumsum(rnorm(N - 1, sd = sigma2) * sqrt(dt)))
  BB <- x + X - t * (X[N] - y + x)
  BB <- spline(BB, n = n)$y
  BB[BB < 0] <- 1e-6

  return(BB)
}
buildBridges <- Vectorize(bridge, vectorize.args = c("x", "y", "sigma.fac"))

