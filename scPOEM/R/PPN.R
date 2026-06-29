#'@title Construct Peak-Peak Network
#'
#'@description Construct peak-peak network.
#'@name PPN
#'@importFrom stringr str_split_fixed
#'@import Matrix
#'@importFrom tictoc tic toc
#'@importFrom magrittr %>%
#'@importFrom stats setNames
#'@import Biobase
#'@importFrom BiocGenerics estimateSizeFactors
#'@import monocle
#'@import cicero
#'@importFrom VGAM binomialff
#'@param X The scATAC-seq data, sparse matrix.
#'@param peak_data The information for peaks, must have a col names "peak_name".
#'@param cell_data The information for cells, must have a col names "cell_name".
#'@param genome The genome length for the species.
#'@param dirpath The folder path to read or write file.
#'@param rebuild_PPN Logical. Whether to rebuild the peak-peak network (PPN) from scratch. If FALSE, the function will attempt to read from `PPN.mtx` under `dirpath/test` in `single` mode or `dirpath/state_name/test` in `compare` mode.
#'@param save_file Logical, whether to save the output to a file.
#'@param seed An integer specifying the random seed to ensure reproducible results.
#'@return The PPN network.
#'@examples
#'\donttest{
#' library(scPOEM)
#' library(monocle)
#' dirpath <- "./example_data"
#' # Download single mode example data
#' data(example_data_single)
#' # Construct PPN net.
#' pp_net <- PPN(example_data_single$X,
#'               example_data_single$peak_data,
#'               example_data_single$cell_data,
#'               example_data_single$genome,
#'               file.path(dirpath, "single"),
#'               save_file=FALSE)
#'}
#'
#' @export

PPN <-function(X, peak_data, cell_data, genome, dirpath=tempdir(), rebuild_PPN=TRUE, save_file=TRUE, seed=NULL){
  if (!rebuild_PPN){
    message("Load peak-peak network\n")
    pp_net <- readMM(file.path(dirpath, "test/PPN.mtx"))
    return(pp_net)
  }
  message("Construct peak-peak matrix from scratch...\n")
  use_cicero <- function(indata, fd, pd, peakinfo, genome, dirpath){
    # make CDS
    input_cds <-  suppressWarnings(newCellDataSet(indata,
                                                  phenoData = pd,
                                                  featureData = fd,
                                                  expressionFamily=binomialff(),
                                                  lowerDetectionLimit=0))
    input_cds@expressionFamily@vfamily <- "binomialff"
    input_cds <- monocle::detectGenes(input_cds)

    #Ensure there are no peaks included with zero reads
    input_cds <- input_cds[Matrix::rowSums(Biobase::exprs(input_cds)) != 0,]

    if(!is.null(seed)){
      set.seed(as.integer(seed))
    }
    input_cds <- detectGenes(input_cds)
    input_cds <- estimateSizeFactors(input_cds)
    input_cds <- reduceDimension(input_cds, max_components = 2, num_dim=6,
                                 reduction_method = 'tSNE', norm_method = "none")
    tsne_coords <- t(reducedDimA(input_cds))
    row.names(tsne_coords) <- row.names(Biobase::pData(input_cds))
    cicero_cds <- make_cicero_cds(input_cds, reduced_coordinates = tsne_coords)
    gc()

    #genome <- fread(genome_file, header = FALSE)#human.hg38.genome,mouse.mm10.genome
    conns <- run_cicero(cicero_cds, genome)
    head(conns)


    conns$Peak2 <- as.character(conns$Peak2)

    mask <- !(is.na(conns$coaccess) | (conns$coaccess==0))
    co <- conns$coaccess[mask]
    Peak1 <- conns$Peak1[mask]
    Peak2 <- conns$Peak2[mask]

    x <- setNames(seq(nrow(peakinfo)), peakinfo$site_name)
    id1 <- x[Peak1]
    id2 <- x[Peak2]
    names(id1) <- NULL
    names(id2) <- NULL
    conn_mx <- sparseMatrix(i = id1, j = id2, x = co, dims = c(nrow(peakinfo), nrow(peakinfo)))

    if (save_file==TRUE){
      if (!dir.exists(file.path(dirpath, "test"))) {
        dir.create(file.path(dirpath, "test"), showWarnings = FALSE, recursive = TRUE)
      }
      writeMM(conn_mx, file.path(dirpath, "test/PPN.mtx"))
      message("PNN is saved in:", file.path(dirpath, "test/PPN.mtx"), "\n")
    }

    return(conn_mx)
  }

  indata <- X#Matrix::readMM(file.path(dirpath, "X.mtx"))
  indata@x[indata@x > 0] <- 1
  indata <- t(indata)

  # format cell info
  #cell_data <- read.csv(file.path(dirpath, "cell_data.csv"))
  #colnames(cell_data) <- c('rank','cell_name')
  cellinfo <- data.frame(v1 = cell_data$cell_name)
  row.names(cellinfo) <- cellinfo$v1
  names(cellinfo) <- "cells"

  # format peak info
  #peak_data <- read.csv(file.path(dirpath, "peak_data.csv"))
  #colnames(peak_data) <- c('rank', 'peak_name')
  peak_name <- peak_data$peak_name
  peakinfo=str_split_fixed(peak_name,"-",2)%>%data.frame()
  peakinfo[,c(3,4)] <- str_split_fixed(peakinfo$X2,"-",2)
  peakinfo[,5] <- peak_name
  peakinfo <- peakinfo[,-2]
  peakinfo <- peakinfo[,-4]
  names(peakinfo) <- c("chr", "bp1", "bp2")
  peakinfo$site_name <- paste(peakinfo$chr, peakinfo$bp1, peakinfo$bp2, sep="_")
  row.names(peakinfo) <- peakinfo$site_name

  row.names(indata) <- row.names(peakinfo)
  colnames(indata) <- row.names(cellinfo)

  fd <- methods::new("AnnotatedDataFrame", data = peakinfo)
  pd <- methods::new("AnnotatedDataFrame", data = cellinfo)

  tic()
  pp_net <- use_cicero(indata, fd, pd, peakinfo, genome, dirpath)
  toc()
  return(pp_net)
}
