## ----eval=FALSE---------------------------------------------------------------
#  install.packages("scCAN")

## -----------------------------------------------------------------------------
library(scCAN)

## ----eval=TRUE----------------------------------------------------------------
#Load example data (SCE dataset)
data("SCE")
#Get data matrix and label
data <- t(SCE$data); label <- as.character(SCE$cell_type1)


## -----------------------------------------------------------------------------
data[1:10,1:10]
dim(data)

## ----eval=FALSE---------------------------------------------------------------
#  #Generate clustering result. The input matrix has rows as samples and columns as genes
#  result <- scCAN(data)
#  #The clustering result can be found here
#  cluster <- result$cluster

## ----eval=FALSE---------------------------------------------------------------
#  suppressPackageStartupMessages({
#  library(irlba)
#  library(ggplot2)
#  library(Rtsne)
#  })
#  # Get 2D emebedding data of the original data.
#  set.seed(1)
#  low <- irlba::irlba(data, nv = 20)$u
#  tsne <- as.data.frame(Rtsne::Rtsne(low)$Y)
#  tsne$cluster <- as.character(cluster)
#  colnames(tsne) <- c("t-SNE1","t-SNE2","Cluster")
#  p <- ggplot2::ggplot(tsne, aes(x = `t-SNE1`, y = `t-SNE2`, colour = `Cluster`))+
#    ggtitle("Transcriptome landscape visualization using t-SNE")+labs(color='Cluster') +
#    geom_point(size=2, alpha = 0.8) +
#      theme_classic()+
#      theme(axis.text.x=element_blank(),
#            axis.ticks.x=element_blank(),
#            axis.text.y=element_blank(),
#            axis.ticks.y=element_blank(),
#            axis.title=element_text(size=14),
#            plot.title = element_text(size=16),
#            panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            legend.position="bottom", legend.box = "horizontal",
#            legend.text =element_text(size=10),
#            legend.title=element_text(size=14))+
#      guides(colour=guide_legend(nrow = 1))
#  p

## ----eval=FALSE---------------------------------------------------------------
#  suppressPackageStartupMessages({
#  library(SummarizedExperiment)
#  })
#  path <- "https://bioinformatics.cse.unr.edu/software/scCAN/data/pollen.rds"
#  SCE <- readRDS(url(path,"rb"))
#  # Get expression matrix
#  data <- t(SummarizedExperiment::assay(SCE))
#  # Get cell annotation
#  label <- SummarizedExperiment::colData(SCE)

## ----eval=FALSE---------------------------------------------------------------
#  #Generate clustering result, the input matrix has rows as samples and columns as genes
#  start <- Sys.time()
#  result <- scCAN(data)
#  running_time <- difftime(Sys.time(),start,units = "mins")
#  print(paste0("Running time = ", running_time))
#  #The clustering result can be found here
#  cluster <- result$cluster

## ----eval=FALSE---------------------------------------------------------------
#  head(label)

## ----eval=FALSE---------------------------------------------------------------
#  #Calculate adjusted Rand Index using mclust package
#  ari <- round(scCAN::adjustedRandIndex(cluster,label$cell_type1), 2)
#  print(paste0("ARI = ", ari))

## ----eval=FALSE---------------------------------------------------------------
#  suppressPackageStartupMessages({
#    library(ggplot2)
#    library(cowplot)
#    library(scatterplot3d)
#    library(scales)
#  })
#    # Get latent data from scCAN result
#    latent <- result$latent
#    # Generating 2D embedding data using 2 first latent variables
#    data1= latent[,1:2]
#  
#    # Generating 3D embedding data using 3 first latent variables
#    data2= latent[,1:3]
#  
#    #Plotting 2D data using ggplot
#    name ="Pollen"
#    dat <- as.data.frame(data1)
#    dat$p <- as.character(cluster)
#    colnames(dat) <- c("Latent Variable 1", "Latent Variable 2", "scCAN Cluster")
#    p1 <- ggplot(dat, aes(x = `Latent Variable 1`,y = `Latent Variable 2`,colour = `scCAN Cluster`)) +
#      theme(plot.title = element_text(hjust = 0.5)) +
#      geom_point(size=2, alpha = 0.6) +
#      #scale_color_manual(values = colors)+
#      theme_classic()+
#      theme(axis.text.x=element_blank(),
#            axis.ticks.x=element_blank(),
#            axis.text.y=element_blank(),
#            axis.ticks.y=element_blank(),
#            axis.title=element_text(size=14),
#            panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            legend.position="none", legend.box = "horizontal",
#            legend.text =element_text(size=12),
#            legend.title=element_text(size=14) )+
#      guides(colour=guide_legend(ncol=2))
#    title1 <- ggdraw() + draw_label(name, size=16)
#    legend1 <- get_legend(p1 +
#                            theme(legend.position="bottom", legend.box = "horizontal",legend.title=element_text(size=12),
#                                  legend.margin = margin(0), legend.spacing.x = unit(0.1, "cm") ,
#                                  legend.text=element_text(size=12)) + guides(color=guide_legend(nrow = 2))
#    )
#  
#    m1 <- plot_grid(title1, p1, legend1, rel_heights = c(0.05, 0.5, .1), ncol = 1)
#    plot_grid(m1,rel_heights = c(0.05, 0.9), ncol = 1)
#  
#  
#    #Plotting 3D data using scatterplot3d package
#    graphics.off()
#    name = "Pollen"
#    dat <- as.data.frame(data2)
#    # dat$t <- label$cell_type1
#    dat$p <- as.character(cluster)
#    colnames(dat) <- c("Latent Variable 1", "Latent Variable 2", "Latent Variable 3", "scCAN Cluster")
#    pal <- hue_pal()(length(unique(cluster)))
#    colors <- pal[as.numeric(dat$`scCAN Cluster`)]
#  
#    scatterplot3d(dat[,1:3], pch = 16,
#                  grid=TRUE, box=TRUE,color = colors)
#  

## ----eval=FALSE---------------------------------------------------------------
#    suppressPackageStartupMessages({
#    library(umap)
#    })
#    set.seed(1)
#    pc.data <- prcomp(latent, rank. = 2)$x
#    tsne.data <- Rtsne(latent, check_duplicates = F,dims = 2)$Y
#    umap.data <- umap(latent,n_components = 2)$layout

## ----eval=FALSE---------------------------------------------------------------
#  suppressPackageStartupMessages({
#    library(Rtsne)
#    library(umap)
#  })
#    # Get latent data from scCAN result
#    latent <- result$latent
#    set.seed(1)
#    pc.data <- prcomp(latent, rank. = 3)$x
#    tsne.data <- Rtsne(latent, check_duplicates = F,dims = 3)$Y
#    umap.data <- umap(latent,n_components = 3)$layout

## ----eval=FALSE---------------------------------------------------------------
#  suppressPackageStartupMessages({
#    library(ggplot2)
#    library(cowplot)
#    library(scatterplot3d)
#    library(scales)
#  })
#    graphics.off()
#    name = "Pollen"
#    par(mfrow=c(1,3))
#  
#    pc.data <- as.data.frame(pc.data)
#    pc.data$t <- label
#    pc$p <- as.character(result$cluster)
#    colnames(pc) <- c("PC1","PC2","PC3","True Cluster","scCAN Cluster" )
#    pal <- hue_pal()(length(result$cluster)))
#    colors <- pal[as.numeric(tsne$`scCAN Cluster`)]
#  
#    scatterplot3d(pc.data[,1:3], pch = 16,
#                    grid=TRUE, box=TRUE,color = colors)
#  
#    tsne <- as.data.frame(tsne.data)
#    tsne$t <- label
#    tsne$p <- as.character(result$cluster)
#    colnames(tsne) <- c("t-SNE1","t-SNE2","t-SNE3","True Cluster","scCAN Cluster" )
#    pal <- hue_pal()(length(unique(result$cluster)))
#    colors <- pal[as.numeric(tsne$`scCAN Cluster`)]
#  
#    scatterplot3d(tsne[,1:3], pch = 16,
#                  grid=TRUE, box=TRUE,color = colors)
#  
#  
#    umap <- as.data.frame(umap.data)
#    umap$t <- label
#    umap$p <- as.character(result$cluster)
#    colnames(umap) <- c("UMAP1","UMAP2","UMAP3","True Cluster","scCAN Cluster" )
#  
#    scatterplot3d(umap[,1:3], pch = 16,
#                  grid=TRUE, box=TRUE,color = colors)
#  
#    mtext(name,side=3,line=-1.5,outer=TRUE,cex = 1.4)

## ----eval=FALSE---------------------------------------------------------------
#  suppressPackageStartupMessages({
#  library(SummarizedExperiment)
#  })
#  path <- "https://bioinformatics.cse.unr.edu/software/scCAN/data/zilionis.rds"
#  SCE <- readRDS(url(path,"rb"))
#  # Get expression matrix
#  data <- t(SummarizedExperiment::assay(SCE))
#  # Get cell annotation
#  label <- SummarizedExperiment::colData(SCE)
#  
#  # Detect rare cell types by increasing sample size and reducing number of neighboring cells
#  res <- scCAN(data, samp.size = 10000, n.neighbors = 10)
#  
#  # Detect rare cell types by performing two-stage clustering analysis
#  # Stage 1: Perform clustering on the whole data using scCAN with default settings
#  res <- scCAN(data)
#  cluster <- res$cluster
#  
#  # Stage 2: Perform clustering on each cluster obtain from stage 1
#  cluster2 <- rep(NA, length(cluster))
#  for (cl in unique(cluster)){
#    idx <- which(cl == cluster)
#    tmp <- scCAN(data[idx, ])
#    cluster2[idx] <- paste0(cl, "_", tmp$cluster)
#  }
#  res$cluster2 <- cluster2

## ----eval=FALSE---------------------------------------------------------------
#  # Loading
#  # if (!require("BiocManager", quietly = TRUE))
#  #     install.packages("BiocManager")
#  # BiocManager::install("DelayedArray")
#  # BiocManager::install("DelayedMatrixStats")
#  # BiocManager::install("TENxBrainData")
#  # BiocManager::install("HDF5Array")
#  suppressPackageStartupMessages({
#    library(TENxBrainData)
#    library(Matrix)
#    library(SummarizedExperiment)
#    library(DelayedArray)
#    library(DelayedMatrixStats)
#    library(HDF5Array)
#  })
#  tenx <- TENxBrainData()
#  counts <- assay(tenx)
#  data <- t(counts)
#  gene.names <- rowData(tenx)
#  cell.names <- colData(tenx)
#  rownames(data) <- cell.names$Barcode
#  colnames(data) <- gene.names$Ensembl

## ----eval=FALSE---------------------------------------------------------------
#  # Set number of processing unit to 20
#  setAutoBPPARAM(BiocParallel::MulticoreParam(workers=4))
#  # Calculate total count for each gene
#  total.count <- DelayedArray::colSums(data)
#  # Filter gene that has total count less than 1 percent of maximum total count.
#  setAutoBPPARAM(BiocParallel::MulticoreParam(workers=4))
#  idx <- which(total.count <= (max(total.count*1/100)))
#  data <- data[,-idx]
#  genes <- as.vector(gene.names$Ensembl)[-idx]
#  colnames(data) <- genes

## ----eval=FALSE---------------------------------------------------------------
#  # Devide data into different block
#  trunks<- seq(from = 1, to = nrow(data))
#  seg <- split(trunks, ceiling(seq_along(trunks)/50000))
#  # Convert each block to sparse dgCMatrix and combine them into a single one
#  mat = NULL
#  for(i in 1:length(seg)){
#    tmp <- as(data[seg[[i]],], "dgCMatrix")
#    mat = rbind(mat,tmp)
#  }
#  rownames(mat) <- cell.names$Barcode
#  colnames(mat) <- genes
#  # Set the number of clusters k = 10:50 (k = 2:15 by default) for scCAN to maximize the number clusters that can be explored.
#  start <- Sys.time()
#  res <- scCAN(mat,sparse = T,r.seed = 1,k = 10:50)
#  running_time <- difftime(Sys.time(),start,units = "mins")
#  cluster <- res$cluster
#  message("Running time = ", running_time)
#  message("Number of cluster = ", unique(cluster))
#  

## ----eval=FALSE---------------------------------------------------------------
#  # Download the cell type with markers gene
#  url <- "https://panglaodb.se/markers/PanglaoDB_markers_27_Mar_2020.tsv.gz"
#  tmp <- tempfile()
#  download.file(url,tmp)
#  file = gzfile(tmp)
#  markers <- read.table(file, sep="\t", header = T)
#  # Select only brain tissue
#  markers = markers[which(markers$organ=="Brain"),]
#  # Remove marker that belongs to human
#  markers = markers[c(which(markers$species=="Mm Hs"),which(markers$species=="Mm")),]
#  # Select cell types that have more than 15 markers
#  breaks = c(15)
#  for(br in breaks){
#    tmp <- as.data.frame(table(markers$cell.type))
#  
#    if(is.null(br)){
#      select_mrks <- tmp
#    }else{
#      select_mrks <- tmp[which(tmp$Freq>=br),]
#    }
#    cts <- unique(select_mrks$Var1)
#    panglao_markers <- list()
#    # ann <- NULL
#    for( ct in cts){
#      idx <- which(markers$cell.type==ct)
#      tmp <- markers[idx,]
#      panglao_markers[[ct]]<-tmp$official.gene.symbol
#    }
#    saveRDS(panglao_markers,paste0("Panglao_mrks_",br,".rds"))
#  }
#  # Load the expression matrix of markers genes collected from PanglaoDB database
#  path <- "https://bioinformatics.cse.unr.edu/software/scCAN/data/1.3M_markers.rds"
#  gene_expression_data <- readRDS(url(path,"rb"))
#  # Load saved scCAN clustering result
#  path <-"https://bioinformatics.cse.unr.edu/software/scCAN/result/1.3M_scCAN.rds"
#  res <- readRDS(url(path,"rb"))
#  cluster <- res$cluster
#  # Evaluate wilcox rank sum test and log fold change values
#  # Perform pair wise Wilcoxon Rank Sum and log fold change for each cluster
#  # This process could take hours for big data
#  eval_gene_markers <- scCAN::get_cluster_markers(gene_expression_data,
#                                                cluster,
#                                                threads = 16)
#  gene_names <- rownames(gene_expression_data)
#  # Select marker the test with p <= 0.001 and log fold change > 1.5,
#  # output is the list of markers that are strongly differentiated in the clusters
#  cluster_markers_list <- scCAN::curate_markers(eval_gene_markers, gene_names,
#                                                wilcox_threshold=0.001,
#                                                logfc_threshold=1.5)

## ----eval=FALSE---------------------------------------------------------------
#  library(matrixStats)
#  # Load pre-saved the list of markers that are strongly expressed in a specific cluster
#  path <-"https://bioinformatics.cse.unr.edu/software/scCAN/result/cluster_markers.rds"
#  cluster_markers_list <- readRDS(url(path,"rb"))
#  # Load PanglaoDB markers
#  path <-"https://bioinformatics.cse.unr.edu/software/scCAN/result/panglao_markers.rds"
#  panglao_markers<- readRDS(url(path,"rb"))
#  # Caculate pair wise cell type/cluster probabilty using jaccard index
#  celltype_prob <- scCAN::calculate_celltype_prob(cluster_markers_list,
#                                                  panglao_markers,
#                                                  type = "jacc")
#  colnames(celltype_prob) <- names(panglao_markers)
#  celltype_id <- matrixStats::rowMaxs(celltype_prob)
#  celltype_prob[1:10,1:10]

## ----eval=FALSE---------------------------------------------------------------
#  library(pheatmap)
#  pheatmap(celltype_prob)

## ----eval=FALSE---------------------------------------------------------------
#  path <-"https://bioinformatics.cse.unr.edu/software/scCAN/result/1.3M_scCAN.rds"
#  res <- readRDS(url(path,"rb"))
#  cluster <- res$cluster
#  # Map the cluster that has highest Jaccard Index value with reference cell type
#  celltype_idx <- apply(celltype_prob,1,function(x){which.max(x)}  )
#  cell.name <- colnames(celltype_prob)[celltype_idx]
#  # Assign reference cell type to all clusters discover by scCAN
#  mapping.table <- data.frame(cluster = seq(1:19), cell_type = cell.name)
#  mapped.ct <- mapping.table$cell_type[mapping.table$cluster[cluster]]
#  mapped.ct[1:10]

