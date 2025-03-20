## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(Signac)
#  library(Seurat)
#  library(SeuratDisk)
#  library(EnsDb.Hsapiens.v86)
#  library(BSgenome.Hsapiens.UCSC.hg38)
#  set.seed(1234)

## -----------------------------------------------------------------------------
#  # Download files
#  system('wget https://cf.10xgenomics.com/samples/cell-arc/1.0.0/pbmc_granulocyte_sorted_10k/pbmc_granulocyte_sorted_10k_filtered_feature_bc_matrix.h5')
#  system('wget https://cf.10xgenomics.com/samples/cell-arc/1.0.0/pbmc_granulocyte_sorted_10k/pbmc_granulocyte_sorted_10k_atac_fragments.tsv.gz')
#  system('wget https://cf.10xgenomics.com/samples/cell-arc/1.0.0/pbmc_granulocyte_sorted_10k/pbmc_granulocyte_sorted_10k_atac_fragments.tsv.gz.tbi')
#  
#  # Load in ATAC and RNA data
#  counts <- Read10X_h5("pbmc_granulocyte_sorted_10k_filtered_feature_bc_matrix.h5")
#  fragpath <- "pbmc_granulocyte_sorted_10k_atac_fragments.tsv.gz"
#  

## -----------------------------------------------------------------------------
#  # create a Seurat object containing the RNA adata
#  pbmc <- CreateSeuratObject(
#    counts = counts$`Gene Expression`,
#    assay = "RNA"
#  )
#  
#  # create ATAC assay and add it to the object
#  pbmc[["ATAC"]] <- CreateChromatinAssay(
#    counts = counts$Peaks,
#    sep = c(":", "-"),
#    fragments = fragpath,
#    annotation = annotation
#  )
#  
#  DefaultAssay(pbmc) <- "ATAC"
#  pbmc <- NucleosomeSignal(pbmc)
#  pbmc <- TSSEnrichment(pbmc)
#  
#  pbmc <- subset(
#    x = pbmc,
#    subset = nCount_ATAC < 100000 &
#      nCount_RNA < 25000 &
#      nCount_ATAC > 1000 &
#      nCount_RNA > 1000 &
#      nucleosome_signal < 2 &
#      TSS.enrichment > 1
#  )
#  
#  # Transform RNA
#  DefaultAssay(pbmc) <- "RNA"
#  pbmc <- SCTransform(pbmc)
#  pbmc <- RunPCA(pbmc)
#  pbmc[["percent.mt"]] <- PercentageFeatureSet(pbmc, pattern = "^MT-")

## -----------------------------------------------------------------------------
#  # load PBMC reference
#  system('wget https://atlas.fredhutch.org/data/nygc/multimodal/pbmc_multimodal.h5seurat')
#  reference <- LoadH5Seurat("pbmc_multimodal.h5seurat")

## -----------------------------------------------------------------------------
#  DefaultAssay(pbmc) <- "SCT"
#  
#  # transfer cell type labels from reference to query
#  transfer_anchors <- FindTransferAnchors(
#    reference = reference,
#    query = pbmc,
#    normalization.method = "SCT",
#    reference.reduction = "spca",
#    recompute.residuals = FALSE,
#    dims = 1:50
#  )
#  
#  predictions <- TransferData(
#    anchorset = transfer_anchors,
#    refdata = reference$celltype.l2,
#    weight.reduction = pbmc[['pca']],
#    dims = 1:50
#  )
#  
#  pbmc <- AddMetaData(
#    object = pbmc,
#    metadata = predictions
#  )

## -----------------------------------------------------------------------------
#  # set the cell identities to the cell type predictions
#  Idents(pbmc) <- "predicted.id"
#  
#  # set a reasonable order for cell types to be displayed when plotting
#  levels(pbmc) <- c("CD4 Naive", "CD4 TCM", "CD4 CTL", "CD4 TEM", "CD4 Proliferating",
#                    "CD8 Naive", "dnT",
#                   "CD8 TEM", "CD8 TCM", "CD8 Proliferating", "MAIT", "NK", "NK_CD56bright",
#                   "NK Proliferating", "gdT",
#                   "Treg", "B naive", "B intermediate", "B memory", "Plasmablast",
#                   "CD14 Mono", "CD16 Mono",
#                   "cDC1", "cDC2", "pDC", "HSPC", "Eryth", "ASDC", "ILC", "Platelet")
#  
#  saveRDS(pbmc, 'pbmc_Signac_tutorial.rds')

## -----------------------------------------------------------------------------
#  pbmc <- readRDS('pbmc_Signac_tutorial.rds')
#  
#  DefaultAssay(pbmc) <- 'ATAC'
#  fragObj <- Fragments(pbmc)

## -----------------------------------------------------------------------------
#  full_pbmc <- merge(
#    pbmc,
#    y = c(pbmc, pbmc),
#    add.cell.ids = c('Sample1', 'Sample2', 'Sample3'),
#    project = 'DuplicateData'
#  )

## -----------------------------------------------------------------------------
#  fragList <- parallel::mclapply(1:3, function(x){
#      frags <- read.table(GetFragmentData(fragObj[[1]]))
#      names(frags) <- c('chr', 'start', 'end', 'barcode', 'val')
#      frags$barcode <- paste("Sample",x,"_", frags$barcode, sep ='')
#      frags
#  }, mc.cores = 3)
#  names(fragList) <- c('Sample1', 'Sample2', 'Sample3')

## -----------------------------------------------------------------------------
#  celltype_sample_list <- apply(expand.grid(unique(pbmc@meta.data$predicted.id), names(fragList)), 1, paste, collapse = "#")
#  # Extract metadata, add the Sample column, as well as CellBarcode.
#  fullMeta <- full_pbmc@meta.data
#  fullMeta$Sample = gsub("_.*","", rownames(full_pbmc@meta.data))
#  fullMeta$CellBarcode = gsub(".*_","", rownames(full_pbmc@meta.data))
#  
#  # Change the CellPopulation_Sample format to CellPopulation#Sample for MOCHA
#  rownames(fullMeta) <- gsub("_","#", rownames(fullMeta))
#  CellType_GRanges <- pbapply::pblapply(cl = 30, celltype_sample_list, function(x){
#    celltype <- gsub("#.*","", x)
#    sample <- gsub(".*#","", x)
#    sortedMeta <- dplyr::filter(fullMeta, predicted.id == celltype, Sample == sample)
#    sortedFrags <- dplyr::filter(fragList[[sample]], barcode %in% rownames(sortedMeta))
#    makeGRangesFromDataFrame(sortedFrags, keep.extra.columns = TRUE)
#  })
#  
#  names(CellType_GRanges) <- celltype_sample_list

## -----------------------------------------------------------------------------
#  avg_reads <- lapply(fragList, function(x){
#                  filtFrag <- dplyr::filter(as.data.frame(x), barcode  %in% rownames(fullMeta))
#                  as.vector(table(filtFrag$barcode))
#      })
#  studySignal <- median(unlist(avg_reads))

## -----------------------------------------------------------------------------
#  # Our blacklist comes included with Signac
#  blackList <- blacklist_hg38_unified
#  # Call Open Tiles
#  tileResults <- MOCHA::callOpenTiles(ATACFragments = CellType_GRanges,
#                               cellColData = fullMeta,
#                               blackList = blackList,
#                               genome = 'BSgenome.Hsapiens.UCSC.hg38',
#                               cellPopLabel = 'predicted.id',
#                               cellPopulations = fullMeta$predicted.id,
#                               studySignal = studySignal,
#                               cellCol = 'barcode',
#                               TxDb = "TxDb.Hsapiens.UCSC.hg38.refGene",
#                               Org = "org.Hs.eg.db",
#                              outDir = paste(getwd(),'/MOCHA_Out', sep = ''), numCores= 35)
#  
#  TSAM <- MOCHA::getSampleTileMatrix(tileResults, threshold = 0.2, numCores = 3, verbose = TRUE)

## -----------------------------------------------------------------------------
#  library(SnapATAC);
#  snap.files = c(
#    "atac_pbmc_5k_nextgem.snap",
#    "atac_pbmc_10k_nextgem.snap"
#  );
#  sample.names = c(
#    "PBMC 5K",
#    "PBMC 10K"
#  );
#  barcode.files = c(
#    "atac_pbmc_5k_nextgem_singlecell.csv",
#    "atac_pbmc_10k_nextgem_singlecell.csv"
#  );
#  x.sp.ls = lapply(seq(snap.files), function(i){
#    createSnap(
#        file=snap.files[i],
#        sample=sample.names[i]
#    );
#  })
#  names(x.sp.ls) = sample.names;
#  barcode.ls = lapply(seq(snap.files), function(i){
#    barcodes = read.csv(
#        barcode.files[i],
#        head=TRUE
#    );
#    barcodes = barcodes[2:nrow(barcodes),];
#    barcodes$logUMI = log10(barcodes$passed_filters + 1);
#    barcodes$promoter_ratio = (barcodes$promoter_region_fragments+1) / (barcodes$passed_filters + 1);
#    barcodes
#  })
#  x.sp.ls

## -----------------------------------------------------------------------------
#  # for both datasets, we identify usable barcodes using [3.5-5] for log10(UMI) and [0.4-0.8] for promoter ratio as cutoff.
#  cutoff.logUMI.low = c(3.5, 3.5);
#  cutoff.logUMI.high = c(5, 5);
#  cutoff.FRIP.low = c(0.4, 0.4);
#  cutoff.FRIP.high = c(0.8, 0.8);
#  barcode.ls = lapply(seq(snap.files), function(i){
#    barcodes = barcode.ls[[i]];
#    idx = which(
#        barcodes$logUMI >= cutoff.logUMI.low[i] &
#        barcodes$logUMI <= cutoff.logUMI.high[i] &
#        barcodes$promoter_ratio >= cutoff.FRIP.low[i] &
#        barcodes$promoter_ratio <= cutoff.FRIP.high[i]
#    );
#    barcodes[idx,]
#  });
#  x.sp.ls = lapply(seq(snap.files), function(i){
#    barcodes = barcode.ls[[i]];
#    x.sp = x.sp.ls[[i]];
#    barcode.shared = intersect(x.sp@barcode, barcodes$barcode);
#    x.sp = x.sp[match(barcode.shared, x.sp@barcode),];
#    barcodes = barcodes[match(barcode.shared, barcodes$barcode),];
#    x.sp@metaData = barcodes;
#    x.sp
#  })
#  names(x.sp.ls) = sample.names;
#  x.sp.ls

## -----------------------------------------------------------------------------
#  # combine two snap object
#  x.sp = Reduce(snapRbind, x.sp.ls);
#  x.sp@metaData["Sample"] = x.sp@sample;
#  print(table(x.sp@sample))
#  x.sp

## -----------------------------------------------------------------------------
#  # Step 2. Add cell-by-bin matrix
#  x.sp = addBmatToSnap(x.sp, bin.size=5000);
#  # Step 3. Matrix binarization
#  x.sp = makeBinary(x.sp, mat="bmat");
#  # Step 4. Bin filtering
#  library(GenomicRanges);
#  black_list = read.table("hg19.blacklist.bed.gz");
#  black_list.gr = GRanges(
#    black_list[,1],
#    IRanges(black_list[,2], black_list[,3])
#  );
#  idy = queryHits(
#    findOverlaps(x.sp@feature, black_list.gr)
#  );
#  if(length(idy) > 0){
#    x.sp = x.sp[,-idy, mat="bmat"];
#  };
#  x.sp
#  
#  # Remove unwanted chromosomes
#  chr.exclude = seqlevels(x.sp@feature)[grep("random|chrM", seqlevels(x.sp@feature))];
#  idy = grep(paste(chr.exclude, collapse="|"), x.sp@feature);
#  if(length(idy) > 0){
#    x.sp = x.sp[,-idy, mat="bmat"]
#  };
#  x.sp

## -----------------------------------------------------------------------------
#  # The coverage of bins roughly obeys a log normal distribution. We remove the top 5% bins that overlap with invariant features such as the house keeping gene promoters.
#  bin.cov = log10(Matrix::colSums(x.sp@bmat)+1);
#  hist(
#    bin.cov[bin.cov > 0],
#    xlab="log10(bin cov)",
#    main="log10(Bin Cov)",
#    col="lightblue",
#    xlim=c(0, 5)
#  );
#  bin.cutoff = quantile(bin.cov[bin.cov > 0], 0.95);
#  idy = which(bin.cov <= bin.cutoff & bin.cov > 0);
#  x.sp = x.sp[, idy, mat="bmat"];
#  x.sp
#  
#  # We will further remove any cells of bin coverage less than 1,000. The rational behind this is that some cells may have high number of unique fragments but end up with low bin coverage after filtering. This step is optional but highly recommended.
#  idx = which(Matrix::rowSums(x.sp@bmat) > 1000);
#  x.sp = x.sp[idx,];
#  x.sp

## -----------------------------------------------------------------------------
#  # Step 5. Dimensionality reduction
#  row.covs.dens <- density(
#    x = x.sp@metaData[,"logUMI"],
#    bw = 'nrd', adjust = 1
#  );
#  sampling_prob <- 1 / (approx(x = row.covs.dens$x, y = row.covs.dens$y, xout = x.sp@metaData[,"logUMI"])$y + .Machine$double.eps);
#  set.seed(1);
#  idx.landmark.ds <- base::sort(sample(x = seq(nrow(x.sp)), size = 10000, prob = sampling_prob));
#  x.landmark.sp = x.sp[idx.landmark.ds,];
#  x.query.sp = x.sp[-idx.landmark.ds,];
#  x.landmark.sp = runDiffusionMaps(
#    obj= x.landmark.sp,
#    input.mat="bmat",
#    num.eigs=50
#  );
#  x.landmark.sp@metaData$landmark = 1;
#  x.query.sp = runDiffusionMapsExtension(
#    obj1=x.landmark.sp,
#    obj2=x.query.sp,
#    input.mat="bmat"
#  );
#  x.query.sp@metaData$landmark = 0;
#  x.sp = snapRbind(x.landmark.sp, x.query.sp);
#  x.sp = x.sp[order(x.sp@metaData["sample"])];
#  x.sp = runKNN(
#    obj=x.sp,
#    eigs.dims=1:20,
#    k=15
#  );
#  x.sp=runCluster(
#    obj=x.sp,
#    tmp.folder=tempdir(),
#    louvain.lib="R-igraph", #"leiden" preferred, but may cause issues. Requires 'library(leiden)'.
#    seed.use=10,
#    resolution=0.7
#  )
#  

## -----------------------------------------------------------------------------
#  # Add clusters (from SnapATAC::runCluster) to metadata
#  x.sp@metaData$cluster = x.sp@cluster
#  
#  # Add Sample name to metadata (if not done previously)
#  x.sp@metaData$Sample = x.sp@sample
#  
#  # Add files to metadata, indicating the original snap file each cell belongs to.
#  snap.files <- c(
#    "atac_pbmc_5k_nextgem.snap",
#    "atac_pbmc_10k_nextgem.snap"
#  )
#  fileList <- unlist(lapply(x.sp@metaData$Sample, function(x){
#    ifelse(x == "PBMC 5K", snap.files[[1]],snap.files[[2]])
#  }))
#  x.sp@metaData$files <- fileList
#  
#  # SAVE this metadata to disk
#  write.csv(x.sp@metaData, "./snapMetadataforMOCHA.csv")

## -----------------------------------------------------------------------------
#  snapMetadata <- read.csv("./snapMetadataforMOCHA.csv")
#  cellCol <- "barcode"
#  cellPopLabel <- "cluster"
#  cellPopulations <- unique(snapMetadata$cluster)
#  allSamples <- unique(snapMetadata$Sample)
#  
#  fragmentsGRangesList <- unlist(lapply(allSamples, function(sample){
#    barcodesList <- lapply(cellPopulations, function(cellPop) {
#      snapMetadata[snapMetadata$Sample == sample,]
#      # Extract barcodes for a single cell population
#      cellPopIdx <- snapMetadata$cluster == cellPop
#      cellPopBarcodes <- snapMetadata[cellPopIdx,]$barcode
#  
#      # Build the file list for the selected cell barcode
#      files <- snapMetadata[cellPopIdx,]$files
#  
#      # Extract fragments
#      cellPopFrags <- SnapATAC::extractReads(cellPopBarcodes, files, do.par = FALSE)
#    })
#    names(barcodesList) <- paste(cellPopulations, sample, sep="#")
#    barcodesList
#  }))

## -----------------------------------------------------------------------------
#  avg_reads <- lapply(fragmentsGRangesList, function(x){
#    filtFrag <- dplyr::filter(as.data.frame(x), barcode  %in% snapMetadata$barcode)
#    as.vector(table(filtFrag$barcode))
#  })
#  studySignal <- median(unlist(avg_reads))

## -----------------------------------------------------------------------------
#  # Call Open Tiles
#  tileResults <- MOCHA::callOpenTiles(
#    ATACFragments = fragmentsGRangesList,
#    cellColData = snapMetadata,
#    blackList = black_list.gr,
#    genome = "BSgenome.Hsapiens.UCSC.hg38",
#    cellPopLabel = cellPopLabel,
#    cellPopulations = cellPopulations,
#    studySignal = studySignal,
#    cellCol = cellCol,
#    TxDb = "TxDb.Hsapiens.UCSC.hg38.refGene",
#    Org = "org.Hs.eg.db",
#    outDir = paste(getwd(),'/MOCHA_Out', sep = ''),
#    numCores = 5
#  )
#  
#  TSAM <- MOCHA::getSampleTileMatrix(tileResults, threshold = 0.2, numCores = 3, verbose = TRUE)

