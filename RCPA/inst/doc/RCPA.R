## ----echo=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(eval = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  unloadNamespace("RCPA")
#  library(RCPA)
#  library(SummarizedExperiment)
#  library(ggplot2)
#  library(gridExtra)

## ----eval=FALSE---------------------------------------------------------------
#  # User-defined directory to save the downloaded data
#  downloadPath <- file.path(getwd(), "GSE5281")
#  # Create the directory if it does not exist
#  if(!dir.exists(downloadPath)) dir.create(downloadPath)
#  # download the data
#  downloadedFiles <- RCPA::downloadGEO(GEOID = "GSE5281", platform = "GPL570", protocol = "affymetrix", destDir = downloadPath)

## ----eval=FALSE---------------------------------------------------------------
#  # read the metadata file
#  affySampleInfo <- read.csv(file.path(downloadPath, "metadata.csv"))
#  
#  # read the CEL files
#  affyExprs <- RCPA::processAffymetrix(dir = downloadPath, samples = affySampleInfo$geo_accession)
#  
#  # create the SummarizedExperiment object
#  affyDataset <- SummarizedExperiment::SummarizedExperiment(assays = affyExprs, colData = affySampleInfo)

## ----eval=FALSE---------------------------------------------------------------
#  # Access to assay data
#  affyExprs <- SummarizedExperiment::assay(affyDataset)
#  # Access to sample information
#  affySampleInfo <- SummarizedExperiment::colData(affyDataset)

## ----eval=FALSE---------------------------------------------------------------
#  # User-defined directory to save the downloaded data
#  downloadPath <- file.path(getwd(), "GSE61196")
#  # Create the directory if it does not exist
#  if(!dir.exists(downloadPath)) dir.create(downloadPath)
#  # download the data
#  downloadedFiles <- RCPA::downloadGEO(GEOID = "GSE61196", platform = "GPL4133", protocol = "agilent", destDir = downloadPath)

## ----eval=FALSE---------------------------------------------------------------
#  # read the metadata file
#  agilSampleInfo <- read.csv(file.path(downloadPath, "metadata.csv"))
#  
#  # read the TXT files
#  agilExprs <- processAgilent(dir = downloadPath, samples = agilSampleInfo$geo_accession, greenOnly = FALSE)
#  
#  # create the SummarizedExperiment object
#  agilDataset <- SummarizedExperiment::SummarizedExperiment(assays = agilExprs, colData = agilSampleInfo)

## ----eval=FALSE---------------------------------------------------------------
#  # Access to assay data
#  agilExprs <- SummarizedExperiment::assay(agilDataset)
#  # Access to sample information
#  agilSampleInfo <- SummarizedExperiment::colData(agilDataset)

## ----eval=FALSE---------------------------------------------------------------
#  # Specify the GEO accession ID
#  GEOID <- "GSE153873"
#  # Create a download path
#  downloadPath <- getwd()
#  if(!dir.exists(downloadPath)) dir.create(downloadPath)
#  # Download the data
#  GEOquery::getGEOSuppFiles(GEOID, fetch_files = TRUE, baseDir = downloadPath)

## ----eval=FALSE---------------------------------------------------------------
#  # Specify the path to the downloaded read counts file
#  countsFile <- file.path(downloadPath, GEOID, "GSE153873_summary_count.star.txt.gz")
#  # Read the downloaded file
#  countsData <- read.table(countsFile, header = TRUE, sep = "\t", fill = 0, row.names = 1, check.names = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  # Download the GEO object to get metadata
#  GEOObject <- GEOquery::getGEO(GEOID, GSEMatrix = T, getGPL = T, destdir = downloadPath)
#  # Check the length of GEOObject
#  print(length(GEOObject))
#  # Extract the dataset from the GEOObject
#  samplesData <- GEOObject[[1]]
#  # Export sample data
#  metadata <- Biobase::pData(samplesData)

## ----eval=FALSE---------------------------------------------------------------
#  # Get the sample IDs in the GEO accession ID form
#  sampleIDs <- rownames(metadata)
#  # Get the sample titles
#  sampleTitles <- metadata[, "title"]
#  # Reorder the columns in the assay data
#  countsData <- countsData[,sampleTitles]
#  # Assign sample IDs to columns' labels
#  colnames(countsData) <- sampleIDs

## ----eval=FALSE---------------------------------------------------------------
#  # Create the SummarizedExperiment object
#  RNASeqDataset <- SummarizedExperiment::SummarizedExperiment(
#    assays = as.matrix(countsData),
#    colData = metadata
#  )

## ----eval=FALSE---------------------------------------------------------------
#  # GSE5281
#  # Add a column specifying the condition of the sample, which can be either normal or alzheimer
#  affySampleInfo$condition <- ifelse(grepl("normal", affySampleInfo$characteristics_ch1.8), "normal", "alzheimer")
#  # Factorize the newly added column
#  affySampleInfo$condition <- factor(affySampleInfo$condition)
#  # Add the new column specify the region of the sample tissue
#  # and use make.names to remove special characters
#  affySampleInfo$region <- make.names(affySampleInfo$characteristics_ch1.4)
#  # Factorize the newly added column
#  affySampleInfo$region <- factor(affySampleInfo$region)
#  # Update the affyDataset object
#  SummarizedExperiment::colData(affyDataset) <- affySampleInfo

## ----eval=FALSE---------------------------------------------------------------
#  # GSE5281
#  # Create a design matrix
#  affyDesign <- model.matrix(~0 + condition + region + condition:region, data = SummarizedExperiment::colData(affyDataset))
#  # Avoid special characters in column names
#  colnames(affyDesign) <- make.names(colnames(affyDesign))
#  # Create a constrast matrix
#  affyContrast <- limma::makeContrasts(conditionalzheimer-conditionnormal, levels=affyDesign)

## ----eval=FALSE---------------------------------------------------------------
#  # Run differential expression analysis
#  affyDEExperiment <- RCPA::runDEAnalysis(affyDataset, method = "limma", design = affyDesign, contrast = affyContrast, annotation = "GPL570")

## ----eval=FALSE---------------------------------------------------------------
#  # Extract the differential analysis result
#  affyDEResults <- SummarizedExperiment::rowData(affyDEExperiment)

## ----eval=FALSE---------------------------------------------------------------
#  # GSE61196
#  colData(agilDataset)$condition <- ifelse(grepl("healthy", colData(agilDataset)$source_name_ch1), "normal", "alzheimer")
#  colData(agilDataset)$condition <- factor(colData(agilDataset)$condition)

## ----eval=FALSE---------------------------------------------------------------
#  # GSE61196
#  agilDesign <- model.matrix(~0 + condition, data = colData(agilDataset))
#  agilContrast <- limma::makeContrasts("conditionalzheimer-conditionnormal", levels=agilDesign)

## ----eval=FALSE---------------------------------------------------------------
#  # GSE61196
#  GPL4133Anno <- GEOquery::dataTable(GEOquery::getGEO("GPL4133"))@table
#  GPL4133GeneMapping <- data.frame(FROM = GPL4133Anno$SPOT_ID, TO = as.character(GPL4133Anno$GENE), stringsAsFactors = F)
#  GPL4133GeneMapping <- GPL4133GeneMapping[!is.na(GPL4133GeneMapping$TO), ]

## ----eval=FALSE---------------------------------------------------------------
#  # GSE61196
#  agilDEExperiment <- RCPA::runDEAnalysis(agilDataset, method = "limma", design = agilDesign, contrast = agilContrast, annotation = GPL4133GeneMapping)

## ----eval=FALSE---------------------------------------------------------------
#  # GSE153873
#  colData(RNASeqDataset)$condition <- ifelse(grepl("Alzheimer", colData(RNASeqDataset)$characteristics_ch1.1), "alzheimer", "normal")

## ----eval=FALSE---------------------------------------------------------------
#  # GSE61196
#  agilDesign <- model.matrix(~0 + condition, data = colData(agilDataset))
#  agilContrast <- limma::makeContrasts("conditionalzheimer-conditionnormal", levels=agilDesign)
#  
#  # GSE153873
#  RNASeqDesign <- model.matrix(~0 + condition, data = colData(RNASeqDataset))
#  RNASeqContrast <- limma::makeContrasts("conditionalzheimer-conditionnormal", levels=RNASeqDesign)

## ----eval=FALSE---------------------------------------------------------------
#  # GSE153873
#  if (!require("org.Hs.eg.db", quietly = TRUE)) {
#      BiocManager::install("org.Hs.eg.db")
#  }
#  
#  library(org.Hs.eg.db)
#  ENSEMBLMapping <- AnnotationDbi::select(org.Hs.eg.db, keys = rownames(RNASeqDataset), columns = c("SYMBOL", "ENTREZID"), keytype = "SYMBOL")
#  colnames(ENSEMBLMapping) <- c("FROM", "TO")

## ----eval=FALSE---------------------------------------------------------------
#  # GSE153873
#  RNASeqDEExperiment <- RCPA::runDEAnalysis(RNASeqDataset, method = "DESeq2", design = RNASeqDesign, contrast = RNASeqContrast, annotation = ENSEMBLMapping)

## ----eval=FALSE---------------------------------------------------------------
#  # GSE153873
#  RNASeqDEExperiment <- RCPA::runDEAnalysis(RNASeqDataset, method = "edgeR", design = RNASeqDesign, contrast = RNASeqContrast, annotation = ENSEMBLMapping)

## ----eval=FALSE---------------------------------------------------------------
#  # MA plots
#  p1 <- RCPA::plotMA(rowData(affyDEExperiment), logFCThreshold = 0.5) + ggplot2::ggtitle("Affymetrix - GSE5281")
#  p2 <- RCPA::plotMA(rowData(agilDEExperiment), logFCThreshold = 0.5) + ggplot2::ggtitle("Agilent - GSE61196")
#  p3 <- RCPA::plotMA(rowData(RNASeqDEExperiment), logFCThreshold = 0.5) + ggplot2::ggtitle("RNASeq - GSE153873")
#  
#  gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

## ----eval=FALSE---------------------------------------------------------------
#  # Volcano plots
#  p1 <- RCPA::plotVolcanoDE(rowData(affyDEExperiment), logFCThreshold = 0.5) + ggplot2::ggtitle("Affymetrix - GSE5281")
#  p2 <- RCPA::plotVolcanoDE(rowData(agilDEExperiment), logFCThreshold = 0.5) + ggplot2::ggtitle("Agilent - GSE61196")
#  p3 <- RCPA::plotVolcanoDE(rowData(RNASeqDEExperiment), logFCThreshold = 0.5) + ggplot2::ggtitle("RNASeq - GSE153873")
#  
#  gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

## ----eval=FALSE---------------------------------------------------------------
#  # All DE genes
#  DEResults <- list(
#  "Affymetrix - GSE5281" = rowData(affyDEExperiment),
#  "Agilent - GSE61196" = rowData(agilDEExperiment),
#  "RNASeq - GSE153873" = rowData(RNASeqDEExperiment)
#  )
#  # Up-regulated genes
#  DEResultUps <- lapply(DEResults, function(df) df[!is.na(df$logFC) & df$logFC > 0,])
#  # Down-regulated genes
#  DEResultDowns <- lapply(DEResults, function(df) df[!is.na(df$logFC) & df$logFC < 0,])
#  
#  p1 <- RCPA::plotVennDE(DEResults) + ggplot2::ggtitle("All DE Genes")
#  p2 <- RCPA::plotVennDE(DEResultUps) + ggplot2::ggtitle("Up-regulated DE Genes")
#  p3 <- RCPA::plotVennDE(DEResultDowns) + ggplot2::ggtitle("Down-regulated DE Genes")
#  
#  gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

## ----eval=FALSE---------------------------------------------------------------
#  genesets <- RCPA::getGeneSets(database = "KEGG", org = "hsa")

## ----eval=FALSE---------------------------------------------------------------
#  #The list of additional arguments passed to fgsea function
#  fgseaArgsList <- list(minSize = 10, maxSize = Inf)
#  
#  #Geneset enrichment analysis
#  affyFgseaResult <- runGeneSetAnalysis(affyDEExperiment, genesets,
#                                                  method = "fgsea", FgseaArgs = fgseaArgsList)
#  agilFgseaResult <- runGeneSetAnalysis(agilDEExperiment, genesets,
#                                                  method = "fgsea", FgseaArgs = fgseaArgsList)
#  RNASeqFgseaResult <- runGeneSetAnalysis(RNASeqDEExperiment, genesets,
#                                                    method = "fgsea", FgseaArgs = fgseaArgsList)

## ----eval=FALSE---------------------------------------------------------------
#  affyORAResult <- runGeneSetAnalysis(affyDEExperiment, genesets,
#                                                method = "ora", ORAArgs = list(pThreshold = 0.05))
#  

## ----eval=FALSE---------------------------------------------------------------
#  affyGSAResult <- runGeneSetAnalysis(affyDEExperiment, genesets, method = "gsa")
#  

## ----eval=FALSE---------------------------------------------------------------
#  affyKSResult <- runGeneSetAnalysis(affyDEExperiment, genesets, method = "ks")
#  

## ----eval=FALSE---------------------------------------------------------------
#  affyKSResult <- runGeneSetAnalysis(affyDEExperiment, genesets, method = "wilcox")
#  

## ----eval=FALSE---------------------------------------------------------------
#  spiaNetwork <- RCPA::getSPIAKEGGNetwork(org = "hsa", updateCache = FALSE)
#  

## ----eval=FALSE---------------------------------------------------------------
#  affySpiaResult <- runPathwayAnalysis(affyDEExperiment, spiaNetwork, method = "spia")
#  agilSpiaResult <- runPathwayAnalysis(agilDEExperiment, spiaNetwork, method = "spia")
#  RNASeqSpiaResult <- runPathwayAnalysis(RNASeqDEExperiment, spiaNetwork, method = "spia")
#  

## ----eval=FALSE---------------------------------------------------------------
#  cepaNetwork <- RCPA::getCePaPathwayCatalogue(org = "hsa", updateCache = FALSE)
#  

## ----eval=FALSE---------------------------------------------------------------
#  affyCepaORAResult <- runPathwayAnalysis(affyDEExperiment, cepaNetwork, method = "cepaORA")
#  

## ----eval=FALSE---------------------------------------------------------------
#  affyCepaGSAResult <- runPathwayAnalysis(affyDEExperiment, cepaNetwork, method = "cepaGSA")
#  

## ----eval=FALSE---------------------------------------------------------------
#  #Preparing the input list of DE results
#  DEResults <- list(
#  	"Affymetrix - GSE5281" = rowData(affyDEExperiment),
#  	"Agilent - GSE61196" = rowData(agilDEExperiment),
#  	"RNASeq - GSE153873" = rowData(RNASeqDEExperiment)
#  	)
#  
#  #Calling the runDEMetaAnalysis with 'stouffer' as the selected method to combine PValues
#  metaDEResult <- RCPA::runDEMetaAnalysis(DEResults, method = "stouffer")

## ----eval=FALSE---------------------------------------------------------------
#  	#Preparing the input list of enrichment results
#  	PAResults <- list(
#  	"Affymetrix - GSE5281" = affyFgseaResult,
#  	"Agilent - GSE61196" = agilFgseaResult,
#  	"RNASeq - GSE153873" = RNASeqFgseaResult
#  	)
#  
#  	#Calling the runPathwayMetaAnalysis with 'stouffer' as the selected method to combine PValues
#  	metaPAResult <- RCPA::runPathwayMetaAnalysis(PAResults, method = "stouffer")
#  

## ----eval=FALSE---------------------------------------------------------------
#  	PAResults <- list(
#  	"fgsea" = affyFgseaResult,
#  	"spia" = affySpiaResult
#  	)
#  
#  	consensusPAResult <- RCPA::runConsensusAnalysis(PAResults, method = "weightedZMean")
#  

## ----eval=FALSE---------------------------------------------------------------
#  PAResults <- list(
#    	"Affymetrix - GSE5281" = affyFgseaResult,
#    	"Agilent - GSE61196" = agilFgseaResult,
#    	"RNASeq - GSE153873" = RNASeqFgseaResult,
#    	"Meta-analysis" = metaPAResult
#  	)
#  
#  	PAREsultUps <- lapply(PAResults, function(df) df[df$normalizedScore > 0,])
#  	PAREsultDowns <- lapply(PAResults, function(df) df[df$normalizedScore < 0,])
#  
#  	p1 <- RCPA::plotVennPathway(PAResults, pThreshold = 0.05) + ggplot2::ggtitle("All Significant Pathways")
#  	p2 <- RCPA::plotVennPathway(PAREsultUps, pThreshold = 0.05) + ggplot2::ggtitle("Significantly Up-regulated Pathways")
#  	p3 <- RCPA::plotVennPathway(PAREsultDowns, pThreshold = 0.05) + ggplot2::ggtitle("Significantly Down-regulated Pathways")
#  
#  	gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

## ----eval=FALSE---------------------------------------------------------------
#    p1 <- RCPA::plotVolcanoPathway(affyFgseaResult, sideToLabel = "left") + ggtitle("Affymetrix - GSE5281")
#  	p2 <- RCPA::plotVolcanoPathway(agilFgseaResult, sideToLabel = "left") + ggtitle("Agilent - GSE61196")
#  	p3 <- RCPA::plotVolcanoPathway(RNASeqFgseaResult, sideToLabel = "left") + ggtitle("RNASeq - GSE153873")
#  	p4 <- RCPA::plotVolcanoPathway(metaPAResult, sideToLabel = "left") + ggtitle("Meta-analysis")
#  
#  	gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 4)

## ----eval=FALSE---------------------------------------------------------------
#  selectedPathways <- c("path:hsa05010", "path:hsa05012", "path:hsa05014", "path:hsa05016", "path:hsa05017", "path:hsa05020", "path:hsa05022", "path:hsa04724", "path:hsa04727", "path:hsa04725", "path:hsa04728", "path:hsa04726", "path:hsa04720", "path:hsa04730", "path:hsa04723", "path:hsa04721", "path:hsa04722")
#  
#  	resultsToPlot <- lapply(PAResults, function(df) df[df$ID %in% selectedPathways,])
#  
#  	RCPA::plotBarChart(resultsToPlot) + ggplot2::ggtitle("FGSEA Analysis Results")

## ----eval=FALSE---------------------------------------------------------------
#  RCPA::plotForest(resultsToPlot, yAxis = "name", statLims = c(-3.5, 3.5))

## ----eval=FALSE---------------------------------------------------------------
#  RCPA::plotPathwayHeatmap(resultsToPlot, yAxis = "name")

## ----eval=FALSE---------------------------------------------------------------
#  alzheimerGenes <- genesets$genesets[["path:hsa05010"]]
#  genesToPlot <- head(metaDEResult[metaDEResult$ID %in% alzheimerGenes, ], 50)$ID
#  
#  genesAnnotation <- RCPA::getEntrezAnnotation(genesToPlot)
#  labels <- genesAnnotation[genesToPlot, "Description"]
#  
#  genesOrderByFC <- order(metaDEResult[match(genesToPlot, metaDEResult$ID), "logFC"])
#  resultsToPlot <- c(DEResults, list(metaDEResult))
#  names(resultsToPlot) <- c(names(DEResults), "Meta-analysis")
#  
#  RCPA::plotDEGeneHeatmap(resultsToPlot, genesToPlot[genesOrderByFC],
#  	labels = labels[genesOrderByFC], negLog10pValueLims = c(0, 5), logFCLims = c(-1, 1)
#  	)

## ----eval=FALSE---------------------------------------------------------------
#  pltObj <- RCPA::plotKEGGMap(DEResults, "hsa05010", stat = "logFC", pThreshold = 1, statLimit = 1)
#  pltObj$plot

## ----eval=FALSE---------------------------------------------------------------
#  genesetsToPlot <- metaPAResult$ID[order(metaPAResult$pFDR)][1:30]
#  
#  pltHtml <- RCPA::plotPathwayNetwork(
#  	PAResults,
#  	genesets = genesets,
#  	selectedPathways = genesetsToPlot,
#  	edgeThreshold = 0.75,
#  	mode = "continuous",
#  	statistic = "normalizedScore"
#  	)

