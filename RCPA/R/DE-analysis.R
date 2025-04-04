#' @title Differential expression analysis
#' @description Functions to perform differential expression analysis
#' These functions are used internally by runDEAnalysis.
#' @param exprs Normalized expression matrix. Rows are genes and columns are samples.
#' @param design A design model output by model.matrix.
#' @param contrast A contrast matrix output by makeContrasts from limma package.
#' @return A data frame with DE analysis results.
#' Must contain the following columns: ID, p.value, logFC, statistic, avgExpr, and logFCSE.
#' @importFrom limma lmFit contrasts.fit eBayes topTable makeContrasts
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom edgeR DGEList calcNormFactors estimateDisp glmQLFit glmQLFTest topTags
#' @importFrom stats model.matrix
#' @importFrom dplyr %>% mutate
#' @name runDEInternal
#' @keywords internal
#' @noRd
.runLimma <- function(exprs, design, contrast) {
    DERes <- exprs %>%
        lmFit(design) %>%
        contrasts.fit(contrast) %>%
        eBayes()

    resTable <- DERes %>%
        topTable(coef = 1, number = nrow(exprs), confint=TRUE) %>%
        mutate(ID = rownames(.),
               p.value = .$P.Value,
               statistic = .$t,
               avgExpr = .$AveExpr,
               logFCSE = (.$CI.R - .$logFC)/qnorm(0.975)
        )

    resTable[rownames(exprs), c("ID", "p.value", "statistic", "logFC", "avgExpr", "logFCSE")]
}

#' @rdname runDEInternal
#' @noRd
.runDESeq2 <- function(exprs, design, contrast) {

    DERes <- DESeqDataSetFromMatrix(
        countData = exprs,
        colData = data.frame(row.names = colnames(exprs), dummy.var = rep(1, ncol(exprs))),
        design = design
    ) %>%
        DESeq() %>%
        results(contrast)

    resTable <- DERes %>%
        as.data.frame() %>%
        mutate(ID = rownames(.), statistic = .$stat, p.value = .$pvalue, logFC = .$log2FoldChange, avgExpr = log2(.$baseMean + 1), logFCSE = .$lfcSE)

    resTable$p.value[is.na(resTable$p.value)] <- 1
    # resTable$dispersion <- resTable$lfcSE
    resTable[rownames(exprs), c("ID", "p.value", "statistic", "logFC", "avgExpr", "logFCSE")]
}

#' @rdname runDEInternal
#' @noRd
.runEdgeR <- function(exprs, design, contrast) {

    dispRes <- DGEList(counts = exprs) %>%
        calcNormFactors() %>%
        estimateDisp(design = design)

    DERes <- dispRes %>%
        glmQLFit(design = design, contrast = contrast) %>%
        glmQLFTest(contrast = contrast)

    resTable <- DERes$table %>%
        mutate(ID = rownames(.), p.value = .$PValue, statistic = .$logFC, logFC = .$logFC, avgExpr = .$logCPM, logFCSE = sqrt(dispRes$tagwise.dispersion))

    resTable$p.value[is.na(resTable$p.value)] <- 1
    # resTable$dispersion <- DERes$dispersion
    resTable[rownames(exprs), c("ID", "p.value", "statistic", "logFC", "avgExpr", "logFCSE")]
}

#' @title Differential expression analysis
#' @description This function performs differential expression analysis using either limma, DESeq2 or edgeR.
#' @param summarizedExperiment SummarizedExperiment object
#' @param method Method to use for differential expression analysis. Can be "limma", "DESeq2" or "edgeR".
#' @param design A design model output by model.matrix.
#' @param contrast A contrast matrix. See limma::makeContrasts.
#' @param annotation A data frame mapping between probe IDs and entrez gene IDs.
#' If not provided, the function will try to get the mapping from the platform annotation in the SummarizedExperiment object.
#' If the annotation is not available, the function will return the probe IDs.
#' Regardless of the type of annotation, it must contains two columns: FROM and TO,
#' where FROM is the probe ID and TO is the entrez gene ID.
#' @return A SummarizedExperiment object with DE analysis results appended to the rowData slot with the following columns:
#' \itemize{
#' \item{ID: gene ID. If annotation is provided, this will be the entrez gene ID. Otherwise, it will be the probe ID.}
#' \item{logFC: log2 fold change}
#' \item{p.value: p-value from the DE analysis using the specified method}
#' \item{pFDR: p-value adjusted for multiple testing using Benjamini-Hochberg method}
#' \item{statistic: statistic from the DE analysis using the specified method.
#' For limma, this is the t-statistic.
#' For DESeq2, this is the Wald statistic.
#' For edgeR, this is the log fold change.}
#' \item{avgExpr: 
#' For limma, it is the average expression.
#' For DESeq2, it is the log base mean.
#' For edgeR, it is the log CPM.
#' }
#' \item{logFCSE: standard error of the log fold change.}
#' \item{sampleSize: sample size used for DE analysis.}
#' }
#' The assay slot will contain the input expression/count matrix,
#' and the rownames will be mapped to the gene IDs if annotation is found in the input SummarizedExperiment object
#' or in the annotation parameter.
#' Other slots will be the same as in the input SummarizedExperiment object.
#' @examples
#' \donttest{
#' library(RCPA)
#' library(SummarizedExperiment)
#'
#' # GSE5281
#' affyDataset <- loadData("affyDataset")
#' affyDesign <- model.matrix(~0 + condition + region + condition:region, 
#'                              data = colData(affyDataset))
#' colnames(affyDesign) <- make.names(colnames(affyDesign))
#' affyContrast <- limma::makeContrasts(conditionalzheimer-conditionnormal, 
#'                                      levels=affyDesign)
#'
#' if (require("hgu133plus2.db", quietly = TRUE)){
#' affyDEExperiment <- RCPA::runDEAnalysis(affyDataset, method = "limma", 
#'                                         design = affyDesign,
#'                                         contrast = affyContrast, 
#'                                         annotation = "GPL570")
#'                                         
#' # check the DE analysis results
#' 
#' print(head(rowData(affyDEExperiment)))
#' }
#' 
#'
#'
#' # GSE61196
#' agilDataset <- loadData("agilDataset")
#' agilDesign <- model.matrix(~0 + condition, 
#'                             data = colData(agilDataset))
#' agilContrast <- limma::makeContrasts(conditionalzheimer-conditionnormal, 
#'                                      levels=agilDesign)
#'
#' # Create Probe mapping
#' options(timeout = 3600)
#' GPL4133Anno <- GEOquery::dataTable(GEOquery::getGEO("GPL4133"))@table
#' GPL4133GeneMapping <- data.frame(FROM = GPL4133Anno$SPOT_ID, 
#'                                  TO = as.character(GPL4133Anno$GENE),
#'                                  stringsAsFactors = FALSE)
#' GPL4133GeneMapping <- GPL4133GeneMapping[!is.na(GPL4133GeneMapping$TO), ]
#'
#' agilDEExperiment <- RCPA::runDEAnalysis(agilDataset, method = "limma", 
#'                                         design = agilDesign,
#'                                         contrast = agilContrast, 
#'                                         annotation = GPL4133GeneMapping)
#' print(head(rowData(agilDEExperiment)))
#'
#' # GSE153873
#' RNASeqDataset <- loadData("RNASeqDataset")
#' RNASeqDesign <- model.matrix(~0 + condition, data = colData(RNASeqDataset))
#' RNASeqContrast <- limma::makeContrasts(conditionalzheimer-conditionnormal, 
#'                                       levels=RNASeqDesign)
#'
#'
#' if (require("org.Hs.eg.db", quietly = TRUE)){
#'     GeneSymbolMapping <- AnnotationDbi::select(org.Hs.eg.db, 
#'                                             keys = rownames(RNASeqDataset),
#'                                             columns = c("SYMBOL", "ENTREZID"), 
#'                                             keytype = "SYMBOL")
#'     colnames(GeneSymbolMapping) <- c("FROM", "TO")
#'
#'     RNASeqDEExperiment <- RCPA::runDEAnalysis(RNASeqDataset,
#'                            method = "DESeq2",
#'                            design = RNASeqDesign,
#'                            contrast = RNASeqContrast,
#'                            annotation = GeneSymbolMapping)
#'     print(head(rowData(RNASeqDEExperiment)))
#' }
#' }
#' @importFrom SummarizedExperiment SummarizedExperiment rowData assay colData
#' @importFrom dplyr %>%
#' @importFrom tidyr drop_na
#' @importFrom stats p.adjust
#' @export
runDEAnalysis <- function(summarizedExperiment, method = c("limma", "DESeq2", "edgeR"), design, contrast, annotation = NULL) {
  
    method <- match.arg(method)

    if (is.null(design)) {
        stop("Design matrix must be provided")
    }

    if (is.null(contrast)) {
        stop("Contrast matrix must be provided")
    }

    if (!.requirePackage("S4Vectors")){
        return(NULL)
    }

    # get ID mapping annotation
    if (is.null(annotation)) {
        if (is.null(S4Vectors::metadata(summarizedExperiment)$platform)) {
            stop("Platform annotation is not found in the meta data of the SummarizedExperiment object. Please provide an annotation data frame instead.")
        }
        annotation <- .getIDMappingAnnotation(platform = S4Vectors::metadata(summarizedExperiment)$platform)
    }

    if ("character" %in% class(annotation)) {
        annotation <- .getIDMappingAnnotation(platform = annotation)
    }

    if ("data.frame" %in% class(annotation)) {
        if (!all(c("FROM", "TO") %in% colnames(annotation))) {
            stop("Annotation data frame must have columns FROM and TO")
        }
    } else {
        stop("Annotation must be a data frame or a string")
    }

    if (is.null(annotation) & pkgEnv$isMissingDependency){
        return(NULL)
    }
    
    if (!is.null(annotation) & "data.frame" %in% class(annotation)) {
      annotation <- annotation %>% drop_na()
    }

    # get expression matrix
    exprs <- assay(summarizedExperiment)

    if (!is.null(annotation)){
        exprs <- exprs[intersect(unique(annotation$FROM), rownames(exprs)),]
    }

    DEFunc <- switch(method,
                     limma = .runLimma,
                     DESeq2 = .runDESeq2,
                     edgeR = .runEdgeR
    )

    # run DE analysis
    DEResult <- DEFunc(exprs, design, contrast)
    DEResult$sampleSize <- nrow(design)

    # map probe IDs to gene symbols
    mappedResults <- .mapIDs(exprs, annotation, DEResult)
    mappedResults$DEResult$pFDR <- p.adjust(mappedResults$DEResult$p.value, method = "BH")

    # create a new SummarizedExperiment object
    newSummarizedExperiment <- SummarizedExperiment(
        assays = S4Vectors::SimpleList(counts = mappedResults$exprs),
        rowData = data.frame(
            cbind(
                rowData(summarizedExperiment)[mappedResults$mapping$ID,],
                PROBEID = mappedResults$mapping$ID,
                mappedResults$DEResult
            ),
            row.names = mappedResults$mapping$TO
        ),
        colData = colData(summarizedExperiment),
        metadata = c(
            S4Vectors::metadata(summarizedExperiment),
            DEAnalysis = list(
                method = method,
                design = design,
                contrast = contrast,
                mapping = mappedResults$mapping
            )
        )
    )

    newSummarizedExperiment
}


