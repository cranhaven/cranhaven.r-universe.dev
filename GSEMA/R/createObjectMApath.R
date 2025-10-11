#' Creation of the object to use in gene set enrichment meta-analysis
#'
#' It allows the creation of an object to perform gene set
#' enrichment meta-analysis.
#'
#' @param listEX A list of dataframes or matrix (genes in rows and sample
#' in columns). A list of ExpressionSets can be used too
#'
#' @param listPheno A list of phenodatas (dataframes or matrix). If the object
#' listEX is a list of ExpressionSets this element can be null.
#'
#' @param namePheno A list or vector of the different colunm names or
#' positions from the phenodatas where the experimental and reference groups
#' are identified. Each element of namePheno correspont to its equivalent
#' element in  the listPheno (default a vector of 1, all the first columns of
#' each elements of listPheno are selected).
#'
#' @param expGroups A list of vectors or a vector containing the names or the
#' positions with which we identify the elements of the experiment groups
#' (cases) of the namePheno element (default a vector  of 1, all the first
#' groups are selected)
#'
#' @param refGroups A list of vectors or a vector containing the names or the
#' positions with which we identify the elements of the reference groups
#' (control) of the namePheno elements (default a vector  of 1, all the first
#' groups are selected)
#'
#' @param geneSets 	List of gene sets to check. Object similar
#' to the one used in the fgsea package
#'
#' @param pathMethod The single sample enrichment method used to obtain
#' the enrichment score of each sample and gene set.
#' See details for more information
#'
#' @param minSize Minimum size of the resulting gene sets after gene
#' identifier mapping. By default, the minimum size is 7.
#'
#' @param kcdf Only neccesary for the GSVA method. Character vector of length 1
#' denoting the kernel to use during the non-parametric estimation of the
#' cumulative distribution function of expression levels across samples.
#' By default, kcdf="Gaussian" which is suitable when input expression values
#' are continuous, such as microarray fluorescent units in logarithmic scale,
#' RNA-seq log-CPMs, log-RPKMs or log-TPMs. When input expression values are
#' integer counts, such as those derived from RNA-seq experiments, then this
#' argument should be set to kcdf="Poisson".
#'
#' @param normalize boolean specifying if the gen set matrices should be
#' normalized. Default value "TRUE".
#'
#' @param n.cores Number of cores to use in the parallelization of the datsets.
#' By default, n.cores=1.
#'
#' @param internal.n.cores Number of cores to use in the parallelization of the
#' single sample enrichment methods. By default internal.n.cores= 1.
#'
#' @details The single sample scoring methods that can be used to obtain the
#' enrichment score of each sample and gene set are:
#'\enumerate{
#'  \item "GSVA": Gene Set Variation method (Hänzelmann S, 2013)
#'  \item "Zscore": Z-score method (Lee E, 2008)
#'  \item "ssGSEA": Single Sample Gene Set Enrichment Analysis method
#'  (Barbie DA, 2009)
#'  \item "Singscore": Single sample scoring of molecular phenotypes
#'  (Foroutan M, 2018)
#'     }
#'
#'  In parallelization, several aspects must be considered.
#'  n.cores refers to the parallelization of studies or datasets.
#'  Therefore, if we have 3 studies, the maximum number for n.cores will be 3.
#'  internal.n.cores refers to the parallelization of single sample enrichment
#'  methods. This is especially recommended for the ssGSEA method. For Singscore
#'  and GSVA, it may also be advisable. The process is parallelized based on the
#'  samples in each study. Therefore, the larger the number of samples, the
#'  slower the process will be.
#'  The number of cores that the computer will use is the multiplication of both
#'  parameters n.cores * internal.n.cores = total cores.
#'
#'
#' @return The object needed to perform gene set enrichment meta-analysis.
#' Each list contains  two elements: The first element is the gene set matrix
#' (gene sets in rows
#' and samples in columns) The second element is a vector of zeros and ones
#' that represents the state of the diffenrent samples of the gene sets
#' matrix. 0 represents reference group (controls) and 1 represents
#' experimental group (cases).
#'
#' @references
#'
#' Hänzelmann S, Castelo R, Guinney J. (2013)
#' GSVA: gene set variation analysis for microarray and RNA-Seq data.
#' BMC Bioinformatics. 2013;14: 7. doi:10.1186/1471-2105-14-7
#'
#' Lee E, Chuang H-Y, Kim J-W, Ideker T, Lee D. (2008)
#' Inferring Pathway Activity toward Precise Disease Classification.
#' PLOS Computational Biology. 2008;4: e1000217.
#' doi:10.1371/journal.pcbi.1000217
#'
#' Barbie DA, Tamayo P, Boehm JS, Kim SY, Moody SE, Dunn IF, et al. (2009)
#' Systematic RNA interference reveals that oncogenic KRAS-driven cancers
#' require TBK1. Nature. 2009;462: 108–112. doi:10.1038/nature08460
#'
#' Foroutan M, Bhuva DD, Lyu R, Horan K, Cursons J, Davis MJ. (2018)
#' Single sample scoring of molecular phenotypes. BMC Bioinformatics.
#' 2018;19: 404. doi:10.1186/s12859-018-2435-4
#'
#' Korotkevich G, Sukhov V, Budin N, Shpak B, Artyomov MN,
#' Sergushichev A. (2021)
#' Fast gene set enrichment analysis. bioRxiv; 2021. p. 060012.
#' doi:10.1101/060012
#'
#'
#' @examples
#'
#' data("simulatedData")
#' listMatrices <- list(study1Ex, study2Ex)
#' listPhenodata <- list(study1Pheno, study2Pheno)
#' phenoGroups <- c("Condition","Condition")
#' phenoCases <- list("Case", "Case")
#' phenoControls <- list("Healthy", "Healthy")
#' objectMApathSim <- createObjectMApath(
#'    listEX = listMatrices,
#'    listPheno = listPhenodata, namePheno = phenoGroups,
#'    expGroups = phenoCases, refGroups = phenoControls,
#'    geneSets = GeneSets,
#'    pathMethod = "Zscore")
#'
#'
#'


createObjectMApath <- function(listEX, listPheno = NULL,
    namePheno =  c(rep(1, length(listEX))),
    expGroups= c(rep(1, length(listEX))),
    refGroups = c(rep(2, length(listEX))),
    geneSets,
    pathMethod = c("GSVA", "Zscore", "ssGSEA", "Singscore"), minSize = 7,
    kcdf = "Gaussian", normalize = TRUE,
    n.cores = 1, internal.n.cores = 1){
    pathMethod <- match.arg(pathMethod)
    objectExMA <- .createObjectMA(listEX, listPheno, namePheno, expGroups,
        refGroups)
    objectExMApath <- .objectExMA.to.objectMApath(objectExMA, geneSets,
        pathMethod, minSize, kcdf, normalize = normalize,
        n.cores, internal.n.cores)
    return(objectExMApath)
}


#Create objectMa similar to DExMA
.createObjectMA <- function(listEX, listPheno = NULL,
    namePheno =  c(rep(1, length(listEX))),
    expGroups= c(rep(1, length(listEX))),
    refGroups = c(rep(2, length(listEX)))){
    #check input objects
    if(!is.list(listEX)){
        stop("listEX  must be a list")
    }
    if(length(listEX)<2){stop("There must be at least 2 studies")}
    if (!any(is.list(listPheno) | is.null(listPheno))){
        stop("listPheno must be a List or NULL")
    }
    if(!any(is.list(namePheno) | is.numeric(namePheno) |
            is.character(namePheno))){
        stop("namePheno  must be a list or numeric or character")
    }
    if(!any(is.list(expGroups) | is.numeric(expGroups) |
            is.character(expGroups))){
        stop("expGroups  must be a list or numeric or character")
    }
    if(!any(is.list(refGroups) | is.numeric(refGroups) |
            is.character(refGroups))){
        stop("refGroups  must be a list or numeric or character")
    }
    #Obtaining the object
    container <- list(0)
    for (i in seq_len(length(listEX))) {
        container[[i]] <- .elementObjectMA(
            expressionMatrix=as.matrix(listEX[[i]]),
            pheno=listPheno[[i]],
            groupPheno=namePheno[[i]],
            expGroup=expGroups[[i]],
            refGroup=refGroups[[i]])
    }
    #Names of the results
    if(is.null(names(listEX))){
        names(container) <- paste("Study",seq_len(length(listEX)),sep="")}
    else{
        if(length(names(listEX)) >
                length(names(listEX)[is.na(names(listEX))==FALSE])){
            names(container) <- paste("Study",seq_len(length(listEX)),sep="")}
        else{names(container) <- names(listEX)}}
    return(container)
}


#Element objectMA similar to DExMA
.elementObjectMA <- function(expressionMatrix, pheno=NULL, groupPheno,
    expGroup=1, refGroup=2){
    if (!any(is.data.frame(expressionMatrix) | is.matrix(expressionMatrix) |
            is(expressionMatrix,"ExpressionSet"))){
        stop("Elements of listEX must be a  dataframe, matrix or
            ExpressionSet object")
    }
    if (!any(is.data.frame(pheno) | is.matrix(pheno) | is.null(pheno))){
        stop("Elements of listPheno must be a dataframe, a matrix or NULL")}
    if (!any(is.character(groupPheno) |is.numeric(groupPheno))){
        stop("groupPheno must be a character or numeric object")}
    if (!any(is.character(expGroup) | is.numeric(expGroup))){
        stop("expGroup must be a character or numeric object")}
    if (!any(is.character(refGroup) | is.numeric(refGroup))){
        stop("refGroup must be a character or numeric object")}
    ## Get data from expression set
    if(is(expressionMatrix, "ExpressionSet")){
        if (is.null(pheno)){
            pheno <- pData(expressionMatrix)}
        expressionMatrix <- exprs(expressionMatrix)
    }
    else{
        if(is.null(pheno)) {
            stop("If DATA is not a ExpressionSet, you must provide
                pheno parameter")}
    }
    colnames(expressionMatrix) <- as.character(colnames(expressionMatrix))
    rownames(pheno) <- as.character(rownames(pheno))
    pheno <- pheno[colnames(expressionMatrix),,drop=FALSE]
    if (is.numeric(groupPheno)){
        groupPheno <- colnames(pheno)[groupPheno]}
    if (is.numeric(expGroup)){
        pheno[,groupPheno] <- factor(pheno[,groupPheno])
        expGroup <- levels(pheno[,groupPheno])[expGroup]}
    if (is.numeric(refGroup)){
        pheno[,groupPheno] <- factor(pheno[,groupPheno])
        refGroup <- levels(pheno[,groupPheno])[refGroup]}
    Inclusion <- rownames(pheno)[pheno[,groupPheno] %in%
            c(expGroup, refGroup)]
    expressionMatrix <- expressionMatrix[,Inclusion]
    pheno <- data.frame(pheno[Inclusion, groupPheno, drop=FALSE])
    colnames(pheno) <- groupPheno
    pheno <- droplevels(pheno)
    ## Convert pheno to 0 and 1 for use the rest of functions
    store<-rep(0, length(pheno))
    for (k in seq_len(length(pheno[,groupPheno]))) {
        if(pheno[,groupPheno][k] %in% expGroup){store[k]=1}
        if(pheno[,groupPheno][k] %in% refGroup){store[k]=0}}
    metalObject <- list(mExpres=expressionMatrix, condition=store)
    return(metalObject)
}

#Create objectMAEXpath
.objectExMA.to.objectMApath <- function(objectExMA, geneSets,
    pathMethod = c("GSVA", "Zscore", "ssGSEA", "Singscore"), minSize = 7,
    kcdf = "Gaussian", n.cores = 1, internal.n.cores = 1, normalize = TRUE){
    pathMethod <- match.arg(pathMethod)
    if(Sys.info()["sysname"] == "Windows"){
        param <- SnowParam(workers = n.cores)
    }
    else{
        param <- MulticoreParam(workers = n.cores)
    }
    #GSVA method
    if(pathMethod == "GSVA"){
        message("Applying the GSVA method")
        objectMApath <- bplapply(objectExMA, function(x){
            x[[1]] <- .applyGSVA(x[[1]], geneSets = geneSets,
                minSize = minSize, kcdf = kcdf, normalize = normalize,
                internal.n.cores = internal.n.cores)
            x[[2]] <- x[[2]]
            names(x) <- c("mPath", "condition")
            return(x)},
            BPPARAM = param
        )
        names(objectMApath) <- names(objectExMA)
    }
    #ssGSEA method
    if(pathMethod == "ssGSEA"){
        message("Applying the ssGSEA method")
        objectMApath <- bplapply(objectExMA, function(x){
            x[[1]] <- .applyssGSEA(x[[1]], geneSets = geneSets,
                minSize = minSize, normalize = normalize,
                internal.n.cores = internal.n.cores)
            x[[2]] <- x[[2]]
            names(x) <- c("mPath", "condition")
            return(x)},
            BPPARAM = param
        )
        names(objectMApath) <- names(objectExMA)
    }
    #Z-score method
    if(pathMethod == "Zscore"){
        message("Applying the Zscore method")
        objectMApath <- bplapply(objectExMA, function(x) {
            x[[1]] <- .applyZscore(x[[1]], geneSets = geneSets,
                minSize = minSize,
                internal.n.cores = internal.n.cores)
            x[[2]] <- x[[2]]
            names(x) <- c("mPath", "condition")
            return(x)},
            BPPARAM = param
        )
        names(objectMApath) <- names(objectExMA)
    }
    #singscore method
    if(pathMethod == "Singscore"){
        message("Applying the singscore method")
        objectMApath <- bplapply(objectExMA, function(x) {
            x[[1]] <- .applySingscore(x[[1]], geneSets = geneSets,
                normalize = normalize, minSize = minSize)
            x[[2]] <- x[[2]]
            names(x) <- c("mPath", "condition")
            return(x)},
            BPPARAM = param
        )
        names(objectMApath) <- names(objectExMA)
    }
    return(objectMApath)
}



#GSVA
.applyGSVA <- function(exMatrix, geneSets, minSize = 7, kcdf = "Gaussian",
    internal.n.cores = 1, normalize = TRUE){
    if(Sys.info()["sysname"] == "Windows"){
        internalparam <- SnowParam(workers = internal.n.cores)
    }
    else{
        internalparam <- MulticoreParam(workers = internal.n.cores)
    }
    paramMatrix <- gsvaParam(exMatrix, geneSets, minSize = minSize, kcdf = kcdf)
    gsvaMatrix <- gsva(paramMatrix,
        BPPARAM = internalparam, verbose = FALSE)
    #gsvaMatrix normalization
    if(normalize == TRUE){
        gsvaMatrix <- (gsvaMatrix - mean(gsvaMatrix)) / sd(gsvaMatrix)
    }
    return(gsvaMatrix)
}

#Zscore
.applyZscore <- function(exMatrix, geneSets, minSize = 7, internal.n.cores = 1){
    if(Sys.info()["sysname"] == "Windows"){
        internalparam <- SnowParam(workers = internal.n.cores)
    }
    else{
        internalparam <- MulticoreParam(workers = internal.n.cores)
    }
    paramMatrix <- zscoreParam(exMatrix, geneSets, minSize =  minSize)
    ZscoreMatrix <- gsva(paramMatrix, BPPARAM = internalparam, verbose = FALSE)
    return(ZscoreMatrix)
}

#ssGSEA
.applyssGSEA <- function(exMatrix, geneSets, minSize = 7, normalize = TRUE,
    internal.n.cores = 1){
    if(Sys.info()["sysname"] == "Windows"){
        internalparam <- SnowParam(workers = internal.n.cores)
    }
    else{
        internalparam <- MulticoreParam(workers = internal.n.cores)
    }
    paramMatrix <- ssgseaParam(exMatrix, geneSets, minSize =  minSize)
    ssGSEAMatrix <- gsva(paramMatrix, BPPARAM = internalparam, verbose = FALSE)
    #ssGSEAMatrix normalization
    if(normalize == TRUE){
        ssGSEAMatrix <- (ssGSEAMatrix - mean(ssGSEAMatrix)) / sd(ssGSEAMatrix)
    }
    return(ssGSEAMatrix)
}

#Singscore
.applySingscore <- function(exMatrix, geneSets, minSize = 7, normalize = TRUE,
    internal.n.cores = 1){
    exMatrixgenes <- rownames(exMatrix)
    pathways_filtered <- lapply(geneSets, function(pathway) {
        genes_com <- intersect(pathway, exMatrixgenes)
        if (length(genes_com) >= minSize) {
            return(pathway)
        } else {
            return(NULL)
        }
    })
    pathways_filtered <- pathways_filtered[!sapply(pathways_filtered, is.null)]
    geneSets <- pathways_filtered
    rankMatrix <- rankGenes(exMatrix, tiesMethod = "average")
    message("Estimating singscore values")
    listSign <- suppressWarnings(lapply(geneSets,
            function(x) simpleScore(rankData = rankMatrix, upSet = x)))
    listScores <- sapply(listSign, function(x) x$TotalScore)
    if(is.list(listScores)){
        pathMatrix <- do.call(rbind, listScores)}
    else{
        pathMatrix <- t(listScores)
    }
    colnames(pathMatrix) <- colnames(exMatrix)
    #pathMatrix normalization
    if(normalize == TRUE){
        pathMatrix <- (pathMatrix - mean(pathMatrix)) / sd(pathMatrix)
    }
    return(pathMatrix)
}
