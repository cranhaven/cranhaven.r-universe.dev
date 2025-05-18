
#function: pool all cells together
poolcells<-function (eset,sample)
{
    eset@phenoData@data[[sample]]<-"one"
    eset
}

#'@title Pseudo bulk data generation function
#'@description This function generates a pseudo bulk samples by random sampled number of cells per subject.
#'@usage generateBulk(eset,ct.varname,sample,disease = NULL,ct.sub,prop_mat = NULL,
#'nbulk = 50,samplewithRep = FALSE,low_s = 0.3,upp_s = 0.7)
#'@param eset The `ExpressionSet` object for single cells.
#'@param ct.varname Variable name for 'cell types'.
#'@param sample Variable name for subject/samples.
#'@param disease Indicate the health condition of subjects.
#'@param ct.sub A subset of cell types that are selected to construct pseudo bulk samples. If NULL, then all cell types are used.
#'@param prop_mat Manually input the cell-type proportion for pseudo bulk samples.
#'@param nbulk The number of pseudo bulk samples to be constructed.
#'@param samplewithRep Logical, randomly sample single cells with replacement. Default is F.
#'@param low_s Lower support a for uniform distribution U[a,b].
#'@param upp_s Upper support b for uniform distribution U[a,b].
#'
#'@return Pseudo bulk samples in the format of `ExpressionSet`,
#'and the true cell-type proportions.
#'
#'@examples
#'##read data
#'library(InteRD)
#'readRDSFromWeb<-function(ref) {readRDS(gzcon(url(ref)))}
#'urlremote<-"https://github.com/chencxxy28/Data/raw/main/data_InteRD/"
#'seger<-readRDSFromWeb(paste0(urlremote,"segerstolpe.rds"))
#'
#'##generate a pseudo bulk data with two samples
#'set.seed(1234567)
#'pseudo.seger<-generateBulk(seger[["sc.eset.qc"]], ct.varname = "cluster",
#'sample = "sample", ct.sub = c("alpha","beta","delta","gamma"),
#'nbulk = 2, low_s = 0.3, upp_s = 0.7)
#'
#'@export
#'@import cowplot pheatmap pheatmap stats mgcv reshape2 Rcpp
#'@importFrom limSolve nnls
#'@importFrom DescTools CCC
#'@importFrom Biobase exprs ExpressionSet AnnotatedDataFrame
#generate pseudo bulk data
generateBulk <- function(eset, ct.varname, sample, disease = NULL, ct.sub,
                               prop_mat = NULL, nbulk=50, samplewithRep = FALSE, low_s = 0.3, upp_s = 0.7){
    #pool cells together
    eset <- poolcells(eset,sample)

    x.sub <- eset[,eset@phenoData@data[,ct.varname] %in% ct.sub]
    # qc: remove non-zero genes
    x.sub <- x.sub[rowSums(exprs(x.sub)) > 0,]
    # calculate sample mean & sample variance matrix: genes by cell types
    ct.id <- droplevels(as.factor(x.sub@phenoData@data[,ct.varname]))
    sample.id <- x.sub@phenoData@data[,sample]

    pdatab <- x.sub@phenoData@data
    if (! is.null(disease)){
        disease <- x.sub@phenoData@data[,disease]
        names(disease) <- sample.id
    }

    # number of cell type of interest
    k <- length(unique(ct.id))
    message(paste('Using',k,'cell types to generate pseudo bulk samples...'))
    # select donors for each pseudo bulk sample
    pseudo_donors <- sample(sample.id, nbulk, replace = TRUE)
    names(pseudo_donors) <- paste("bulk",1:nbulk, sep = "_")

    # generate random matrix for true proportions
    if (!is.null(prop_mat)){
        true.p1 <- prop_mat # manually input proportion matrix...
        colnames(true.p1) <- unique(ct.id)[order(unique(ct.id))]
        rownames(true.p1) <- names(pseudo_donors)
        message("Using input proportion matrix to create pseudo bulk samples...")
        true.ct <- matrix(data = 0,ncol = k, nrow = nbulk) # true number of cells per cell type for each sample
        colnames(true.ct) <- unique(ct.id)[order(unique(ct.id))]
        rownames(true.ct) <- paste(pseudo_donors,1:nbulk,sep = "_")
        # make sure if without replacement, number of cells matches the input prop mat...
        # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    } else {
        true.p1 <- matrix(data = 0,ncol = k, nrow = nbulk)
        colnames(true.p1) <- unique(ct.id)[order(unique(ct.id))]
        rownames(true.p1) <- paste(pseudo_donors,1:nbulk,sep = "_")
        true.ct <- matrix(data = 0,ncol = k, nrow = nbulk) # true number of cells per cell type for each sample
        colnames(true.ct) <- unique(ct.id)[order(unique(ct.id))]
        rownames(true.ct) <- paste(pseudo_donors,1:nbulk,sep = "_")
        message("Generating random cell type proportions...")
    }

    # create pseudo bulk sample.id according to total available number of cells:
    pseudo_bulk <- NULL
    for(xx in 1:length(pseudo_donors)){ #length(pseudo_donors)
        # xx = 1
        message('generating bulk ', xx, ' from donor ', pseudo_donors[xx], '...')
        idxd <- sample.id == pseudo_donors[xx] # for a selected donor
        temp <- exprs(x.sub)[,idxd] # his expression matrix
        temp.cluster <- ct.id[idxd] # cluster info for cells

        # match names!!!!!!!!!!!!!!!!!
        temp.ncellk <- table(factor(temp.cluster))

        if (is.null(prop_mat)){ #if using random proportions
            temp.nct <- ceiling(runif(length(temp.ncellk), min = low_s, max = upp_s)*temp.ncellk) # take random number of available single cells
            true.p1[xx,names(temp.nct)] <- temp.nct/sum(temp.nct) # true proportions
            true.ct[xx,] <- temp.nct[colnames(true.ct)] # true number of cells in the pseudo bulk.
            true.p1[is.na(true.p1)] <- 0
            true.ct[is.na(true.ct)] <- 0
        } else { #if using the user-defined proportions
            temp.ntotal <- min(temp.ncellk / true.p1[xx,], na.rm = TRUE)
            if (temp.ntotal <= k){
                message("Please check if your input prop_mat is reasonable. The number of cells of certain selected cell type might be too small.")
            }
            true.ct[xx,] <- round(temp.ntotal*true.p1[xx,names(temp.ncellk)] ) # true number of cells in the pseudo bulk.
            true.p1[is.na(true.p1)] <- 0
            true.ct[is.na(true.ct)] <- 0
        }

        temp.b1 <- sapply(ct.sub, function(ucluster){
            temp.vec <- temp[,temp.cluster %in% ucluster] # for a specific cell type
            if (is.null(dim(temp.vec))){
                temp.sum <- rep(0, length(temp.vec))
            } else if (! is.null(dim(temp.vec))) {
                if (dim(temp.vec)[2] == 0){
                    temp.sum <- rep(0, dim(temp.vec)[1])
                } else {
                    temp.sample <- sample(1:ncol(temp.vec), true.ct[xx, ucluster], replace = samplewithRep) # which cells in this cell type will be selected
                    temp.mat <- temp.vec[,temp.sample] # select all those cells
                    if (is.null(dim(temp.mat))){
                        temp.sum <- temp.mat
                    } else {
                        temp.sum <- rowSums(temp.mat, na.rm = TRUE) # expression sum for one cell type in this bulk, need to sum up all types.
                    }
                }
            }
        })

        out = rowSums(temp.b1)
        pseudo_bulk <- cbind(pseudo_bulk, out)
        colnames(pseudo_bulk)[xx] <- paste(pseudo_donors[xx],xx,sep = "_")
    }
    # create pseudo eset for bulk sample.id
    if (! is.null(disease)){
        pseudo_pdata <- data.frame(sample.id = colnames(pseudo_bulk), donors = pseudo_donors, disease = disease[pseudo_donors])
    } else {
        pseudo_pdata <- data.frame(sample.id = colnames(pseudo_bulk), donors = pseudo_donors)
    }
    rownames(pseudo_pdata) <- colnames(pseudo_bulk)
    pseudo_fdata <- data.frame(labelDescription = rownames(pseudo_bulk),
                               row.names = rownames(pseudo_bulk))
    message("generating expression set object for pseudo bulk sample.id...")
    pseudo_eset <- ExpressionSet(pseudo_bulk,
                                 AnnotatedDataFrame(pseudo_pdata),
                                 AnnotatedDataFrame(pseudo_fdata))
    pseudo_eset0 <- pseudo_eset[,rowSums(!is.na(true.p1))>0] #non-zero eset
    true.p0 <- true.p1[rowSums(!is.na(true.p1))>0,]
    true.ct0 <- true.ct[rowSums(!is.na(true.p1))>0,]
    return(list(true_p = true.p1, pseudo_bulk = pseudo_bulk, pseudo_eset = pseudo_eset,
                num.real = true.ct, true_p0 = true.p0, true.ct0 = true.ct0, pseudo_eset0 = pseudo_eset0)) # , entropy = entropy
}
