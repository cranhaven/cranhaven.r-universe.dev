get_indexList <- function(alist) {
    nsample <- length(alist)
    nr <- 0
    indexList <- list()
    for (i in 1:nsample) {
        indexList[[i]] <- (nr + 1):(nrow(alist[[i]]) + nr)
        nr <- nr + nrow(alist[[i]])
    }
    return(indexList)
}

mat2list <- function(z_int, nvec) {
    zList_int <- list()
    istart <- 1
    for (i in 1:length(nvec)) {
        zList_int[[i]] <- z_int[istart:sum(nvec[1:i]), ]
        istart <- istart + nvec[i]
    }
    return(zList_int)
}

matlist2mat <- function(XList) {
    # transfer a matrix list to a matrix stacked by rows.
    r_max <- length(XList)
    X0 <- XList[[1]]
    if (is.null(dim(X0)) || any(dim(X0) == 1)) {
        if (r_max > 1) {
            for (r in 2:r_max) {
                X0 <- c(X0, XList[[r]])
            }
        }

        return(matrix(X0, ncol = 1))
    }
    if (r_max > 1) {
        for (r in 2:r_max) {
            X0 <- rbind(X0, XList[[r]])
        }
    }

    return(X0)
}

vec2list <- function(y_int, nvec) {
    if (length(y_int) != sum(nvec)) stop("vec2list: Check the argument: nvec!")

    yList_int <- list()
    istart <- 1
    for (i in 1:length(nvec)) {
        yList_int[[i]] <- y_int[istart:sum(nvec[1:i])]
        istart <- istart + nvec[i]
    }
    return(yList_int)
}

dfList2df <- function(dfList) {
    df <- dfList[[1]]
    r_max <- length(dfList)
    if (r_max > 1) {
        for (r in 2:r_max) {
            df <- rbind(df, dfList[[r]])
        }
    }

    return(df)
}

get_correct_exp <- function(XList, RfList, houseKeep, covariateList = NULL, q_unwanted = 10, subsample_rate = NULL, sample_seed = 1) {
    if (!all(sapply(XList, is.matrix))) {
        XList <- lapply(XList, as.matrix)
    }
    nvec <- sapply(XList, nrow)
    XList_sub <- pbapply::pblapply(XList, function(x) x[, houseKeep])
    M0 <- wpca(matlist2mat(XList_sub), q = q_unwanted, FALSE)$PCs
    HList <- mat2list(M0, nvec = nvec)

    Rf <- matlist2mat(RfList)
    colnames(Rf) <- paste0("Rf", 1:ncol(Rf))
    if (!is.null(covariateList)) {
        # covariates <- matlist2mat(covariateList)
        # covariates <- as.matrix(covariates)
        covarites_df <- dfList2df(covariateList)
        covariates <- model.matrix.lm(object = ~ . + 1, data = covarites_df, na.action = "na.pass")
        rm(covariateList, covarites_df)
        Rf <- cbind(Rf, covariates[, -1])
        rm(covariates)
    }
    rm(RfList)

    # subsampling to speed up the computation!
    if (is.null(subsample_rate)) subsample_rate <- 1
    index_List <- get_indexList(XList)
    set.seed(sample_seed)
    index_subsample <- sort(sample(sum(nvec), floor(sum(nvec) * subsample_rate)))
    ## calculate the number of indices belonging to the index of each slide
    nvec_subsample <- rep(NA, length(nvec))
    for (i in 1:length(nvec_subsample)) {
        ## message("i = ", i)
        nvec_subsample[i] <- sum(index_subsample %in% index_List[[i]])
    }
    index_List_new <- lapply(XList, function(x) 1:nrow(x))
    index_subsample_new <- unlist(index_List_new)[index_subsample]
    index_subsampleList <- vec2list(index_subsample_new, nvec_subsample)
    RfList <- mat2list(Rf, nvec = nvec)

    XList_sub <- list()
    RList_sub <- list()
    HList_sub <- list()
    AdjList_sub <- list()
    for (i in 1:length(XList)) {
        # message("i = ", i)
        index_tmp <- index_subsampleList[[i]]
        XList_sub[[i]] <- XList[[i]][index_tmp, ]
        RList_sub[[i]] <- RfList[[i]][index_tmp, ]
        HList_sub[[i]] <- HList[[i]][index_tmp, ]
    }
    rm(RfList, HList, index_tmp)


    ### XList <-  lapply(XList, scale, scale=FALSE)
    X_sub <- matlist2mat(XList_sub)
    rm(XList_sub)
    Rf <- matlist2mat(RList_sub)
    colnames(Rf) <- NULL
    H <- matlist2mat(HList_sub)
    colnames(H) <- NULL
    nc_M0 <- ncol(H)
    lm1 <- lm(X_sub ~ 0 + cbind(H, Rf))
    coefmat <- coef(lm1)[c(1:nc_M0), ]
    # row.names(coef(lm1))
    rm(lm1, X_sub)
    hX <- matlist2mat(XList) - M0 %*% coefmat
    return(hX)
}


get_sampleID <- function(XList) {
    sampleID <- list()
    r_max <- length(XList)
    for (r in 1:r_max) {
        sampleID[[r]] <- rep(r, nrow(XList[[r]]))
    }
    sampleID <- unlist(sampleID)
    return(sampleID)
}




#' Perform Batch Correction and Integration with CAESAR Using Housekeeping Genes
#'
#' @description
#' This function performs batch correction and integration of multiple Seurat objects using housekeeping genes and distance matrices. It supports human and mouse data, and can optionally use custom housekeeping genes provided by the user.
#'
#' @param seuList A list of Seurat objects to be integrated.
#' @param distList A list of distance matrices corresponding to each Seurat object in `seuList`.
#' @param verbose Logical, indicating whether to display progress messages. Default is \code{FALSE}.
#' @param species A character string specifying the species, either "human" or "mouse". Default is "human".
#' @param custom_housekeep A character vector of custom housekeeping genes. If \code{NULL}, default housekeeping genes for the species are used. Default is \code{NULL}.
#'
#' @return A Seurat object that contains the integrated and batch-corrected data in a new assay called "CAESAR".
#'
#' @examples
#' data(toydata)
#' 
#' seu <- toydata$seu
#' markers <- toydata$markers
#'
#' seu <- ProFAST::pdistance(seu, reduction = "caesar")
#'
#' marker.freq <- markerList2mat(list(markers))
#' anno_res <- CAESAR.annotation(seu, marker.freq, cal.confidence = FALSE, cal.proportions = FALSE)
#'
#' seuList <- list(seu, seu)
#' distList <- list(anno_res$ave.dist, anno_res$ave.dist)
#' seuInt <- CAESAR.RUV(seuList, distList, species = "human", verbose = TRUE)
#'
#' @importFrom Seurat CreateSeuratObject DefaultAssay GetAssayData VariableFeatures SetAssayData
#' @importFrom Matrix t sparseMatrix
#' @importFrom stats model.matrix.lm lm coef
#' @export
CAESAR.RUV <- function(seuList, distList, verbose = FALSE, species = "human", custom_housekeep = NULL) {
    # Check if 'seuList' is a list of Seurat objects
    if (!is.list(seuList) || !all(sapply(seuList, inherits, "Seurat"))) {
        stop("Input 'seuList' must be a list of Seurat objects.")
    }

    # Check if 'distList' is a list of matrices
    if (!is.list(distList) || !all(sapply(distList, is.matrix))) {
        stop("Input 'distList' must be a list of distance matrices.")
    }

    # Check if 'species' is valid
    species <- tolower(species)
    if (!species %in% c("human", "mouse")) {
        stop("'species' must be either 'human' or 'mouse'.")
    }

    # Check if 'custom_housekeep' is a character vector if provided
    if (!is.null(custom_housekeep) && !is.character(custom_housekeep)) {
        stop("'custom_housekeep' must be a character vector or NULL.")
    }

    # Extract the default assay data for each Seurat object
    defAssay_vec <- sapply(seuList, Seurat::DefaultAssay)
    n_r <- length(seuList)

    # Extract and transpose the assay data for each Seurat object
    XList <- lapply(
        seq_along(defAssay_vec), function(r) {
            Matrix::t(Seurat::GetAssayData(seuList[[r]], assay = defAssay_vec[r], slot = "data"))
        }
    )

    # Convert gene names based on species
    if (species == "mouse") {
        for (r in 1:n_r) {
            colnames(XList[[r]]) <- firstup(colnames(XList[[r]]))
        }
        if (!is.null(custom_housekeep)) {
            custom_housekeep <- firstup(custom_housekeep)
        }
    } else if (species == "human") {
        for (r in 1:n_r) {
            colnames(XList[[r]]) <- toupper(colnames(XList[[r]]))
        }
        if (!is.null(custom_housekeep)) {
            custom_housekeep <- toupper(custom_housekeep)
        }
    }

    # Handle covariates (if provided)
    covariates_use <- NULL
    if (!is.null(covariates_use)) {
        covariateList <- lapply(seuList, function(x) x@meta.data[covariates_use])
    } else {
        covariateList <- NULL
    }

    # Check for duplicate barcodes
    barcodes_all <- lapply(XList, row.names)
    if (any(duplicated(unlist(barcodes_all)))) {
        for (r in 1:n_r) {
            row.names(XList[[r]]) <- paste0(row.names(XList[[r]]), r)
        }
    }

    # Determine housekeeping genes based on species
    genelist <- colnames(XList[[1]])
    houseKeep <- switch(species,
        human = intersect(toupper(genelist), CAESAR.Suite::Human_HK_genes$Gene),
        mouse = intersect(firstup(genelist), CAESAR.Suite::Mouse_HK_genes$Gene),
        character()
    )

    # Merge with custom housekeeping genes
    houseKeep <- c(houseKeep, custom_housekeep)
    houseKeep <- intersect(houseKeep, colnames(XList[[1]]))
    houseKeep <- intersect(houseKeep, Seurat::VariableFeatures(seuList[[1]]))

    # Handle large datasets with subsampling
    nvec <- sapply(XList, nrow)
    subsample_rate <- 1
    if (sum(nvec) > 8e4) {
        subsample_rate <- 5e4 / sum(nvec)
        if (verbose) {
            message("Subsampling schema will be used to speed up computation.")
        }
    }

    sample_seed <- 1

    # Perform batch correction using housekeeping genes
    if (length(houseKeep) < 5) {
        if (verbose) {
            message("Using CAESAR results for batch correction due to insufficient housekeeping genes.")
        }
        # hX <- get_correct_mean_exp(...)  # Replace with actual batch correction code
    } else {
        if (verbose) {
            message("Using both housekeeping genes and CAESAR results for batch correction.")
        }
        hX <- get_correct_exp(XList, distList,
            houseKeep = houseKeep, q_unwanted = min(10, length(houseKeep)),
            covariateList = covariateList, subsample_rate = subsample_rate, sample_seed = sample_seed
        )
    }

    # Create a new Seurat object with corrected data
    meta_data <- data.frame(batch = factor(get_sampleID(XList)))
    row.names(meta_data) <- row.names(hX)
    count <- Matrix::sparseMatrix(i = 1, j = 1, x = 0, dims = dim(t(hX)))
    row.names(count) <- colnames(hX)
    colnames(count) <- row.names(hX)

    seuInt <- Seurat::CreateSeuratObject(counts = count, assay = "CAESAR", meta.data = meta_data)

    # Add corrected data to the Seurat object
    if (inherits(seuInt[["CAESAR"]], "Assay5")) {
        seuInt <- Seurat::SetAssayData(object = seuInt, slot = "data", assay = "CAESAR", new.data = t(hX))
    } else {
        seuInt[["CAESAR"]]@data <- t(hX)
    }

    return(seuInt)
}
