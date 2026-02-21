

# get_gene_mat <- function(net_disease, GO_human) {
    # net_genes <- union(net_disease[, 1], net_disease[, 2])
    # aa <- which(GO_human[, 1] %in% net_genes)
    # GO_file <- GO_human[aa, ]
    # genes <- unique(GO_file[, 1])
    # genes_mat <- matrix("gaga", length(genes), 3)

    # genes_mat[, 1] <- genes
    # rownames(genes_mat) <- genes_mat[, 1]
    # for (i in seq_len(dim(GO_file)[1])) {
        # genes_mat[GO_file[i, 1], 2] <- paste(genes_mat[GO_file[i, 1], 2],
        # GO_file[i, 2], sep = ",")
    # }

    # for (i in seq_len(dim(genes_mat)[1])) {
        # genes_mat[i, 2] <- sub("gaga,", "", genes_mat[i, 2])
        # genes_mat[i, 3]<- length(unique(unlist(strsplit(genes_mat[i, 2],","))))
    # }
    # return(genes_mat)

# }

#' Get a one-to-many matrix of gene and GO term
#'
#' @param net_disease a disease related network, matrix
#' @return a matrix
#' @importFrom AnnotationDbi select
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @export
#'
#' @examples
#' get_gene_mat(net_disease)
get_gene_mat <- function(net_disease) {
    net_genes <- union(net_disease[, 1], net_disease[, 2])
    GO_file <- select(org.Hs.eg.db, keys=net_genes, columns="GO", keytype="SYMBOL")
    genes <- unique(GO_file[, "SYMBOL"])
    genes_mat <- matrix("gaga", length(genes), 3)
    
    genes_mat[, 1] <- genes
    rownames(genes_mat) <- genes_mat[, 1]
    for (i in seq_len(dim(GO_file)[1])) {
        if(!is.na(GO_file[i, "GO"]))
            genes_mat[GO_file[i, "SYMBOL"], 2] <- paste(genes_mat[GO_file[i, "SYMBOL"], 2],
                GO_file[i, "GO"], sep = ",")
    }

    for (i in seq_len(dim(genes_mat)[1])) {
        genes_mat[i, 2] <- sub("gaga,", "", genes_mat[i, 2])
        genes_mat[i, 3]<- length(unique(unlist(strsplit(genes_mat[i, 2],","))))
    }
    genes_mat <- genes_mat[which(genes_mat[,2] != "gaga"),]
    return(genes_mat)
}


# get_term_mat <- function(net_disease, GO_human) {
    # net_genes <- union(net_disease[, 1], net_disease[, 2])
    # aa <- which(GO_human[, 1] %in% net_genes)
    # GO_file <- GO_human[aa, ]
    # go_terms <- unique(GO_file[, 2])
    # terms_mat <- matrix("gaga", length(go_terms), 3)

    # terms_mat[, 1] <- go_terms
    # rownames(terms_mat) <- terms_mat[, 1]
    # for (i in seq_len(dim(GO_file)[1])) {
        # terms_mat[GO_file[i, 2], 2] <- paste(terms_mat[GO_file[i, 2], 2],
        # GO_file[i, 1], sep = ",")
    # }

    # for (i in seq_len(dim(terms_mat)[1])) {
        # terms_mat[i, 2] <- sub("gaga,", "", terms_mat[i, 2])
        # terms_mat[i, 3] <-length(unique(unlist(strsplit(terms_mat[i, 2],","))))
    # }
    # return(terms_mat)

# }

#' Get a one-to-many matrix of GO term and gene
#'
#' @param net_disease a disease related network, matrix
#' @return a matrix
#' @importFrom AnnotationDbi select
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @export
#'
#' @examples
#' get_term_mat(net_disease)
get_term_mat <- function(net_disease) {
    net_genes <- union(net_disease[, 1], net_disease[, 2])
    GO_file <- select(org.Hs.eg.db, keys=net_genes, columns="GO", keytype="SYMBOL")
    go_terms <- unique(GO_file[, "GO"])
    terms_mat <- matrix("gaga", length(go_terms), 3)
    terms_mat[, 1] <- go_terms
    rownames(terms_mat) <- terms_mat[, 1]
    for (i in seq_len(dim(GO_file)[1])) {
        if(!is.na(GO_file[i, "GO"]))
            terms_mat[GO_file[i, "GO"], 2] <- paste(terms_mat[GO_file[i, "GO"], 2],
                GO_file[i, "SYMBOL"], sep = ",")
    }

    for (i in seq_len(dim(terms_mat)[1])) {
        terms_mat[i, 2] <- sub("gaga,", "", terms_mat[i, 2])
        terms_mat[i, 3] <-length(unique(unlist(strsplit(terms_mat[i, 2],","))))
    }
    terms_mat <- terms_mat[which(terms_mat[,2] != "gaga"),]
    return(terms_mat)
}



#' Title Get the GO terms for each pair of nodes in the network
#'
#' @param genes_mat a one-to-many matrix of GO term and gene
#' @param net_disease a disease related network, matrix
#'
#' @return a matrix
#' @export
#'
#' @examples
#' get_net_disease_term(genes_mat,net_disease)
get_net_disease_term <- function(genes_mat, net_disease) {
    genes <- genes_mat[, 1]
    aa1 <- which(net_disease[, 1] %in% genes)
    aa2 <- which(net_disease[, 2] %in% genes)
    aa <- intersect(aa1, aa2)
    net_disease_small <- net_disease[aa, ]
    inter_term <- rep(0, dim(net_disease_small)[1])
    inter_term_num <- rep(0, dim(net_disease_small)[1])
    for (i in seq_len(length(inter_term))) {
        aa1 <- unlist(strsplit(genes_mat[net_disease_small[i, 1], 2], ","))
        aa2 <- unlist(strsplit(genes_mat[net_disease_small[i, 2], 2], ","))
        aa <- intersect(aa1, aa2)
        inter_term[i] <- paste(aa, collapse = ",")
        inter_term_num[i] <- length(aa)
    }
    net_disease_term <- cbind(net_disease_small,
    inter_term, inter_term_num)
    return(net_disease_term)
}
