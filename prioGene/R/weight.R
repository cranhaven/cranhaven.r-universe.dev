
#' Title weight node
#'
#' @param genes_mat a one-to-many matrix of GO term and gene
#'
#' @return a matrix
#' @export
#'
#' @examples
#' get_node_weight(genes_mat)
get_node_weight <- function(genes_mat) {
    n <- sum(as.numeric(genes_mat[, 3]))
    node_weight <- cbind(genes_mat[, 1], as.numeric(genes_mat[, 3])/n)
}

#' Title weight edge
#'
#' @param net_disease_term GO terms for each pair of nodes in the network
#'
#' @param terms_mat result of get_term_mat()
#'
#' @return a matrix
#' @export
#'
#' @examples
#' get_edge_weight(net_disease_term,terms_mat)
get_edge_weight <- function(net_disease_term, terms_mat) {
    edge_weight <- cbind(net_disease_term[, seq_len(2)], "gaga")
    
    for (i in seq_len(dim(edge_weight)[1])) {
        aa <- unlist(strsplit(net_disease_term[i, 3], ","))
        bb <- terms_mat[aa, 3]
        edge_weight[i, 3] <- sum(1/as.numeric(bb))     
    }
    edge_weight_haha <- edge_weight
    for (i in seq_len(dim(edge_weight_haha)[1])) {
        edge_weight_haha[i,seq_len(2)] <- sort(edge_weight_haha[i,seq_len(2)])
    }
    rownames(edge_weight) <- paste(edge_weight_haha[, 1], 
    edge_weight_haha[, 2], sep = "_")
    return(edge_weight)
}

