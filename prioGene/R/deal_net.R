#' Title deal with network
#'
#' @param net a network
#' @param dise_gene a matrix with one column of genes
#'
#' @return a matrix
#' @export
#'
#' @examples
#' deal_net(net,dise_gene)
deal_net <- function(net, dise_gene) {
    net_genes <- union(net[, 1], net[, 2])
    dise_genes <- intersect(dise_gene[, 1], net_genes)
    aa1 <- which(net[, 1] %in% dise_genes)
    aa2 <- which(net[, 2] %in% dise_genes)
    aa <- union(aa1, aa2)
    net_disease <- net[aa, ]
    return(net_disease)
}
