
#' Title
#'
#' @param node1 a gene
#' @param node2 a gene
#'
#' @return a number
get_W <- function(node1, node2) {
    node <- paste(sort(c(node1, node2)), collapse = "_")
    return(as.numeric(ifelse(node %in% rownames(edge_weight), 
    edge_weight[node, 3], 0)))
}

#' Title get neighbor of a node
#'
#' @param node a gene
#' @param net a network
#'
#' @return a vector of gene
get_neighbor <- function(node, net) {
    aa1 <- which(net[, 1] == node)
    aa2 <- which(net[, 2] == node)
    return(union(net[aa1, 2], net[aa2, 1]))
}


#' Title get the disease risk transition probability matrix
#'
#' @param node_weight a matrix, genes and their weights
#' @param net_disease_term GO terms for each pair of nodes in the network
#' @return a matrix

get_Q <- function(node_weight, net_disease_term) {
    Q <- matrix(0, dim(node_weight)[1], dim(node_weight)[1])
    rownames(Q) <- node_weight[, 1]
    colnames(Q) <- node_weight[, 1]
    for (i in seq_len(dim(net_disease_term)[1])) {
        Wij <- get_W(net_disease_term[i, 1], net_disease_term[i, 2])
        node_i <- net_disease_term[i, 1]
        node_j <- net_disease_term[i, 2]
        node_j_neighbor <- get_neighbor(node_j, net_disease_term)
        wlj_sum <- 0
        for (mm in node_j_neighbor) {
            wlj_sum <- wlj_sum + get_W(mm, node_j)
        }
        
        Q[node_i, node_j] <- Wij/wlj_sum
        Wji <- Wij
        node_i_neighbor <- get_neighbor(node_i, net_disease_term)
        wli_sum <- 0
        for (mm in node_i_neighbor) {
            wli_sum <- wli_sum + get_W(mm, node_i)
        }
        Q[node_j, node_i] <- Wji/wli_sum
    }
    
    return(Q)
}

#' Title get the vector of initial disease risk scores for all genes
#'
#' @param disease_gene a matrix of a column of genes
#' @param node_weight a matrix, genes and their weights
#' @param f  an integer parameter to measure the significance of disease
#'genes and candidate genes
#'
#' @return a vector
#' @export
#'
#' @examples
#' get_R_0(dise_gene,node_weight,1)
get_R_0 <- function(disease_gene, node_weight, f = 1) {
    is_disease <- rep(1, dim(node_weight)[1])
    
    is_disease[which(node_weight[, 1] %in% disease_gene)] <- f
    node_weight_sum <- sum(as.numeric(node_weight[, 2]) * is_disease)
    R_0 <-as.numeric(as.numeric(node_weight[, 2]) * is_disease/node_weight_sum)
    return(R_0)
}
#' Title get the final genetic disease risk scores
#'
#' @param bet  a parameter to measure the importance of genes and interactions
#' @param R_0 the vector of initial disease risk scores for all genes
#' @param node_weight a matrix, genes and their weights
#' @param threshold a threshold for terminating iterations
#' @param net_disease_term GO terms for each pair of nodes in the network
#' @return a matrix
#' @export
#'
#' @examples
#' net_disease <- deal_net(net,dise_gene)
#' genes_mat <- get_gene_mat(net_disease)
#' node_weight <- get_node_weight(genes_mat)
#' net_disease_term <- get_net_disease_term(genes_mat,net_disease)
#' R_0<- get_R_0(dise_gene,node_weight,f=1)
#' result <- get_R(node_weight, net_disease_term, bet = 0.5, R_0 = R_0, threshold = 10^(-9))
get_R <- function(node_weight, net_disease_term, bet, R_0, threshold = 10^(-9)) {
    
    QQ <- get_Q(node_weight, net_disease_term)
    QQ[is.na(QQ)] <- 0
    R_dise <- 1
    R_old <- R_0
    kk <- 1
    while (R_dise > threshold) {
        R_new <- (1 - bet) * QQ %*% R_old + bet * R_0
        Po_old = norm(as.matrix(R_old), "F")
        Po_new = norm(as.matrix(R_new), "F")
        R_dise <- abs(Po_new - Po_old)
        R_old <- R_new
        kk <- kk + 1
    }
    print(kk)
    result <- cbind(node_weight[, 1], R_new)
    return(result)
}

