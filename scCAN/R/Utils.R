.dominateset <- function(xx,KK=20) {
  ###This function outputs the top KK neighbors.

  zero <- function(x) {
    s = sort(x, index.return=TRUE)
    x[s$ix[1:(length(x)-KK)]] = 0
    return(x)
  }
  normalize <- function(X) X / rowSums(X)
  A = matrix(0,nrow(xx),ncol(xx));
  for(i in 1:nrow(xx)){
    A[i,] = zero(xx[i,]);

  }


  return(normalize(A))
}

.discretisation <- function(eigenVectors) {

  normalize <- function(x) x / sqrt(sum(x^2))
  eigenVectors = t(apply(eigenVectors,1,normalize))

  n = nrow(eigenVectors)
  k = ncol(eigenVectors)

  R = matrix(0,k,k)
  R[,1] = t(eigenVectors[round(n/2),])

  mini <- function(x) {
    i = which(x == min(x))
    return(i[1])
  }

  c = matrix(0,n,1)
  for (j in 2:k) {
    c = c + abs(eigenVectors %*% matrix(R[,j-1],k,1))
    i = mini(c)
    R[,j] = t(eigenVectors[i,])
  }

  lastObjectiveValue = 0
  for (i in 1:20) {
    eigenDiscrete = .discretisationEigenVectorData(eigenVectors %*% R)

    svde = svd(t(eigenDiscrete) %*% eigenVectors)
    U = svde[['u']]
    V = svde[['v']]
    S = svde[['d']]

    NcutValue = 2 * (n-sum(S))
    if(abs(NcutValue - lastObjectiveValue) < .Machine$double.eps)
      break

    lastObjectiveValue = NcutValue
    R = V %*% t(U)

  }

  return(list(discrete=eigenDiscrete,continuous =eigenVectors))
}

.discretisationEigenVectorData <- function(eigenVector) {

  Y = matrix(0,nrow(eigenVector),ncol(eigenVector))
  maxi <- function(x) {
    i = which(x == max(x))
    return(i[1])
  }
  j = apply(eigenVector,1,maxi)
  Y[cbind(1:nrow(eigenVector),j)] = 1

  return(Y)

}




#' @title adjustedRandIndex
#' @description The function to calculate adjusted Rand index value with the
#' inputs of true clusters and predicted clusters
#' @param x A vector that contain predicted cluster assignment.
#' @param y A vector that contain true cluster assignment.
#' @return An value number ranging from 0 to 1 where 1 indicates a perfect
#' clustering result and 0 means random partition.
#' @export
#'
adjustedRandIndex <- function (x, y)
{
  x <- as.vector(x)
  y <- as.vector(y)
  if(length(x) != length(y))
    stop("arguments must be vectors of the same length")
  tab <- table(x,y)
  if(all(dim(tab)==c(1,1))) return(1)
  a <- sum(choose(tab, 2))
  b <- sum(choose(rowSums(tab), 2)) - a
  c <- sum(choose(colSums(tab), 2)) - a
  d <- choose(sum(tab), 2) - a - b - c
  ARI <- (a - (a + b) * (a + c)/(a + b + c + d)) /
    ((a + b + a + c)/2 - (a + b) * (a + c)/(a + b + c + d))
  return(ARI)
}

#' @title get_cluster_markers
#' @description Find markers for each cluster
#' @param input_data_matrix An expression matrix in which rows are genes and
#' columns are cells.
#' @param labels_vector A vector of cluster labels ontained from clustering methods.
#' @param threads A parameter to control number of cores used for analysis
#' \code{threads = 1} by default.
#' @return A list that contains markers for each cluster.
#' @export
#'
get_cluster_markers <- function (input_data_matrix, labels_vector, threads = 1) # score markers
{
  data = input_data_matrix
  store_lists <- list()

  if(min(labels_vector) == 0) {
    labels_vec <- labels_vector + 1
  }
  else{
    labels_vec <- labels_vector
  }

  s1_list <- sort(unique(labels_vec))

  for (i in c(1:length(unique(s1_list)))) {
    message(i)
    store_lists[[s1_list[i]]] <- find_markers(data, labels_vec,
                                              identity = s1_list[i], threads = threads)
  }
  return(store_lists)
}

#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom stats wilcox.test
#' @title find_markers
#' @description Perform cluster-wise Wilcox test and fold-change for each gene.
#' @param input_data_matrix An expression matrix in which rows are genes and
#' columns are cells.
#' @param cluster_labels A vector of cluster labels obtained from clustering methods.
#' @param identity A parameter to select specific cluster \code{identity = 1}
#' by default.
#' @param threads A parameter to control number of cores used for analysis
#' \code{threads = 1} by default.
#' @return A list that contains p-value and fold-change ratio for all genes of each cluster.
find_markers <- function(input_data_matrix, cluster_labels, identity=1, threads = 8){
  groups = as.character(cluster_labels)
  identity = as.character(identity)
  groups[which(groups==identity)] <- "a"
  groups[which(groups!="a")] <- "b"
  message(table(groups))
  cl <- makeCluster(threads)

  res1 <- parLapply(cl, 1:nrow(input_data_matrix), fun=function(x){
    wilcox.test(input_data_matrix[x, ]~factor(groups), correct = T)$p.value})
  stopCluster(cl)
  closeAllConnections()

  tt <- Sys.time()
  avg.a = apply(input_data_matrix[,groups=="a"], 1, mean)
  avg.na = apply(input_data_matrix[,groups!="a"], 1, mean)

  log2fc = avg.a / avg.na
  message(length(log2fc))
  te <- Sys.time()
  message(paste("AVG Time: ", te-tt))
  return(list(res1, log2fc))
}


#' @title calculate_celltype_prob
#' @description Calculate clusters and cell types similarity based on the markers.
#' @param clt_marker_list A list of markers for all cluster.
#' @param marker_database_list A list of markers of all reference cell types.
#' @param type A parameter to select the method to measure cluster and cell type
#' similarity
#' \itemize{
#' \item jacc - Jaccard index.
#' \item ac - Accuracy.
#' \item f1 - F1 score.
#' }.
#' @return A confusion matrix between clusters and cell types. Each cell represents
#' a probabilty of a cluster belongs to a cell type.
#' @export
calculate_celltype_prob <- function (clt_marker_list, marker_database_list, type = "jacc")
{
  marker_database_list <- marker_database_list
  no_clts <- length(clt_marker_list)
  no_celltypes <- length(marker_database_list)

  result_matrix <- matrix(0, nrow = no_clts, ncol = no_celltypes)

  for (i in c(1:no_celltypes)) {
    # tmp_markers <- tolower(marker_database_list[[i]])
    tmp_markers <- marker_database_list[[i]]
    tmp_results <- find_specific_marker(tmp_markers, clt_marker_list,
                                        type)
    result_matrix[, i] <- tmp_results
  }
  return(result_matrix)
}

#' @title find_specific_marker
#' @description Calculate cluster and cell type similarity based on the markers.
#' @param gene_name A list of markers belong to the cluster.
#' @param f_list A list of markers belongs to a reference cell type.
#' @param type A parameter to select the method to measure cluster and cell type
#' similarity
#' \itemize{
#' \item jacc - Jaccard index.
#' \item ac - Accuracy.
#' \item f1 - F1 score.
#' }.
#' @return A vector of probabilties of a cluster belongs to cell types.

find_specific_marker <- function(gene_name, f_list, type = "jacc"){
  store_present <- c()
  # type: f1 / ac
  for(i in c(1:length(f_list))){
    # g1 <- match(gene_name, tolower(f_list[[i]]))
    g1 <- match(gene_name, f_list[[i]])
    tp = length(which(!is.na(g1)))
    fp = length(f_list[[i]]) - tp
    fn = length(gene_name) - tp
    pr = tp / (tp+fp)
    re = tp / (tp+fn)
    if(pr==0 & re==0){
      f1 = 0
    }else{
      f1 = 2*(pr*re / (pr+re))
    }
    acc_sc <- length(which(!is.na(g1)))/length(gene_name)
    # Calculate jaccard index
    jacc <- length(which(!is.na(g1))) / (length(gene_name) + length(f_list[[i]]))
    if(type == "ac"){
      store_present <- c(store_present, acc_sc)
    }else if(type=="f1"){
      store_present <- c(store_present, f1)
    }else{
      store_present <- c(store_present, jacc)
    }
  }
  return(store_present)
}

#' @title curate_markers
#' @description Filter genes that have low p-value and fold-change.
#' @param whole_list A list of markers for all clusters.
#' @param gene_names All the gene names of the expression matrix.
#' @param wilcox_threshold A threshold for p-value \code{wilcox_threshold = 0.001} by default.
#' @param logfc_threshold A threshold for fold-change \code{logfc_threshold = 1.5} by default.
#' @return A list of markers that are strong expressed for discovered clusters.
#' @export
#'
curate_markers <- function (whole_list, gene_names, wilcox_threshold = 0.001, logfc_threshold = 1.5)
{
  store_genes <- list()
  n_clusters <- length(whole_list)
  non_na <- which(!is.na(gene_names))
  for (i in c(1:n_clusters)) {
    c1 <- intersect(which(as.numeric(whole_list[[i]][[1]]) <=
                            wilcox_threshold), which(as.numeric(whole_list[[i]][[2]]) >
                                                       logfc_threshold))
    c_markers <- gene_names[intersect(c1, non_na)]
    message(length(c_markers))
    if (length(c_markers) == 0) {
      subset1 <- which(as.numeric(whole_list[[i]][[2]]) >
                         logfc_threshold)
      sorted_wilcox <- order(as.numeric(whole_list[[i]][[1]]),
                             decreasing = F)
      len_pos <- c(1:length(whole_list[[i]][[1]]))
      len_sorted <- len_pos[sorted_wilcox]
      selected_pos <- intersect(len_sorted, subset1)[1:100]
      message(as.numeric(whole_list[[i]][[1]])[selected_pos])
      c_markers <- gene_names[selected_pos]
    }
    store_genes[[i]] <- c_markers
  }
  return(store_genes)
}
