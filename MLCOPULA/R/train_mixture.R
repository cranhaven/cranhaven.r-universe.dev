train.mixture <- function(X, y, distribution, weights, cop, graph_model){
  nc <- length(unique(y))
  est <- list()
  for(c in 0:(nc-1)){
    Xc <- X[y == c, ]
    #Estimar marginales
    den_est <- density.estimation(Xc, distribution = distribution)
    #Estimar copulas
    #est_cop <- build.weights(den_est$U, cop.est = estimation.amh,cop = "amh",
    #                         weights = weights)
    est_cop <- build.weights2(U = den_est$U, weights = weights, cop = cop)
    #
    if(graph_model == "tree"){
      g <- graph_from_adjacency_matrix(adjmatrix = -1 * est_cop$w, 
                                       mode = "undirected", weighted = TRUE)
      mst_result <- mst(g)
      #arbol <- as_data_frame(mst_result)
      arbol <- data.frame(as_edgelist(mst_result))
      colnames(arbol) <- c("from","to")
      #arbol <- faux(arbol = arbol, m = est_cop$w)
    } else {
      mst_result <- selection(f.aux2(est_cop$w)) 
      arbol <- mst_result$table
      #arbol <- faux(arbol = arbol, m = est_cop$w)
    }
    est[[c + 1]] <- list(den = den_est, copula = est_cop,
                         mst_result = mst_result, arbol = arbol,
                         distribution = distribution,nclass = nc)
  }
  return(model = est)
}
