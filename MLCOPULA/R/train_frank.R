#' @title Train a classification model using the Frank copula.
#' @param X Matrix with predictor variables.
#' @param y Numerical vector with the classes 
#' to predict, y = {0,1,...,nclass}.
#' @param distribution Distribution to be used: normal or kernels, 
#' by default normal.  
#' @param weights Character with the weight construction method:
#'  "likelihood" or "mutual_information", by default likelihood.

train.frank <- function(X, y, distribution,weights,graph_model){
  nc <- length(unique(y))
  est <- list()
  for(c in 0:(nc-1)){
    Xc <- X[y == c,]
    #Estimar marginales
    den_est <- density.estimation(Xc,distribution = distribution)
    #Estimar copulas
    est_cop <- build.weights(den_est$U, cop.est = estimation.frank,cop = "frank",
                             weights = weights)
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
                         distribution = distribution,
                         cop = "frank",nclass = nc)
  }
  
  return(model = est)
}
