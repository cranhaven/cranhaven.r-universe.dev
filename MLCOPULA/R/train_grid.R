#' @title Train a classification model using the Grid copula.
#' @param X Matrix with predictor variables.
#' @param y Numerical vector with the classes 
#' to predict, y = {0,1,...,nclass}.
#' @param distribution Distribution to be used: normal or kernels, 
#' by default normal.  
#' @param weights Character with the weight construction method:
#'  "likelihood" or "mutual_information", by default likelihood.
#' @param k positive integer indicating the 
#' number of subintervals for the U2 variable.
#' @param m positive integer indicating the number
#'  of subintervals for the U1 variable.
#' @param method Method that uses, least squares "ls" or
#'  maximum likelihood "ml", by default "ml".

train.grid <- function(X, y, distribution,k, m,method,weights,graph_model){
  nc <- length(unique(y))
  est <- list()
  for(c in 0:(nc-1)){
    Xc <- X[y == c,]
    #Estimar marginales
    den_est <- density.estimation(Xc,distribution = distribution)
    #Estimar copulas
    est_cop <- build.weightsGRID(den_est$U, 
                                 k = k, m = m, 
                                 method = method,
                                 weights = weights)
    #
    if(graph_model == "tree"){
      g <- graph_from_adjacency_matrix(adjmatrix = -1 * est_cop$w, 
                                       mode = "undirected", weighted = TRUE)
      mst_result <- mst(g)
      #arbol <- as_data_frame(mst_result)
      arbol <- data.frame(as_edgelist(mst_result))
      colnames(arbol) <- c("from","to")
      arbol <- faux(arbol = arbol, m = est_cop$w)
    } else {
      mst_result <- selection(f.aux2(est_cop$w)) 
      arbol <- mst_result$table
      arbol <- faux(arbol = arbol, m = est_cop$w)
    }
    est[[c + 1]] <- list(den = den_est, copula = est_cop,
                         mst_result = mst_result, arbol = arbol,
                         distribution = distribution,
                         cop = "grid",nclass = nc)
  }
  
  return(model = est)
}
