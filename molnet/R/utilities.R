graph_metrics <- function(graph, verbose = TRUE, return = FALSE) {
  #' Analyses metrics of an iGraph object
  #'
  #' This helper function prints or returns multiple metrics of arbitrary
  #' iGraph graph object.
  #'
  #' @param graph iGraph object to analyze.
  #' @param verbose If TRUE graph information is printed.
  #' @param return If TRUE graph information is returned from function.
  #' @export
  #'
  #' @examples
  #' adj_mat <- matrix(rnorm(36),nrow=6)
  #' graph <- igraph::graph_from_adjacency_matrix(adj_mat)
  #' graph_metrics(graph, verbose = TRUE, return = FALSE)
  #'
  #'
  #' @return Named list of metrics including vertex count, edge count, number of components,
  #' size of largest component and the relative frequency of zero degree vertices.
  #'

  metrics <- list(
    'n_vertices' = igraph::gorder(graph),
    'n_edges' = igraph::ecount(graph),
    'components' = igraph::components(graph),
    'degree_distribution' = igraph::degree_distribution(graph)
  )

  if(verbose == TRUE) {
    message('Vertex count:', metrics$n_vertices, '\n')
    message('Edge count:', metrics$n_edges, '\n')
    message('Number of components:', metrics$components$no, '\n')
    message('Size of largest component:', metrics$components$csize[1], '\n')
    message('Relative frequency of zero degree vertices:', metrics$degree_distribution[1], '\n')
  }

  if(return == TRUE) {
    return(metrics)
  }
}

set_cluster <- function(n_threads) {
  #' @title Create and register cluster
  #'
  #' @description (INTERNAL) Helper function to create and register a cluster for parallel
  #' computation
  #' of p-value reduction
  #'
  #' @param n_threads number of nodes in the cluster
  #'
  #' @export
  #'
  parallel::setDefaultCluster(parallel::makeCluster(n_threads))
}

shutdown_cluster <- function() {
  #' Shutdown cluster and remove corresponding connections
  #'
  #' (INTERNAL) Run this if the pipeline fails during parallel computation to
  #' clean the state. If a cluster is registered, this functions stops it and
  #' removes corresponding connections. Ignores errors. Has no effect if no
  #' cluster is registered.
  #'
  #' @export
  #'
  try(parallel::stopCluster(parallel::getDefaultCluster()), silent = TRUE)
  try(closeAllConnections(), silent = TRUE)
}

install_python_dependencies <- function(package_manager="pip3") {
  #' Installs python dependencies needed for interaction score computation
  #'
  #' Uses specified pip or conda executable (default: pip3) to install all
  #' required python modules. When using conda, the currently active
  #' environment is used. Commands run are `pip install -r requirements` or
  #' `conda install --file requirements`. Installs the following requirements:
  #' - numpy
  #' - tqdm
  #' - python-igraph
  #' - ray
  #'
  #' @param package_manager The package manager command or path to use (default: pip3)
  #'
  #' @export
  #'
  py_requirements = system.file("requirements.txt", package = "molnet")
  if (grepl("pip", package_manager, fixed = TRUE)) {
    system2(package_manager, args = c("install", "-r", py_requirements))
  }
  else if (grepl("conda", package_manager, fixed = TRUE)) {
    system2(package_manager, args = c("install", "--file", py_requirements))
  }
}

get_layer <- function(name, layers) {
  #' [INTERNAL] Fetch layer by name from layer object
  #'
  #' @param name The layer to fetch
  #' @param layers a layers object \code{\link{layers_example}}
  #' @return Returns the layer along with layer names
  #' @export
  layer_names <- sapply(layers, function(l) l[['name']])
  return(layers[[which(layer_names == name)]])
}
