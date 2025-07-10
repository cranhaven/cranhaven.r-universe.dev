#### Functions to validate user input ####

# swaps the order of vertex labels
swap_labels <- function(x) {
  if (x[2] < x[1]) {
    return(c(x[2], x[1]))
  } else{
    return(x)
  }
}

# enforce upper triangular adjacency matrix 
# structure for the edgelist
enforce_upper_triangular <- function(el) {
  t(apply(X = el, MARGIN = 1, FUN = swap_labels))
}

# checks that edgelist represents a simple graph
check_simple_df <- function(el) {
    has_dupes <- apply(X=el, MARGIN=1, FUN=function(x){x[1]==x[2]})
    if (sum(has_dupes) > 0){
        stop("Self-loops are not allowed.")
    }
}

# checks that edgelist contains no multiedges
check_duplicates <- function(el) {
  if (sum(duplicated(el)) > 0) {
    stop("Duplicated / multi-edges are not allowed.")
  }
}

# checks dimension of edgelist
check_df_dims <- function(el) {
    if (ncol(el) != 3) {
        stop("Array representing edgelist must have 3 columns")
    }
}


format_and_check_dataframe <- function(x) {
    check_df_dims(x)
    x[, c(1, 2)] <- enforce_upper_triangular(x[, c(1, 2)])
    check_duplicates(x)
    check_simple_df(x)
    return(x)
}


# check_simple <- function(g) {
#   if (!igraph::is.simple(g)) {
#     stop("Graph must be simple")
#   }
# }

# check_undirected <- function(g) {
#   if (igraph::is.directed(g)) {
#     stop("Graph must be undirected")
#   }
# }

# check_name_attribute <- function(g) {
#     if (!is.null(igraph::get.vertex.attribute(g, "name"))) {
#         warning("Vertex names will be converted to interger indices.")
#     }
# }

# check_weights_attribute <- function(g) {
#     if (is.null(igraph::get.edge.attribute(g, "weights"))) {
#         stop("Edge attribute 'weights' must be set")
#     }
# }

# check_igraph <- function(g) {
#     check_undirected(g)
#     check_simple(g)
#     check_name_attribute(g)
#     check_weights_attribute(g)
# }