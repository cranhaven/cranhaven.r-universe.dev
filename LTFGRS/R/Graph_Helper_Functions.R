#'
#' Add pseudo point to graph
#'
#' Internal function. Identifies the proband and makes a duplicated point that will act as genetic liability in kinship matrix
#'
#' @param fam_graph family graph center on proband
#' @param index_id id of proband.
#'
#' @return returns family graph with a pseudo point added that will have the same connections as the proband point. For kinship construction, the relationship to the proband must be adjusted to 1 \* h2 (and not 0.5 \* h2).
#' @noRd

add_gen_liab_to_graph = function(fam_graph, index_id) {
  # find all index edges
  all_edges = igraph::as_ids(igraph::E(fam_graph))
  # extract only edges that directly match the index_id.
  # edges are of the form str1|str2; to avoid matching the wrong substring,
  # we include string end and start as well as the divider "|" in the match

  # find edges that start with index_id
  startWith_edges = stringr::str_detect(all_edges, paste0("^", index_id, "\\|"))
  # find edges that end with index_id
  endWith_edges = stringr::str_detect(all_edges, paste0("\\|", index_id, "$"))

  index_edges = all_edges[startWith_edges | endWith_edges ]
  # subset edges to and from index with new id - here with "_g" added

  # we need to handle the no edges case, i.e. one point graphs.
  if (length(index_edges)  > 0) {
    gen_liab_edges = stringr::str_replace(index_edges, index_id, paste0(index_id, "_g")) %>%
      # format into vector with start and end of edge at positions 1 & 2, 3 & 4, etc
      # as defined by igraph.
      stringr::str_split("\\|") %>%
      # rbind is much faster than cbind
      do.call("rbind", .) %>%
      # transpose for 2xN
      t() %>%
      # c for column wise conversion to vector - each edge is a column
      c()
  }

  fam_graph %>%
    igraph::add.vertices(., nv = 1, name = paste0(index_id, "_g")) %>%
    # adding edges found above and edge between _g and index_id
    igraph::add.edges(., edges = c(if (length(index_edges) > 0) gen_liab_edges,
                                   paste0(rep(index_id, 4), c("", "_g", "_g", ""))))
}


#' Construct kinship matrix from graph
#'
#' construct the kinship matrix from a graph representation of a family, centered on an index person (proband).
#'
#'
#' @param fam_graph graph.
#' @param h2 heritability.
#' @param index_id proband id. Only used in conjuction with add_ind = TRUE.
#' @param add_ind add genetic liability to the kinship matrix. Defaults to true.
#' @param fix_diag Whether to set diagonal to 1 for all entries except for the
#' genetic liability.
#'
#' @return A kinship matrix.
#'
#' @examples
#' fam <- data.frame(
#' i = c(1, 2, 3, 4),
#' f = c(3, 0, 4, 0),
#' m = c(2, 0, 0, 0)
#' )
#'
#' thresholds <- data.frame(
#'   i = c(1, 2, 3, 4),
#'   lower = c(-Inf, -Inf, 0.8, 0.7),
#'   upper = c(0.8, 0.8, 0.8, 0.7)
#' )
#'
#' graph <- prepare_graph(fam, icol = "i", fcol = "f", mcol = "m", node_attributes = thresholds)
#'
#' get_covmat(graph, h2 = 0.5, index_id = "1")
#'
#' @export
#'

get_covmat = function(fam_graph, h2, index_id = NA, add_ind = TRUE, fix_diag = TRUE) {
  if (add_ind) {
    index_id_g = paste0(index_id, "_g")
    # adding in point to act as genetic liability for index person
    # need to adjust individual and genetic liab point's internal relationship later
    fam_graph = add_gen_liab_to_graph(fam_graph, index_id)

  }

  # in-distances
  ph = igraph::distances(fam_graph, mode = "in")
  rnames = rownames(ph)
  nrows = nrow(ph)

  # construct dummy matrix
  new_distances = matrix(NA, ncol = nrows, nrow = nrows)
  rownames(new_distances) = rnames
  colnames(new_distances) = rnames

  # distance to self is always 0
  diag(new_distances) = 0

  # fill dummy matrix
  for (i in 1:(nrows - 1)) {
    for (j in (i + 1):nrows) {
      new_distances[i,j] <- new_distances[j,i] <- min(ph[i, ] + ph[j, ])
    }
  }

  # calculating kinship matrix, _g needs adjustment
  kinship = 0.5^new_distances
  if (add_ind) {
    # multipying with h2 and fixing diagonal to 1 - except for gen liab
    kinship[index_id, index_id_g] <- kinship[index_id_g, index_id] <- kinship[index_id_g, index_id_g] <- 1
    kinship = kinship * h2
    if (fix_diag) { # fixing diagonal at 1, except the genetic component
      diag(kinship)[-which(rownames(kinship) == index_id_g)] <- 1
    }

  } else {
    kinship = kinship * h2

    if (fix_diag) diag(kinship) <- 1
  }

  return(kinship)
}


#'
#' construct all combinations of input vector
#'
#' pastes together all combinations of input vector
#'
#' @param vec vector of strings
#'
#' @return A vector of strings is returned.
#'
#' @examples
#' get_all_combs(letters[1:3])
#'
#' @export
get_all_combs = function(vec) {
  # outer: creates all combinations of entries in vec
  # then we select only the off diagonal entries
  outer(vec, vec, paste, sep = "_")[!diag(TRUE, nrow = length(vec))]
  # results in length(vec) * (length(vec) - 1) entries
  # since it is the number of entries in the upper *and* lower triangle
  # number of entries in either is given by length(vec) * (length(vec) - 1)/2
}



