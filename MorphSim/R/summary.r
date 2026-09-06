#' Calculates statistics for a morpho object
#'
#' @description
#' Computes three key pieces of information for a morpho object:
#' 1. The Consistency Index (CI) and Retention Index (RI) based on the tip sequence data.
#' 2. Convergent traits, identifying traits that have evolved independently multiple times.
#' 3. Summary information about the size and structure of the tree.
#'
#' @param data A morpho object
#' @return A list with three elements:
#'   - \code{Statistics}: data.frame with CI and RI
#'   - \code{Convergent_Traits}: data.frame listing convergent traits
#'   - \code{Tree}: data.frame summarizing extant/extinct tips and sampled ancestors
#' @importFrom phangorn phyDat CI RI
#' @importFrom ape node.depth.edgelength
#'
#' @export
#'
#' @examples
#' data(morpho_data)
#'
#' summary <- stats.morpho(data = morpho_data)
#'
#' @seealso \code{\link{get.convergent}} for detailed per-trait
#' convergence information
stats.morpho <- function(data){

  if (is.null(data) || !inherits(data, "morpho")) {
    stop("Error: `data` must be a morpho object.")
  }

  morpho_summary <- vector("list", 3)
  names(morpho_summary) <- c("Statistics", "Convergent_Traits", "Tree")

  ## consistency index & retention index
  phylev <- unique(unlist(unique(data$sequences$tips)))
  tree <- data$trees$EvolTree
  data_ph <-  phangorn::phyDat(data$sequences$tips, type="USER", levels=phylev, return.index = TRUE)

  morpho_summary[["Statistics"]] <- data.frame(
    CI =  phangorn::CI(tree, data_ph),
    RI = phangorn::RI(tree, data_ph)
  )

  ## convergent characters
  morpho_summary[["Convergent_Traits"]] <- convergent_evol(data = data)

  ## number of sampled ancestors
  if (!is.null(data$sequences$SA)){
    num_sa <- length(data$sequences$SA)
  } else {
    num_sa <- NA
  }

  ## number of extant tips
  if(!is.null(data$trees$TimeTree)){
    tip_depths <- ape::node.depth.edgelength(data$trees$TimeTree)[1:length(data$trees$TimeTree$tip.label)]
    tree_height <- max(ape::node.depth.edgelength(data$trees$TimeTree))
    extant_tips <- length(data$trees$TimeTree$tip.label[abs(tip_depths - tree_height) < 1e-8])

    ## number extinct tips
    extinct_tips <- length(data$trees$TimeTree$tip.label) - extant_tips

  } else {
    extant_tips <- NA
    extinct_tips <- NA
  }

  morpho_summary[["Tree"]] <- data.frame(
    Extant =  extant_tips,
    Extinct = extinct_tips,
    Sampled_Ancestors = num_sa
  )

  return(morpho_summary)
}


#' Determines the number of convergently evolved traits
#'
#' @description
#' Identifies which traits have evolved independently multiple times
#' (convergent evolution) in a morpho object.
#'
#' @param data A morpho object
#' @return A data.frame listing convergent traits, their state, and number of transitions
#'
#' @examples
#' phy <- ape::rtree(10)
#' morpho_data <- sim.morpho(tree = phy, k = 2, trait.num = 20)
#' convergent_evol(morpho_data)
#'
#' @export
convergent_evol <- function(data = NULL) {

  if (is.null(data) || !inherits(data, "morpho")) {
    stop("Error: `data` must be a morpho object.")
  }

  dat <- data[["transition_history"]]
  tree <- data$trees$EvolTree
  tip_states <- as.data.frame(data$sequences$tips)
  ntax <- length(tree$tip.label)

  convergent_traits <- matrix(ncol = 3)
  colnames(convergent_traits) <- c("trait", "state", "num.transitions")

  for (convT in seq_along(dat)) {
    temp <- dat[[convT]]
    trait_n <- tip_states[convT, ]

    # Skip if there are no transitions
    if (length(temp$edge) == 0) next

    # Prepare transition matrix
    trans_trait <- matrix(nrow = ntax, ncol = 2)
    rownames(trans_trait) <- tree$tip.label
    colnames(trans_trait) <- c("transition", "state")

    # Number transitions
    temp$num <- seq_along(temp$edge)

    for (tip in seq_along(tree$tip.label)) {
      route_n <- find_path_to_tip(tree, tree$tip.label[tip])

      for (l in seq(nrow(route_n), 1)) {
        bran <- which(tree$edge[, 1] == route_n[l, "parent"] &
                        tree$edge[, 2] == route_n[l, "child"])

        if (bran %in% temp$edge) {
          temp_sub <- temp[temp[["edge"]] == bran, ]
          max_tran <- max(temp_sub$hmin)

          trans_trait[tree$tip.label[tip], "transition"] <-
            temp_sub$num[temp_sub$hmin == max_tran]

          trans_trait[tree$tip.label[tip], "state"] <-
            temp_sub$state[temp_sub$hmin == max_tran]

          break
        }
      }
    }

    # Fill unchanged tips with root state
    rows_na <- apply(trans_trait, 1, function(row) any(is.na(row)))
    if (any(rows_na)) {
      trans_trait[rows_na, ] <- matrix(
        rep(c(0, data$root.state[convT]), sum(rows_na)),
        nrow = sum(rows_na),
        byrow = TRUE
      )
    }

    # Identify convergent traits
    states <- unique(trans_trait[, "state"])
    for (s in states) {
      by_state <- trans_trait[trans_trait[, "state"] == s, "transition"]
      if (length(unique(by_state)) > 1) {
        n <- as.numeric(c(convT, s, length(unique(by_state))))
        convergent_traits <- rbind(convergent_traits, n)
      }
    }
  }

  # Clean up output
  convergent_traits <- convergent_traits[-1, , drop = FALSE]
  if (nrow(convergent_traits) > 0) {
    convergent_traits <- as.data.frame(convergent_traits)
    rownames(convergent_traits) <- seq_len(nrow(convergent_traits))
  }

  return(convergent_traits)
}


#' Determines the route (nodes and branches) for a tip in a phylogenetic tree
#'
#' @description
#' Traverses the tree to determine the evolutionary path (branches) from root to a given tip.
#'
#' @param tree A phylogenetic tree of class \code{phylo}
#' @param tip Tip label (character)
#' @return A matrix with columns \code{parent} and \code{child} representing the path
#' @importFrom ape Ntip
find_path_to_tip <- function(tree, tip) {
  # Ensure the tip is valid
  if (!tip %in% tree$tip.label) {
    stop("The specified tip is not found in the tree.")
  }

  # Get the edge matrix and root node
  edge <- tree$edge
  root <- ape::Ntip(tree) + 1

  # the node number of the tip
  tip_node <- which(tree$tip.label == tip)

  # Start from the tip node and move upward toward the root
  path <- tip_node
  current_node <- tip_node

  # loop until reaching the root node
  while (current_node != root) {
    # find the parent of the current node
    parent_node <- edge[edge[, 2] == current_node, 1]
    path <- c(parent_node, path)
    current_node <- parent_node
  }
  length(path)-1
  output <- matrix(nrow =length(path)-1, ncol=2)
  colnames(output) <- c("parent", "child")

  for (i in 1:length(path)-1){
    output[i,] <- c(path[i], path[i+1])
  }

  return(output)
}


#' Combine Two Morpho Objects
#'
#' This function merges two `morpho` objects, combining their sequences,
#' model parameters, and transition histories, while ensuring tree and fossil
#' consistency.
#'
#' @param x A `morpho` object.
#' @param y A `morpho` object.
#'
#' @return A combined `morpho` object.
#'
#' @importFrom phangorn RF.dist
#' @export
#'
#' @examples
#' phy <- ape::rtree(10)
#'
#' # simulate characters along the branches of the tree
#' morpho1 <-  sim.morpho(tree = phy,
#'                            k = c(2,3,4),
#'                            trait.num = 20,
#'                            ancestral = TRUE,
#'                            partition = c(10,5,5),
#'                            ACRV = "gamma",
#'                            variable = TRUE,
#'                            ACRV.ncats = 4,
#'                            define.Q = NULL)
#'
#' morpho2 <-  sim.morpho(tree = phy,
#'                            k = c(2,3,4),
#'                            trait.num = 20,
#'                            ancestral = TRUE,
#'                            partition = c(10,5,5),
#'                            ACRV = "gamma",
#'                            variable = TRUE,
#'                            ACRV.ncats = 4,
#'                            define.Q = NULL)
#'
#' combined <- combine.morpho(morpho1, morpho2)
combine.morpho <- function(x, y) {

  if (!inherits(x, "morpho")) stop("Error: x must be a 'morpho' object")
  if (!inherits(y, "morpho")) stop("Error: y must be a 'morpho' object")
  if (!identical(x$fossil, y$fossil)) stop("Error: morpho objects have different fossil objects")
  if (!phangorn::RF.dist(x$trees$EvolTree,
                         y$trees$EvolTree) == 0) stop("Error: morpho objects have different trees")
  if (!identical(x$trees$BrRates, y$trees$BrRate)) stop("Error: morpho objects have different branch lengths")

  combined_tips <- list()
  tip_names <- names(x$sequences$tips)
  for (i in seq_along(tip_names)) {
    combined_tips[[tip_names[i]]] <- c(unname(x$sequences$tips[[i]]),
                                       unname(y$sequences$tips[[i]]))
  }

  combined_nodes <- list()
  nodes_names <- names(x$sequences$nodes)
  for (i in seq_along(nodes_names)) {
    combined_nodes[[nodes_names[i]]] <- c(unname(x$sequences$nodes[[i]]),
                                          unname(y$sequences$nodes[[i]]))
  }

  combined_SA <- list()
  sa_names <- names(x$sequences$SA)
  for (i in seq_along(sa_names)) {
    combined_SA[[sa_names[i]]] <- c(unname(x$sequences$SA[[i]]),
                                    unname(y$sequences$SA[[i]]))
  }

  out <- list(
    sequences = list(
      tips  = combined_tips,
      nodes = combined_nodes,
      SA    = combined_SA
    ),
    trees = list(
      EvolTree = x$trees$EvolTree,
      TimeTree = x$trees$TimeTree,
      BrRates  = x$trees$BrRates
    ),
    model = list(
      Specified    = c(x$model$Specified, y$model$Specified),
      RateVar      = rbind(x$model$RateVar, y$model$RateVar),
      RateVarTrait = rbind(x$model$RateVarTrait, y$model$RateVarTrait)
    ),
    transition_history = c(x$transition_history, y$transition_history),
    root.states        = c(x$root.states, y$root.states),
    fossil             = x$fossil,
    combined = TRUE
  )

  class(out) <- class(x)
  return(out)
}


#' Add reconstructed tree and matrix to morpho object
#'
#' @description This function should be called if you want to access the
#' reconstructed tree and character matrix directly from the morpho object.
#' It adds the reconstructed tree in Newick format ($trees$Recon), containing
#' only the sampled lineages, and a character matrix ($sequences$recon)
#' containing only the taxa present in that reconstructed tree.
#'
#' @param data `morpho object` containing a fossil object
#'
#' @return a `morpho object`
#'
#' @export
#' @examples
#' # simulate tree
#' lambda = 0.1
#' mu = 0.05
#' tips = 10
#' t = TreeSim::sim.bd.taxa(n = tips, numbsim = 1, lambda = lambda, mu = mu)[[1]]
#'
#' # Simulate fossils and extant taxa
#' rate = 0.1 # poisson sampling rate
#' f = FossilSim::sim.fossils.poisson(rate = rate, tree = t, root.edge = FALSE)
#' rho = 0.5
#' f2 = FossilSim::sim.extant.samples(fossils = f, tree = t, rho = rho)

#' morpho_data <-  sim.morpho(k = c(2,3),
#'                            time.tree = t,
#'                            trait.num = 6,
#'                            ancestral = TRUE,
#'                            br.rates = 0.1,
#'                            partition = c(4,2),
#'                            ACRV = "gamma",
#'                            variable = TRUE,
#'                            ACRV.ncats = 4,
#'                            fossil = f2)
#'
#' re <- get.reconstructed(morpho_data)
#'
get.reconstructed <- function(data) {
  if (!is.morpho(data)) stop ("Error: must provide a morpho object")
  if (is.null(data$fossil)) stop ("Error: must provide a morpho object with fossil information")

  r_tree <- FossilSim::reconstructed.tree.fossils.objects(fossils  = data$fossil,
                                                        tree = data$trees$TimeTree,
                                                        tip_order = "youngest_first")
  recon <- reconstruct.matrix(data)

  data$sequences[["recon"]] <- recon
  data$trees[["Recon"]] <- r_tree

  return(data)
}


#' Identify convergent traits
#'
#' @description
#' Identifies convergent evolution in a morpho object. When called without a
#' trait, returns a summary of which traits are convergent. When called with a
#' specific trait, returns tip groupings by state and evolutionary origin.
#'
#' @param data A morpho object
#' @param trait The trait number to examine. If \code{NULL} (default), returns a summary.
#'
#' @return A data frame. Without \code{trait}: trait index and number of convergent states.
#'   With \code{trait}: state, origin, tips, and whether the state is convergent.
#' @export
#'
#' @examples
#' phy <- ape::rtree(10)
#' morpho_data <- sim.morpho(tree = phy, k = 2, trait.num = 20)
#' get.convergent(morpho_data)
#' get.convergent(morpho_data, trait = 3)
#'

get.convergent <- function(data, trait = NULL) {
  if (!is.morpho(data)) stop("Error: data must be a morpho object")

  # Summary mode: return which traits are convergent
  if (is.null(trait)) {
    conv <- convergent_evol(data)
    if (nrow(conv) == 0) return(data.frame(trait = integer(0),
                                           num.convergent.states = integer(0)))
    summary_df <- aggregate(state ~ trait, data = conv, FUN = length)
    colnames(summary_df) <- c("trait", "num.convergent.states")
    return(summary_df)
  }

  # Detail mode: group tips by state and origin for a specific trait
  tree <- data$trees$EvolTree
  tip_states <- sapply(data$sequences$tips, function(x) x[trait])
  trans <- data$transition_history[[trait]]
  root_state <- data$root.states[trait]

  origin_labels <- character(length(tip_states))
  names(origin_labels) <- names(tip_states)

  for (tip in names(tip_states)) {
    path <- find_path_to_tip(tree, tip)
    origin <- "root"

    for (l in seq(nrow(path), 1)) {
      bran <- which(tree$edge[, 1] == path[l, "parent"] &
                      tree$edge[, 2] == path[l, "child"])
      if (nrow(trans) > 0 && bran %in% trans$edge) {
        matches <- trans[trans$edge == bran, ]
        last_change <- matches[which.max(matches$hmin), ]
        origin <- paste0("edge_", last_change$edge, "_t_", round(last_change$hmin, 4))
        break
      }
    }
    origin_labels[tip] <- origin
  }

  result <- data.frame(
    tip = names(tip_states),
    state = tip_states,
    origin = origin_labels,
    row.names = NULL
  )

  summary <- aggregate(tip ~ state + origin, data = result,
                       FUN = function(x) paste(x, collapse = ", "))

  # flag convergent states
  origins_per_state <- table(summary$state)
  convergent_states <- names(origins_per_state[origins_per_state > 1])

  summary$convergent <- summary$state %in% convergent_states
  summary <- summary[order(summary$state, summary$origin), ]

  return(summary)
}


#' Get Tranisition History
#'
#' @description
#' Returns the full transition history for a given trait,
#' including the root state.
#'
#' @param data A morpho object
#' @param trait The trait number .
#'
#' @return A data frame with columns edge (branch number), state, and
#' hmin (where along the branch the transition occurred)
#'
#' @export
#' @examples
#' phy <- ape::rtree(10)
#' morpho_data <- sim.morpho(tree = phy, k = 2, trait.num = 20)
#' get.transitions(morpho_data, trait = 2)
#'
get.transitions <- function(data, trait) {
  if (!is.morpho(data)) stop("Error: data must be a morpho object")
  if (trait > length(data$transition_history)) {
    stop(paste0("Error: trait ", trait, " is out of range. Object has ",
                length(data$transition_history), " traits."))
  }

  root_row <- data.frame(edge = 0, state = as.numeric(data$root.states[trait]), hmin = 0)
  tr <- data$transition_history[[trait]]

  rbind(root_row, tr)
}


#' Get Specific Morphological Matrix
#'
#' @description
#' Return data frame of morphological matrix
#'
#' @param data A morpho object
#' @param seq The sequence data you want to extract
#'
#' @return A data frame with columns edge (branch number), state, and
#' hmin (where along the branch the transition occurred)
#'
#' @export
#' @examples
#' phy <- ape::rtree(10)
#' morpho_data <- sim.morpho(tree = phy, k = 2, trait.num = 20)
#' get.matrix(morpho_data, seq = "tips")

get.matrix <- function(data, seq) {
  if (!is.morpho(data)) stop("Error: data must be a morpho object")
  if (!seq  %in% names(data$sequences)) {
    stop(paste0(seq, " not found in morpho object"))
  }

  s <- t(as.data.frame(data$sequences[[seq]]))
  return(s)

}





