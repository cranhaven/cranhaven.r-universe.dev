#' Simulate characters along branches in a tree
#'
#' @description
#' This function simulates discrete character data along the branches of a
#' phylogenetic tree using a continuous-time Markov chain. It implements the
#' Mk model and its extensions (Lewis 2001), including among-character rate
#' variation (ACRV) using discretized gamma distributions (Yang 1994) or
#' lognormal distributions (Wagner 2012), following the implementation
#' described in Capobianco and Hohna (2025). It can be used with either a
#' time tree or a tree with branch lengths in evolutionary distance. If using
#' a time tree, branch rates can be specified either as a single value for all
#' branches or as a vector with different rates per branch. If no branch rates
#' are specified a default of 0.1 is applied to all branches.
#'
#' @param tree A phylogenetic tree (class \code{"phylo"}) with branches
#'   representing genetic distance.
#' @param time.tree A phylogenetic tree (class \code{"phylo"}) with branches
#'   representing time.
#' @param k Integer (>= 2). Number of trait states. Can be a vector if using
#'   partitions.
#' @param trait.num Integer (> 0). The total number of traits to simulate.
#' @param partition Integer vector. Specifies the number of traits per
#'   partition. Must sum to \code{trait.num}.
#' @param br.rates Numeric. Clock rates per branch. Can be a single value
#'   (strict clock) or a numeric vector of length equal to the number of
#'   branches in the tree, one rate per branch in the same order as
#'   \code{tree$edge}. Custom vectors of rates can be provided directly here,
#'   for example rates simulated using \code{simclock::relaxed.tree()}.
#' @param root.state Integer. The root state at the start of the simulation.
#'   Must be within the range of states implied by \code{k}.
#' @param variable Logical. If \code{TRUE}, simulate only varying characters
#'   (MkV model). Default is \code{FALSE}.
#' @param full.states Logical. If \code{TRUE}, ensures that all character
#'   states specified by \code{k} are present in at least one tip for each
#'   trait. Default is \code{FALSE}.
#' @param parsimony Character. If \code{"standard"}, retain only characters
#'   where at least two states each occur in two or more taxa; autapomorphic
#'   states are tolerated. If \code{"strict"}, every observed state must occur
#'   in two or more taxa, excluding characters with any autapomorphic states
#'   entirely. If \code{NULL}, no parsimony filter is applied. Default is
#'   \code{NULL}.
#' @param ACRV Character. Among-character rate variation model. One of
#'   \code{"gamma"}, \code{"lgn"}, \code{"user"}, or \code{NULL}.
#'   \code{"gamma"} draws rates from a discretized gamma distribution (Yang
#'   1994); requires \code{ACRV.ncats} and \code{alpha.gamma}.
#'   \code{"lgn"} draws rates from a discretized lognormal distribution
#'   (Wagner 2012); requires \code{ACRV.ncats}, \code{meanlog}, and
#'   \code{sdlog}. \code{"user"} uses user-supplied rates via
#'   \code{define.ACRV.rates}. \code{NULL} simulates all traits under the
#'   same rate. Default is \code{NULL}.
#' @param alpha.gamma Numeric. Shape parameter \eqn{\alpha} for the gamma
#'   distribution. Default is 1.
#' @param ACRV.ncats Integer. Number of discrete rate categories for
#'   among-character rate variation.
#' @param meanlog Numeric. Mean of the lognormal distribution on the log
#'   scale. Required when \code{ACRV = "lgn"}.
#' @param sdlog Numeric. Standard deviation of the lognormal distribution on
#'   the log scale. Required when \code{ACRV = "lgn"}.
#' @param define.ACRV.rates Numeric vector. User-supplied rate categories.
#'   Required when \code{ACRV = "user"}.
#' @param ancestral Logical. If \code{TRUE}, return the states at all
#'   ancestral nodes. Default is \code{TRUE}.
#' @param fossil A fossil object from \code{FossilSim} to simulate
#'   morphological characters for sampled ancestors.
#' @param define.Q Numeric matrix. A custom transition rate matrix Q for
#'   simulation. Must be square and rows must sum to zero.
#'
#' @return An object of class `morpho`, with the following components:
#' \describe{
#'   \item{sequences}{A list containing up to 3 elements: morphological data for the `tips`
#'   of the tree, the `nodes`, and, if provided, the sampled ancestors (`SA`). For `SA`,
#'   the naming scheme differs from that of `FossilSim`: the morphological data are named
#'   using the specimen number (`data$fossil$specimen`) and the branch number along which
#'   the fossil was sampled.}
#'
#'   \item{tree}{A list containing up to 3 elements: the `EvolTree` (branch lengths in
#'   genetic distance), the `TimeTree` (branch lengths in time units), and `BrRates`
#'   (the evolutionary rate per branch).}
#'
#'   \item{model}{Information about the model used to simulate the data. `Specified`
#'   states the exact model used per partition, as well as the number of traits and
#'   character states respectively. `RateVar` contains the relative rates used to simulate
#'   the data, and `RateVarTrait` contains information about which rate category was used
#'   to simulate each trait. These values are listed from lowest rate (1) to highest.}
#'
#'   \item{transition_history}{A list containing *n* data frames, where *n* is the number
#'   of simulated traits. Each data frame contains information about transitions that
#'   occurred for that trait, including the branch number (`edge`), the new state number (`state`),
#'   and the point along the branch where the transition occurred (`hmin`).}
#'
#'   \item{root.states}{A vector of root states for each trait.}
#'
#'   \item{fossil}{The fossil object provided to `morphsim` from `FossilSim`. The naming
#'   scheme therefore matches that of `FossilSim`.}
#' }
#' @references
#' Lewis, P.O. (2001) A likelihood approach to estimating phylogeny from
#' discrete morphological character data. \emph{Systematic Biology} 50:913-925.
#'
#' Yang, Z. (1994) Maximum likelihood phylogenetic estimation from DNA
#' sequences with variable rates over sites: approximate methods.
#' \emph{Journal of Molecular Evolution} 39:306-314.
#'
#' Wagner, P.J. (2012) Modelling rate distributions using character
#' compatibility: implications for morphological evolution among fossil
#' invertebrates. \emph{Biology Letters} 8:143-146.
#'
#' Capobianco, A. and Hohna, S. (2025) On the MkV model with
#' among-character rate variation. \emph{Systematic Biology}.
#'
#' @importFrom ape Ntip
#'
#' @export

#' @examples
#'# simulated tree
#' phy <- ape::rtree(10)
#'
#'# simulate characters along the branches of the tree
#' morpho_data <-  sim.morpho(tree = phy,
#'                            k = c(2,3,4),
#'                            trait.num = 20,
#'                            partition = c(10,5,5),
#'                            ACRV = "gamma",
#'                            ACRV.ncats = 4,
#'                            variable = TRUE,
#'                            ancestral = TRUE,
#'                            define.Q = NULL)
#'
#' # To simulate ordered characters:
#' # First define a Q-matrix. The following is for ordered characters where transitions can only occur
#' # between states 0 and 1 and 1 and 2
#'
#' ord_Q <- matrix(c(
#' -0.5, 0.5, 0.0,
#' 0.3333333, -0.6666667, 0.3333333,
#' 0.0, 0.5, -0.5
#' ), nrow = 3, byrow = TRUE)
#'
#' # This Q matrix can be then used to simulate character data.
#'
#' morpho_data <- sim.morpho(tree = phy,
#'                           k = 3,
#'                           trait.num = 20,
#'                           ancestral = TRUE,
#'                           ACRV = "gamma",
#'                           variable = TRUE,
#'                           ACRV.ncats = 4,
#'                          define.Q = ord_Q)



sim.morpho <- function(tree = NULL,
                       time.tree = NULL,
                       k = 2,
                       trait.num,
                       partition = NULL,
                       br.rates = NULL,
                       root.state = NULL,
                       variable = FALSE,
                       full.states = FALSE,
                       parsimony = NULL,
                       ACRV = NULL,
                       alpha.gamma = 1,
                       ACRV.ncats = 4,
                       meanlog = NULL,
                       sdlog = NULL,
                       define.ACRV.rates = NULL,
                       ancestral = TRUE,
                       fossil = NULL,
                       define.Q = NULL) {

  if (is.null(tree) && is.null(time.tree)) stop("Error: Must provide a tree object")
  if (any(k < 2)) stop("Error: Need to simulate at least 2 states")
  if (is.null(trait.num)) stop("Error: Specify the total number of traits to simulate")

  if (!is.null(ACRV) && ACRV == "user" && is.null(define.ACRV.rates)) {
    stop("Error: Need to provide a vector of rates if ACRV is set to 'user'")
  }

  if (is.null(partition) && length(k) != 1) {
    stop("Error: Data being simulated under 1 partition, supply 1 character state for k")
  }

  if (!is.null(partition) && length(k) != length(partition)) {
    stop("Error: Need to specify maximum character state for each partition")
  }

  if (!is.null(partition) && sum(partition) != trait.num) {
    stop("Error: The total number characters in partitions and trait.num must match")
  }

  if (!is.null(define.Q)) {
    if (!is.matrix(define.Q)) stop("Error: `define.Q` must be a matrix.")
    if (nrow(define.Q) != ncol(define.Q)) stop("Error: `define.Q` must be square.")
    if (any(abs(rowSums(define.Q)) > 1e-6)) {
      stop("Error: Incorrect Q matrix specified: rows must sum to zero.")
    }
  }

  if (!is.logical(variable)) stop("Error: `variable` must be TRUE or FALSE.")
  if (!is.logical(ancestral)) stop("Error: `ancestral` must be TRUE or FALSE.")
  if (!is.null(fossil) && !FossilSim::is.fossils(fossil)) {
    stop("Error: `fossil` must be a FossilSim object.")
  }


  if (!is.null(root.state) && root.state > min(k) -1){
    stop("Root state specified not incluced in state matrix specified for simulation")
  }

  if (!is.null(parsimony) && !parsimony %in% c("standard", "strict")) {
    stop("Error: `parsimony` must be NULL, 'standard', or 'strict'")
  }


  ## if provided with time tree, need to transform branches in genetic distance
  if (is.null(tree) && !is.null(time.tree)) {
    tree <- time.tree
    if (is.null(br.rates)) {
      print("Warning: No branch rate provided, using default of 0.1 for all branches")
      br.rates <- rep(0.1, length(tree$edge.length))
    }
    if (length(br.rates) == 1) {
      br.rates <- rep(br.rates, length(tree$edge.length))
    }
    tree$edge.length <- time.tree$edge.length * br.rates
  }

  tree.ordered <- reorder(tree)

  if (!is.null(time.tree)) {
    time.tree.order <- reorder(time.tree)
  } else {
    time.tree.order <- NULL
  }


  # counters and setup
  tr.num <- 1
  edge <- tree.ordered$edge
  num.nodes <- max(edge)
  num.tips <- ape::Ntip(tree.ordered)
  rs <- c()

  # define the node labels
  parent <- as.integer(edge[, 1])
  child <- as.integer(edge[, 2])
  tips <- setdiff(child, parent)
  nodes <-unique(parent)

  # create an result matrix
  transition_history <- list()
  state_at_tips <- matrix(nrow = num.tips, ncol = trait.num)
  rownames(state_at_tips) <- tree.ordered$tip.label
  state_at_nodes <- matrix(nrow = tree$Nnode, ncol = trait.num)
  rownames(state_at_nodes) <- nodes
  ACRV_rate <- matrix(ncol = trait.num, nrow = 1)

  # identify the root
  root <- as.integer(parent[!match(parent, child, 0)][1])
  if (is.null(partition)) partition <- trait.num

  ### Among character rate variation
  if (!is.null(ACRV)) {
    if (ACRV == "gamma") {
      disc_rates <- get_gamma_rates(alpha.gamma, ACRV.ncats)
    } else if(ACRV == "user"){
      disc_rates <- define.ACRV.rates
      } else if (ACRV == "lgn") {
      disc_rates <- get_lognormal_rates(meanlog, sdlog, ACRV.ncats)
    }
  }

  ## start the partition loop
  for (part in 1:length(partition)) {
    part.trait.num <- partition[part]
    part_k <- k[part]

    if (is.null(define.Q)) {
      Q <- symmetric.Q.matrix(part_k)
    } else {
      Q <- define.Q
    }

    states <- as.character(c(0:(part_k - 1)))
    bl <- tree.ordered$edge.length

    # start of simulation loop
    for (tr in 1:part.trait.num) {
      iter <- 0
      repeat {
        iter <- iter + 1
        if (iter > 1e6) {
          stop("sim.morpho: character ", tr, " has not converged after 1e6 iterations. ",
                  "Consider reducing k, increasing branch the rates, or relaxing the full.states or parsimony conditions.")
          }

        if (is.null(root.state)){
        root.state <- sample(states, 1, replace = TRUE, prob = rep(1 / part_k, part_k))
        } else {
          root.state <- root.state
        }
        state_at_nodes[as.character(root), tr.num] <- as.numeric(root.state)

        if (!is.null(ACRV)) {
            trait_rate <- disc_rates[sample(1:ACRV.ncats, 1)]
            Q_r <- Q * trait_rate
             } else {
          Q_r <- Q
        }

        # container for the transitions
        transitions <- matrix(ncol = 3, nrow = 0)
        colnames(transitions) <- c("edge", "state", "hmin")

        # loop through all branches
        for (i in seq_along(bl)) {
          total_wait <- 0
          from <- parent[i]
          to <- child[i]
          current_state <- as.numeric(unname(state_at_nodes[as.character(from), tr.num]))
           # Time tracking
          time_remaining <- bl[i]

          while (time_remaining > 0) {
            # Get the rates for the current state
            rates <- Q_r[current_state + 1, ]
            rates[current_state + 1] <- 0
            # calculate the rate of any transition occurring
            total_rate <- -Q_r[current_state + 1, current_state + 1]
            waiting_time <- rexp(1, rate = total_rate)
            time_remaining <- time_remaining - waiting_time
            if (time_remaining <= 0) break

            # Transition to a new state
            next_state <- sample(states, 1, prob = rates / total_rate)
            current_state <- as.numeric(next_state)
            add_t <- c(i, next_state, waiting_time + total_wait)
            transitions <- rbind(transitions, as.numeric(add_t))
            total_wait <- total_wait + waiting_time
          }

          if (to %in% tips) state_at_tips[tree$tip.label[to], tr.num] <- current_state
          if (to %in% nodes) state_at_nodes[as.character(to), tr.num] <- current_state
        }

        # filtering conditions
        if (!variable && !full.states && is.null(parsimony)) break
        vari <- !variable || (length(unique(state_at_tips[, tr.num])) > 1 & length(unique(state_at_nodes[, tr.num])) > 1)
        has_max_state <- !full.states || all(states %in% state_at_tips[, tr.num])
        pars <- if (is.null(parsimony)) {
          TRUE
        } else if (parsimony == "standard") {
          # Literal parsimony informative from paup, TNT
          # At least two states each appear in >= 2 taxa (allows autapomorphies)
          sum(table(state_at_tips[, tr.num]) >= 2) >= 2
        } else if (parsimony == "strict") {
          # Strictly parsimony-informative: no autapomorphies permitted.
          # Every observed state must occur in >= 2 taxa.
          all(table(state_at_tips[, tr.num]) >= 2)
        }
        if (vari && has_max_state && pars) break
          } # end of simulation loop

      if (!is.null(ACRV)) {
         ACRV_rate[tr.num] <- which(disc_rates == trait_rate)
      }

      transition_history[[tr.num]] <- as.data.frame(transitions)
      tr.num <- tr.num + 1
      rs <- rbind(rs, root.state)
    }
  }

  ### fossil object
  if (!is.null(fossil)) {
    f.morpho <- fossil
    f.morpho$ape.branch <- NA
    f.morpho$specimen <- seq(1, length(f.morpho$sp))

    state_at_fossils <- matrix(nrow = length(f.morpho$sp), ncol = trait.num)
    rownames(state_at_fossils) <- f.morpho$specimen

    for (i in 1:length(f.morpho$edge)) {
      child <- f.morpho$edge[i]
      f.morpho$ape.branch[i] <- which(tree.ordered$edge[, 2] == child)
    }

    tree.age <- max(ape::node.depth.edgelength(time.tree))

    for (tr.num in 1:trait.num) {
      for (spec in 1:length(f.morpho$specimen)) {
        branch <- f.morpho$ape.branch[[spec]]
        parent <- tree.ordered$edge[branch, 1]
        current_state <- state_at_nodes[[which(rownames(state_at_nodes) == parent), tr.num]]

        if (!(f.morpho$ape.branch[spec] %in% transition_history[[tr.num]][[1]])) {
          state_at_fossils[spec, tr.num] <- current_state
        } else {
          time_position_fossil <- tree.age - f.morpho$hmin[spec] + time.tree$root.edge
          changes <- which(transition_history[[tr.num]][[1]] == f.morpho$ape.branch[spec])
          changes_along_edge <- transition_history[[tr.num]][changes, ]
          node_age <- ape::node.depth.edgelength(time.tree)[time.tree$edge[branch, 1]]
          changes_along_edge$time <- (changes_along_edge[, 3] / br.rates[branch]) + node_age + time.tree$root.edge

          later <- which(changes_along_edge[, 4] > time_position_fossil)
          if (!length(later) == 0) changes_along_edge <- changes_along_edge[-later, ]

          if (length(changes_along_edge[, 1]) == 0) {
            state_at_fossils[spec, tr.num] <- current_state
          } else {
            tran <- which(changes_along_edge$hmin == max(changes_along_edge$hmin))
            state_at_fossils[spec, tr.num] <- changes_along_edge[tran, 2]
          }
        }
      }
    }

    fossil_names <- paste0(f.morpho$specimen, "_", f.morpho$ape.branch)
    fossil_sequence <- list()
    rownames(state_at_fossils) <- fossil_names
    for (i in 1:length(fossil_names)) {
      fossil_sequence[[fossil_names[i]]] <- state_at_fossils[fossil_names[i], ]
    }
  }

  ## create morpho object
  seq <- list(NA, NA, NA)
  names(seq) <- c("tips", "nodes", "SA")
  tip_names <- rownames(state_at_tips)
  tip_sequence <- list()

  for (i in 1:length(tip_names)) {
    tip_sequence[[tip_names[i]]] <- state_at_tips[tip_names[i], ]
  }
  seq[["tips"]] <- tip_sequence

  if (ancestral) {
    node_names <- rownames(state_at_nodes)
    node_sequence <- list()
    for (i in 1:length(node_names)) {
      node_sequence[[node_names[i]]] <- state_at_nodes[node_names[i], ]
    }
  } else {
    node_sequence <- NULL
  }
  seq[["nodes"]] <- node_sequence

  if (is.null(fossil)) {
    fossil_sequence <- NULL
    f.morpho <- NULL
  }
  seq[["SA"]] <- fossil_sequence

  if (is.null(ACRV)) ACRV_rate <- NULL


  if (exists("disc_rates")) {
    discrete_rates <- disc_rates
  } else {
    discrete_rates <- NULL
  }

  if (isTRUE(variable)) {
    var <- "V"
  } else {
    var <- NULL
  }
  model_components <- paste0("Mk", ACRV, var, "_Part:", partition, "states:", k)

  trees <- list(NA, NA, NA)
  names(trees) <- c("EvolTree", "TimeTree", "BrRates")
  trees[["EvolTree"]] <- tree.ordered
  trees[["TimeTree"]] <- time.tree
  trees[["BrRates"]] <- br.rates

  model <- list(NA, NA, NA)
  names(model) <- c("Specified", "RateVar", "RateVarTrait")
  model[["Specified"]] <- model_components
  model[["RateVar"]] <- discrete_rates
  model[["RateVarTrait"]] <- ACRV_rate

  sim.output <- as.morpho(sequences = seq,
                          trees = trees,
                          model = model,
                          transition_history = transition_history,
                          root.states = rs,
                          fossil = f.morpho)

  return(sim.output)
}




