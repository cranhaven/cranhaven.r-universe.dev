#' Null distribution for Cognitive Imaginary Structures
#' 
#' These functions can be used to generate null distributions for testing the
#' prevalence of imaginary CSS. The null is a function of the individual level
#' accuracy rates, in other words. Pr(i perceives a one| there is a one) and
#' Pr(i perceives a zero| there is a zero).
#' 
#' @param graph A barry_graph object.
#' @param which_nets Integer vector. The networks to sample from.
#' @details
#' There are two special cases worth mentioning. First, when the dyads in 
#' question are all present the probability of true negative is set to `NA`.
#' On the other hand, if the dyads in question are all null, the probability of
#' true positive is `NA` as well. This doesn't affect the `sample_css_network`
#' function because those probabilities are unsed since tie/no tie probabilities
#' are according to the baseline graph, meaning that, for instance, a fully
#' connected network will never use the `p_0_ego` and `p_0_alter`
#' probabilities and an empty network will never use the `p_1_ego` and
#' `p_1_alter` probabilities.
#' 
#' @return
#' The function `tie_level_accuracy` returns a data frame with the following columns:
#' \itemize{
#'  \item \code{k}: The perceiver id.
#'  \item \code{p_0_ego}: The probability of no tie between the perceiver (ego) and an alter.
#'  \item \code{p_1_ego}: The probability of a tie between the perceiver and an alter.
#'  \item \code{p_0_alter}: The probability of no tie between two alters.
#'  \item \code{p_1_alter}: The probability of a tie between two alters.
#' }
#' @examples
#' # Using the Krackhardt advice network
#' data(krackhardt_advice)
#' data(krackhardt_advice_perceptions)
#'
#' n_people <- 21
#' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
#' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
#'   krackhardt_advice$value
#'
#' krack_graph <- new_barry_graph(
#'   c(list(advice_matrix), krackhardt_advice_perceptions)
#' )
#'
#' # Calculate accuracy rates
#' accuracy <- tie_level_accuracy(krack_graph)
#' print(accuracy)
#'
#' # Visualize accuracy patterns
#' boxplot(accuracy[, -1],
#'         main = "Accuracy Rates by Type",
#'         ylab = "Probability",
#'         names = c("P(0|0) Ego", "P(1|1) Ego",
#'                   "P(0|0) Alter", "P(1|1) Alter"))
#' 
#' @export 
tie_level_accuracy <- function(
    graph,
    which_nets = NULL
) {
  
  # Error if graph is not barry_graph
  stopifnot_barry_graph(graph)
  
  # Getting the edgelist
  elist <- barray_to_edgelist(graph)
  
  # And other graph attributes
  netsize   <- attr(graph, "netsize")
  endpoints <- attr(graph, "endpoints")
  nnets     <- length(endpoints)
  
  if (length(which_nets) == 0L)
    which_nets <- seq_len(nnets)
  else {
    if (any(which_nets < 1L) || any(which_nets > nnets))
      stop("The networks to sample from are not valid.", call. = FALSE)
  }
  
  # Generating the matrices
  # Since endpoints starts at 0, we need to correct
  endpoints <- endpoints + 1L
  from <- c(netsize + 1, endpoints)
  to   <- from + nnets
  
  
  # Generating the baseline graph
  elist0 <- elist[
    which((elist[, 1] <= netsize) & (elist[, 2] <= netsize)), ,
    drop = FALSE
  ]
  
  g_0 <- matrix(0L, nrow = netsize, ncol = netsize)
  g_0[elist0] <- 1L
  
  # NA diagonal
  diag(g_0) <- NA
  
  # Identifying 
  edgelist <- lapply(which_nets, function(i) {
    
    # Extracting edges
    eseq <- from[i]:to[i]
    
    e_i <- elist[
      which((elist[, 1] %in% eseq) & (elist[, 2] %in% eseq)),
      ,
      drop = FALSE] - from[i] + 1L
    
    # Creating the graph
    g_i <- matrix(0L, nrow = netsize, ncol = netsize)
    g_i[e_i] <- 1L
    
    # NA diagonal
    diag(g_i) <- NA
    
    # Computing the true positive rate
    p_1_alter <- g_0[-i, -i, drop = FALSE]
    p_1_alter <- which(p_1_alter == 1, arr.ind = TRUE)
    
    # If no ties present, then the probabilities are set to be NA
    p_1_alter <- if (length(p_1_alter)) {
      mean(g_i[-i, -i, drop=FALSE][p_1_alter] == 1, na.rm = TRUE)
    } else {
      NA_real_
    }
    
    # The same but for 0
    p_0_alter <- g_0[-i, -i, drop = FALSE]
    p_0_alter <- which(p_0_alter == 0, arr.ind = TRUE)
    
    p_0_alter <- if (length(p_0_alter)) {
      mean(g_i[-i, -i, drop=FALSE][p_0_alter] == 0, na.rm = TRUE)
    } else {
      NA_real_
    }
    
    # Now only for i
    p_1_ego <- c(g_0[i, ], g_0[, i])
    p_1_ego <- which(p_1_ego == 1)
    
    p_1_ego <- if (length(p_1_ego)) {
      mean(
        c(g_i[i, ], g_i[, i])[p_1_ego] == 1,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
    
    # The same but for 0
    p_0_ego <- c(g_0[i,], g_0[, i])
    p_0_ego <- which(p_0_ego == 0)
    
    p_0_ego <- if (length(p_0_ego)) {
      mean(
        c(g_i[i, ], g_i[, i])[p_0_ego] == 0,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
    
    data.frame(
      k         = i,
      p_0_ego   = p_0_ego,
      p_1_ego   = p_1_ego,
      p_0_alter = p_0_alter,
      p_1_alter = p_1_alter
    )
    
    
  })
  
  # Generating the samples
  res <- do.call(rbind, edgelist)
  class(res) <- c(class(res), "tie_level_accuracy")
  res
  
}

#' @rdname tie_level_accuracy
#' @param prob A numeric vector of length 4L or a data frame (see details).
#' @param i Integer vector. The network to sample from.
#' @param keep_baseline Logical scalar. When `TRUE`, the function returns
#' the baseline network as the first element of the list.
#' @details
#' The function `sample_css_network` samples perceived networks from the
#' baseline network. The baseline network is the first network in the
#' `graph` object. The function `tie_level_accuracy` can be used to
#' generate the probability vector. 
#' 
#' The probability vector is a numeric vector of length 4L. The first
#' two elements are the probability of a tie/no tie between an ego and an alter.
#' The third and fourth elements are the probability of a tie/no tie between
#' two alters. When `prob` is a data frame, the function will sample from
#' each row of the data frame (returned from the function `tie_level_accuracy`).
#' 
#' @examples
#' # Using the Krackhardt advice network
#' data(krackhardt_advice)
#' data(krackhardt_advice_perceptions)
#'
#' n_people <- 21
#' advice_matrix <- matrix(0L, nrow = n_people, ncol = n_people)
#' advice_matrix[cbind(krackhardt_advice$from, krackhardt_advice$to)] <-
#'   krackhardt_advice$value
#'
#' krack_graph <- new_barry_graph(
#'   c(list(advice_matrix), krackhardt_advice_perceptions)
#' )
#'
#' # Method 1: Using accuracy data frame (recommended)
#' accuracy <- tie_level_accuracy(krack_graph)
#' sampled_networks <- sample_css_network(krack_graph, prob = accuracy)
#' length(sampled_networks)
#'
#' # Method 2: Using manual probability vector for a single perceiver
#' # p_0_ego, p_1_ego, p_0_alter, p_1_alter
#' manual_probs <- c(0.8, 0.9, 0.85, 0.75)
#' sampled_manual <- sample_css_network(
#'   krack_graph,
#'   prob = manual_probs,
#'   i = 1,
#'   keep_baseline = FALSE
#' )
#'
#' @export
#' @return 
#' The function `sample_css_network` returns a list of square matrices of size
#' `attr(graph, "netsize")`. If `keep_baseline = TRUE`, the first element of
#' the list is the baseline network. Otherwise, it is not returned.
sample_css_network <- function(
    graph,
    prob = tie_level_accuracy(graph),
    i = 1L:attr(graph, "netsize"),
    keep_baseline = TRUE) {
  
  if (!inherits(graph, "barry_graph"))
    stop("The graph is not a barry_graph.", call. = FALSE)
  
  # Getting the edgelist
  elist <- barray_to_edgelist(graph)
  
  # And other graph attributes
  netsize   <- attr(graph, "netsize")
  
  # Generating the baseline graph
  elist0 <- elist[
    which((elist[, 1] <= netsize) & (elist[, 2] <= netsize)), ,
    drop = FALSE
  ]
  
  g_0 <- matrix(0L, nrow = netsize, ncol = netsize)
  g_0[elist0] <- 1L
  
  # Sampling the network
  if (inherits(prob, "numeric")) {
    
    if (length(prob) != 4L)
      stop("The probability vector must have length 4L.", call. = FALSE)
    
    if (missing(i))
      stop("The network to sample from must be specified.", call. = FALSE)
    
    # Generating the sample
    list(
      if (keep_baseline) g_0 else NULL,
      sample_css_network_vector(g_0, prob, i)
    )
    
  } else if (inherits(prob, "data.frame")) {
    
    # Generating the sample
    c(
      if (keep_baseline) list(g_0) else NULL,
      sample_css_network_dataframe(g_0, prob)
    )
    
  } else
    stop("The probability must be a numeric vector or a data.frame.", call. = FALSE)
  
}

sample_css_network_dataframe <- function(g_0, prob) {
  
  # Splitting the prob data.frame by row
  lapply(seq_len(nrow(prob)), function(i) {
    
    # Getting the probabilities
    p <- unlist(
      prob[i, c("p_0_ego", "p_1_ego", "p_0_alter", "p_1_alter")]
    )
    
    # Generating the sample
    sample_css_network_vector(g_0, p, i)
    
  })
  
}

#' @importFrom stats runif
#' @noRd
sample_css_network_vector <- function(g_0, prob, i) {
  
  # Creating empty matrix
  g <- matrix(0L, nrow = nrow(g_0), ncol = ncol(g_0))
  
  # Inverting the first and third of prob, so that way
  # all are "probability of the entry to be equal to 1"
  prob[c(1, 3)] <- 1 - prob[c(1, 3)]
  
  # Greating the matrix of probabilities (first and second position)
  pmat <- g_0
  pmat[] <- prob[g_0 + 3]
  
  # Editing ego probs (third and fourth position)
  pmat[i,] <- prob[g_0[i,] + 1]
  pmat[,i] <- prob[g_0[,i] + 1]
  
  # Sampling
  n <- nrow(g_0)
  res <- matrix(
    as.integer(stats::runif(n*n) < as.vector(pmat)),
    nrow = n
  )
  
  diag(pmat) <- NA_real_
  diag(res)  <- 0L
  
  structure(
    res,
    probs = pmat
  )
  
}