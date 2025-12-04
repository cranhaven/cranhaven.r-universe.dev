utils::globalVariables(c(".","amv"))

#' @useDynLib MSmix, .registration=TRUE
#' @importFrom BayesMallows compute_expected_distance compute_rank_distance get_cardinalities sample_mallows
#' @importFrom bmixture rdirichlet
#' @importFrom data.table data.table
#' @importFrom dplyr filter
#' @importFrom factoextra fviz_dist
#' @importFrom fields image.plot
#' @importFrom foreach %dopar% foreach getDoParRegistered getDoParWorkers
#' @importFrom ggplot2 aes element_blank element_text geom_bar geom_point geom_text geom_tile ggplot ggtitle guides guide_legend labs theme theme_minimal scale_fill_gradient scale_fill_gradientn scale_size_continuous scale_x_discrete scale_x_continuous scale_y_discrete scale_y_reverse stat_ecdf ylim
#' @importFrom ggbump geom_bump
#' @importFrom graphics box image legend lines par points polygon text mtext
#' @importFrom grDevices adjustcolor colorRamp colorRampPalette rgb
#' @importFrom gmp factorialZ
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @importFrom Rankcluster frequence
#' @importFrom RColorBrewer brewer.pal
#' @importFrom Rcpp evalCpp
#' @importFrom rlang .data
#' @importFrom reshape melt
#' @importFrom scales hue_pal
#' @importFrom spsUtil quiet
#' @importFrom nnet multinom
#' @importFrom stats addmargins coef density dist heatmap na.omit qnorm rnorm runif uniroot
#' @importFrom utils getFromNamespace menu sessionInfo
NULL


# assign_cluster_full ----
#/' Utility for the exported fitMSmix
#/'
#/' Compute the matrix of the estimated posterior component membership probabilities for the \eqn{N} individual FULL rankings from that returned by the EM algorithm associated to the \eqn{L} distinct FULL rankings.
#/'
#/' @keywords internal
#/' @param rankings_orig Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with full rankings in each row.
#/' @param z_hat Numeric \eqn{L}\eqn{\times}{x}\eqn{G} matrix of the estimated posterior component membership probabilities for the \eqn{L\leq N} distinct full rankings in \code{rankings_orig}.
#/'
#/' @return Numeric \eqn{N}\eqn{\times}{x}\eqn{G} matrix of the estimated posterior component membership probabilities for the \eqn{N} individual full rankings in \code{rankings_orig}.
#/'
assign_cluster_full <- function(rankings_orig, z_hat) {

  l <- do.call(paste, data.frame(rankings_orig))
  ml <- match(l, l)
  tab <- table(ml)
  quali <- list()
  j <- 1
  for (i in names(tab)) {
    quali[[j]] <- which(ml == as.integer(i))
    j <- j + 1
  }
  n_clust <- ncol(z_hat)
  z_hat_orig <- matrix(NA, ncol = n_clust, nrow = nrow(rankings_orig))
  for (i in 1:nrow(z_hat)) {
    z_hat_orig[quali[[i]], ] <- matrix(rep(z_hat[i, ], length(quali[[i]])),
                                       ncol = n_clust, byrow = TRUE
    )
  }
  return(z_hat_orig)
}



# assign_cluster_partial ----
#/' Utility for the exported fitMSmix
#/'
#/' Compute the matrix of the estimated posterior component membership probabilities for the \eqn{N} individual PARTIAL rankings from that returned by the EM algorithm associated to the \eqn{M} distinct FULL rankings obtained with \code{data_augmentation}.
#/'
#/' @keywords internal
#/' @param rankings_part_orig Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row.
#/' @param aug_list A list with elements corresponding to the matrices of full rankings compatible with the partial sequences in \code{rankings_part}.
#/' @param aug_mat Integer matrix of full rankings with rows given by the stacked matrices in \code{aug_list}.
#/' @param z_hat Numeric \eqn{M}\eqn{\times}{x}\eqn{G} matrix of the estimated posterior component membership probabilities for the \eqn{M} distinct full rankings associated to \code{rankings_part_orig}.
#/' @param freq_compl Integer vector with the estimated frequencies of each distinct full ranking.
#/'
#/' @return Numeric \eqn{N}\eqn{\times}{x}\eqn{G} matrix of the estimated posterior component membership probabilities for the \eqn{N} individual partial rankings in \code{rankings_part_orig}.
#/'
assign_cluster_partial <- function(rankings_part_orig,
                                   aug_list,
                                   aug_mat,
                                   z_hat,
                                   freq_compl) {
  n_clust <- ncol(z_hat)
  l <- do.call(paste, data.frame(rankings_part_orig))
  ml <- match(l, l)
  tab <- table(ml)
  quali <- list()
  j <- 1
  for (i in names(tab)) {
    quali[[j]] <- which(ml == as.integer(i))
    j <- j + 1
  }
  n_aug_mat <- nrow(aug_mat)

  l <- do.call(paste, data.frame(aug_mat))
  ml <- match(l, l)
  tab <- table(ml)

  quali2 <- list()
  j <- 1

  for (i in names(tab)) {
    quali2[[j]] <- which(ml == as.integer(i))
    j <- j + 1
  }
  # quali2 is a list from augmented full data to unique augmented full data
  # quali2 is same dimension if no nested missing

  # if there's nested relevant go back on the space of aug_mat
  if (nrow(z_hat) != n_aug_mat) {
    fitz_expl <- matrix(NA, ncol = ncol(z_hat), nrow = n_aug_mat)
    freq_expl <- vector(length = n_aug_mat)
    auglist_expl <- list()
    for (i in 1:nrow(z_hat)) {
      fitz_expl[quali2[[i]], ] <- matrix(rep(z_hat[i, ], length(quali2[[i]])),
                                         ncol = n_clust, byrow = TRUE
      )
      freq_expl[quali2[[i]]] <- rep(freq_compl[i], length(quali2[[i]]))
    }
  } else {
    fitz_expl <- z_hat
    freq_expl <- freq_compl
  }

  # from fitz_expl to unique original (reduce)
  Nstar <- length(aug_list)
  tmp <- fitz_expl
  tmp2 <- freq_expl
  z_rid <- matrix(NA, ncol = ncol(z_hat), nrow = Nstar)

  for (j in 1:Nstar) {
    quanti <- nrow(aug_list[[j]])
    zj <- tmp[1:quanti,,drop=FALSE]
    fj <- tmp2[1:quanti]
    prodotto <- zj * fj
    z_rid[j, ] <- prop.table(colMeans(prodotto))
    tmp <- tmp[-c(1:quanti), ,drop=FALSE]
    tmp2 <- tmp2[-c(1:quanti)]
  }

  # Expand
  z_hat_orig <- matrix(NA, ncol = ncol(z_rid), nrow = nrow(rankings_part_orig))
  for (i in 1:nrow(z_rid)) {
    z_hat_orig[quali[[i]], ] <- matrix(rep(z_rid[i, ], length(quali[[i]])),
                                       ncol = ncol(z_rid), byrow = TRUE
    )
  }

  return(z_hat_orig)
}


# data_augmentation ----
#' Data augmentation of partial rankings
#'
#' @description For a given partial ranking matrix, generate all possible full rankings which are compatible with each partially ranked sequence. Partial rankings with at most 10 missing positions and arbitrary patterns of censoring are supported.
#'
#' @details The data augmentation of a full ranking returns the complete ranking itself arranged in a row vector. The function can be applied on partial observations expressed in ordering format as well. A message informs the user when data augmentation may be heavy.
#'
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#' @param fill_single_na Logical: whether single missing positions in the row of \code{rankings} must be filled in prior to data augmentation. Defaults to \code{TRUE}.
#'
#' @return A list of \eqn{N} elements corresponding to the matrices of full rankings compatible with each partial sequence.
#'
#' @references
#' Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#'
#' @examples
#'
#' ## Example 1. Data augmentation of a single partial top-9 ranking.
#' data_augmentation(c(3, 7, 5, 1, NA, 4, NA, 8, 2, 6, NA, 9))
#'
#' ## Example 2. Data augmentation of partial ranking matrix with different censoring patterns.
#' rank_data <- rbind(c(NA, 4, NA, 1, NA),
#'                    c(NA, NA, NA, NA, 1),
#'                    c(2, NA, 1, NA, 3),
#'                    c(4, 2, 3, 5, 1),
#'                    c(NA, 4, 1, 3, 2))
#' data_augmentation(rank_data)
#'
#'
#'
#' @export
#'
data_augmentation <- function(rankings,
                              fill_single_na = TRUE) {

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }

  quali_na <- is.na(rankings)
  quanti_na <- rowSums(quali_na)

  if (any(rowSums(!quali_na) == 0)) {
    stop("Some rankings have all NA entries and should be removed before performing the analysis.\n")
  }

  if (any(quanti_na > 10)) {
    stop("Data augmentation cannot be performed because some partial rankings have more than 10 missing positions.\n")
  }

  if (fill_single_na) {
    rankings <- fill_single_entries_new(data = rankings)
  }

  n_items <- ncol(rankings)
  N <- nrow(rankings)
  out <- vector(mode = "list", length = N)

  if ((n_items > 11)&(any(quanti_na > 6))) {
    message("Generating all possible full rankings compatible with the partial observations. \nPlease, be aware that this may be slow and allocate a lot of memory.\n")
  }

  for (j in 1:N) {
    r_obs <- rankings[j, ]
    if (quanti_na[j] == 0) {
      out[[j]] <- matrix(r_obs, nrow = 1)
    } else {
      mis <- which(quali_na[j, ])
      permj <- perm[[quanti_na[j]]]
      qualiranks <- setdiff(1:n_items, r_obs)
      out[[j]] <- matrix(r_obs, ncol = n_items, nrow = nrow(permj), byrow = TRUE)
      out[[j]][, mis] <- qualiranks[permj]
    }
  }

  return(out)
} # list of N elements which are all matrices with n columns and an arbitrary number of rows

# data_completion ----
#' Completion of partial rankings with reference full rankings
#'
#' @description Deterministic completion of partial rankings with the relative positions of the unranked items in the reference full rankings. Partial rankings with arbitrary patterns of censoring are supported.
#'
#' @details
#' The arguments \code{rankings} and \code{ref_rho} must be objects with the same class (matrix or data frame) and same dimensions.
#'
#' The completion of a full ranking returns the complete ranking itself.
#'
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with the partial rankings to be completed in each row. Missing positions must be coded as \code{NA}.
#' @param ref_rho Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame whose rows represent the reference full rankings to be used to complete the partial rankings.
#'
#' @return Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with the completed rankings in each row.
#'
#' @references
#' Crispino M, Mollica C and Modugno L (2025+). MSmix: An R Package for clustering partial rankings via mixtures of Mallows Models with Spearman distance. \emph{(submitted)}
#'
#' @examples
#'
#' ## Example 1. Completion of a single partial ranking.
#' data_completion(rankings = c(3, NA, NA, 1, NA), ref_rho = c(4, 5, 1, 3, 2))
#'
#' ## Example 2. Completion of partial rankings with arbitrary censoring patterns.
#' rankings <- rbind(c(3, NA, NA, 7, 2, NA, NA), c(NA, 6, NA, 5, NA, NA, 1),7:1)
#' data_completion(rankings = rankings, ref_rho = rbind(c(4, 5, 6, 1, 3, 7, 2),
#'                 7:1, 1:7))
#'
#' @export
#'

data_completion <- function(rankings,ref_rho) {

  if (class(rankings)[1] == class(ref_rho)[1]) {
    if (is.vector(rankings) & is.vector(ref_rho)) {
      rankings <- matrix(rankings, nrow = 1)
      ref_rho <- matrix(ref_rho, nrow = 1)
    }


    if (any(is.na(ref_rho))) {
      stop("The argument 'ref_rho' must contain full rankings in each row.\n")
    }

    if (!(identical(dim(rankings),dim(ref_rho)))) {
      stop("The arguments 'rankings' and 'ref_rho' must be objects with the same dimensions.\n")
    }

    rankings <- fill_single_entries_new(data = rankings)

    check_na <- is.na(rankings)

    if (any(check_na)) {

      if (any(rowSums(!check_na) == 0)) {
        stop("Some rankings have all NA entries and should be removed before performing the analysis.\n")
      }

      N <- nrow(rankings)
      compl_rankings <- t(sapply(1:N, function(x) suppressWarnings(ranking_completion(rankings[x,], ref_rho[x,]))))

      return(compl_rankings)
    } else {
      warning("Data completion is not necessary because ranking_part contains full rankings only.\n")
    }
  } else {
    stop("The rankings and ref_rho arguments must be objects with the same class and dimensions.\n")
  }
} # matrix with the partial rankings completed in each row.


# ranking_completion ----
#/' Utility for the exported data_completion
#/'
#/' Deterministic completion of a partial ranking with the relative positions of the unranked items provided by a reference full ranking.
#/'
#/' @keywords internal
#/' @param part_ranking Integer vector of length \eqn{n} representing the partial ranking to be completed.
#/' @param rho Integer vector of length \eqn{n} representing the reference full ranking.
#/'
#/' @return A full ranking of \eqn{n} items.
#/'
ranking_completion <- function(part_ranking,rho) {
  is_item_unranked <- is.na(part_ranking)
  sum_unranked <- sum(is_item_unranked)
  if (sum_unranked == 0) {
    warning("The input partial ranking is already complete.\n")
  } else {
    n <- length(rho)
    if (sum_unranked > 1) {
      part_ranking[is_item_unranked] <- setdiff(1:n, part_ranking)[rank(rho[is_item_unranked])]
    } else {
      part_ranking[is_item_unranked] <- setdiff(1:n, part_ranking)
      warning("The input partial ranking is already complete. The single missing rank has been added.\n")
    }
  }
  return(part_ranking) # vector of length n which is, actually, the partial ranking completed.
}


# ranking_completion_hide ----
#/' Utility for the internal em_db_mix and the exported fitMSmix
#/'
#/' Deterministic completion of a partial ranking with the relative positions of the unranked items provided by a reference full ranking.
#/'
#/' Although this internal routine parallels the utility \code{ranking_completion}, it allows to speed up the iterative completion of partial rankings in the MCEM algorithm.
#/'
#/' @keywords internal
#/' @param part_ranking Integer vector of length \eqn{n} representing the partial ranking to be completed.
#/' @param rho Integer vector of length \eqn{n} representing the reference full ranking.
#/' @param items_unranked Integer vector with the labels of the unranked items in the \code{part_ranking}.
#/' @param n_items Number of items.
#/'
#/' @return A full ranking of \eqn{n} items.
#/'
ranking_completion_hide <- function(part_ranking, rho, items_unranked, n_items) {
  part_ranking[items_unranked] <- setdiff(1:n_items, part_ranking)[rank(rho[items_unranked])]
  return(part_ranking) # vector of length n which is, actually, the partial ranking completed.
}

# data_conversion ----
#' Switch data format from rankings to orderings and vice versa
#'
#' @description Convert the format of the input dataset from rankings to orderings and vice versa. Differently from existing analogous functions supplied by other \code{R} packages, \code{data_conversion} supports also partial rankings/orderings with arbitrary patterns of censoring.
#'
#' @param data Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with partial sequences (either rankings or orderings) in each row, whose format has to be converted. Missing entries must be coded as \code{NA}.
#' @param subset Optional logical or integer vector specifying the subset of observations, i.e. rows of \code{data}, to be kept. Missing values are taken as \code{FALSE}. Defaults to \code{NULL} meaning that all the rows are considered.
#'
#' @return Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix of partial sequences with inverse format with respect to the input \code{data}.
#'
#' @examples
#'
#' ## Example 1. Switch the data format for a single complete observation.
#' data_conversion(c(4, 5, 1, 3, 2))
#'
#' ## Example 2. Switch the data format for partial sequences with arbitrary censoring patterns.
#' data_conversion(rbind(c(NA, 2, 5, NA, NA), c(4, NA, 2, NA, 3), c(4, 5, 1, NA, NA),
#'                       c(NA, NA, NA, NA, 2), c(NA, 5, 2, 1, 3), c(3, 5, 1, 2, 4)))
#'
#' @export
#'
data_conversion <- function(data, subset = NULL) {

  if (!is.matrix(data)) {
    if (is.vector(data)) {
      data <- matrix(data, nrow = 1)
    } else {
      data <- as.matrix(data)
    }
  }

  if (!is.null(subset)) {
    if(is.logical(subset)){
      data <- data[(subset & !is.na(subset)),, drop = FALSE]
    }else{
      data <- data[na.omit(subset),, drop = FALSE]
    }
  }

  data <- fill_single_entries_new(data = data)

  check_na <- is.na(data)
  if (any(rowSums(!check_na) == 0)) {
    stop("Some rankings have all NA entries and should be removed before performing the analysis.\n")
  }

  n_items <- ncol(data)
  out <- t(apply(data, 1, myorder_new, n_items = n_items))

  return(out)
} # N*n_items matrix


# fill_single_entries_new ----
#/' Utility for the preliminary check of the partial sequences in several exported functions
#/'
#/' Fill in the single missing entries in partial orderings/rankings. Recall that a partial sequence with \eqn{(n-1)} observed entries corresponds to a fully observed sequence.
#/'
#/' @keywords internal
#/' @param data Integer matrix with partial sequences (either rankings or orderings) in each row.
#/'
#/' @return Integer data matrix of partial sequences in the same format of the input \code{data} with possible single missing entries filled.
#/'
fill_single_entries_new <- function(data) {

  if (is.vector(data)) {
    data <- matrix(data, nrow = 1)
  }

  n_items <- ncol(data)
  temp_na <- is.na(data)
  r_single_miss <- (rowSums(temp_na) == 1)

  if (any(r_single_miss)) {
    w_row <- which(r_single_miss)
    w_col <- apply(temp_na[w_row, , drop = FALSE], 1, which)
    w_item <- apply(data[w_row, , drop = FALSE], 1, setdiff, x = 1:n_items)
    data[cbind(w_row, w_col)] <- w_item
    warning(paste("Partial rankings of length", n_items - 1, "correspond to full rankings. Single missing entries filled.\n"), call. = FALSE)
  }

  return(data)
}

# myorder_new ----
#/' Utility for the exported data_conversion
#/'
#/' Switch the data format of a single partial sequence from ranking to ordering and viceversa.
#/'
#/' It works also with complete sequences.
#/'
#/' @keywords internal
#/' @param x Integer vector of length \eqn{n} representing a single partial sequence (either ranking or ordering). Missing positions must be coded as \code{NA}.
#/' @param n_items Number of items.
#/'
#/' @return The input partial sequence with inverse data format.
#/'
myorder_new <- function(x, n_items) {
  if (any(is.na(x))) {
    temp <- order(x, na.last = NA)
    out <- rep(NA, n_items)
    out[sort(na.omit(x))] <- temp
  } else {
    out <- order(x)
  }

  return(out)
}


# data_description ----
#' Descriptive summaries for partial rankings
#'
#' @description Compute various data summaries for a partial ranking dataset. Differently from existing analogous functions supplied by other \code{R} packages, \code{data_description} supports partial observations with arbitrary patterns of censoring.
#'
#' @details
#'
#' The implementation of \code{data_description} is similar to that of \code{rank_summaries} from the \code{PLMIX} package. Differently from the latter, \code{data_description} works with any kind of partial rankings (not only top rankings) and allows to summarize subsamples thanks to the additional \code{subset} argument.
#'
#' The Borda ranking, obtained from the ordering of the mean rank vector, corresponds to the MLE of the consensus ranking of the Mallows model with Spearman distance. If \code{mean_rank} contains some \code{NA}s, the corresponding items occupy the bottom positions in the \code{borda_ordering} according to the order they appear in \code{item_names}.
#'
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#' @param marg Logical: whether the first-order marginals have to be computed. Defaults to \code{TRUE}.
#' @param borda_ord Logical: whether, in the summary statistics, the items must be ordered according to the Borda ranking (i.e., mean rank vector). Defaults to \code{FALSE}.
#' @param paired_comp Logical: whether the pairwise comparison matrix has to be computed. Defaults to \code{TRUE}.
#' @param subset Optional logical or integer vector specifying the subset of observations, i.e. rows of \code{rankings}, to be kept. Missing values are taken as \code{FALSE}. Defaults to \code{NULL} meaning that all the rows are considered.
#' @param item_names Character vector with the names to be used for the items. Defaults to \code{NULL}, meaning that \code{colnames(rankings)} is used and, if not available, \code{item_names} is set equal to \code{"Item1","Item2",...}.
#'
#' @return An object of class \code{"data_descr"}, which is a list with the following named components:
#'
#'  \item{\code{n_ranked}}{Integer vector of length \eqn{N} with the number of items ranked in each partial sequence.}
#'  \item{\code{n_ranked_distr}}{Frequency distribution of the \code{n_ranked} vector.}
#'  \item{\code{n_ranks_by_item}}{Integer \eqn{3}\eqn{\times}{x}\eqn{n} matrix with the number of times that each item has been ranked or not. The last row contains the total by column, i.e. the sample size \eqn{N}.}
#'  \item{\code{mean_rank}}{Mean rank vector.}
#'  \item{\code{borda_ordering}}{Character vector corresponding to the Borda ordering. This is obtained from the ranking of the mean rank vector.}
#'  \item{\code{marginals}}{Integer \eqn{n}\eqn{\times}{x}\eqn{n} matrix of the first-order marginals in each column: the \eqn{(j,i)}-th entry indicates the number of times that item \eqn{i} is ranked in position \eqn{j}.}
#'  \item{\code{pc}}{Integer \eqn{n}\eqn{\times}{x}\eqn{n} pairwise comparison matrix: the \eqn{(i,i')}-th entry indicates the number of times that item \eqn{i} is preferred to item \eqn{i'}.}
#'  \item{\code{rankings}}{When \code{borda_ord = TRUE}, an integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix corresponding to \code{rankings} with columns rearranged according to the Borda ordering, otherwise the input \code{rankings}.}
#'
#'
#' @references
#' Mollica C and Tardella L (2020). PLMIX: An R package for modelling and clustering partially ranked data. \emph{Journal of Statistical Computation and Simulation}, \bold{90}(5), pages 925--959, ISSN: 0094-9655, DOI: 10.1080/00949655.2020.1711909.
#'
#' Marden JI (1995). Analyzing and modeling rank data. \emph{Monographs on Statistics and Applied Probability} (64). Chapman & Hall, ISSN: 0-412-99521-2. London.
#'
#' @seealso \code{\link{plot.data_descr}}, \code{\link{print.data_descr}}
#'
#' @examples
#'
#' ## Example 1. Sample statistics for the Antifragility dataset.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' descr <- data_description(rankings = r_antifrag)
#' descr
#'
#' ## Example 2. Sample statistics for the Sports dataset.
#' r_sports <- ranks_sports[, 1:8]
#' descr <- data_description(rankings = r_sports, borda_ord = TRUE)
#' descr
#'
#' ## Example 3. Sample statistics for the Sports dataset by gender.
#' r_sports <- ranks_sports[, 1:8]
#' desc_f <- data_description(rankings = r_sports, subset = (ranks_sports$Gender == "Female"))
#' desc_m <- data_description(rankings = r_sports, subset = (ranks_sports$Gender == "Male"))
#' desc_f
#' desc_m
#'
#' @export
#'
data_description <- function(rankings, marg = TRUE, borda_ord = FALSE, paired_comp = TRUE, subset = NULL, item_names = NULL) {
  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }


  if (!is.null(subset)) {
    if(is.logical(subset)){
      rankings <- rankings[(subset & !is.na(subset)),, drop = FALSE]
    }else{
      rankings <- rankings[na.omit(subset),, drop = FALSE]
    }
  }

  rankings <- fill_single_entries_new(data = rankings)
  N <- nrow(rankings)
  n_items <- ncol(rankings)

  if (is.null(item_names)) {
    item_names <- colnames(rankings)
    if (is.null(item_names)) {
      colnames(rankings) <- item_names <- paste0("Item", 1:n_items)
    }
  } else {
    colnames(rankings) <- item_names
  }

  mean_rank <- colMeans(rankings, na.rm = TRUE)

  if (borda_ord) {
    rankings <- rankings[, order(mean_rank), drop = FALSE]
    mean_rank <- mean_rank[order(mean_rank)]
    item_names <- colnames(rankings)
  }

  isna_data <- is.na(rankings)
  n_ranked <- rowSums(!isna_data)

  if (any(n_ranked == 0)) {
    stop("Some rankings have all NA entries and should be removed before performing the analysis.\n")
  }

  n_ranked_distr <- table(factor(n_ranked, levels = 1:n_items))
  na_by_item <- colSums(isna_data)
  n_ranks_by_item <- rbind(na_by_item, N - na_by_item, rep(N, n_items))
  dimnames(n_ranks_by_item) <- list(c("n. missing pos.", "n. observed pos.", "sample size"), item_names)

  borda_ordering <- names(mean_rank)[order(mean_rank)]

  if (marg) {
    marginals <- apply(rankings, 2, tabulate, nbins = n_items)
    rownames(marginals) <- paste0("Rank", 1:n_items)
    marginals_tot <- addmargins(marginals)
  } else {
    marginals_tot <- NULL
  }

  if (paired_comp) {
    pc <- paired_comparisonsMSmix(rank_data = rankings)
    rownames(pc) <- colnames(pc) <- item_names
  } else {
    pc <- NULL
  }

  out <- list(
    n_ranked = n_ranked,
    n_ranked_distr = n_ranked_distr,
    n_ranks_by_item = n_ranks_by_item,
    mean_rank = mean_rank,
    borda_ordering = borda_ordering,
    marginals = marginals_tot,
    pc = pc,
    rankings = rankings
  )

  class(out) <- "data_descr"

  message("Use function plot() to visualize the object of class 'data_descr'.\n")

  return(out)
}

# print.data_descr ----
#' Print descriptive statistics for partial rankings
#'
#' @description \code{print} method for class \code{"data_descr"}.
#'
#'
#' @param x An object of class \code{"data_descr"} returned by \code{\link{data_description}}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @rdname data_description
#'
#' @export print.data_descr
#' @export
#'

print.data_descr <- function(x, ...) {
  data_descr_out <- x

  if (!is(data_descr_out, "data_descr")) {
    stop("The function requires an object of S3 class 'data_descr' as its first argument.\n")
  }

  N <- length(data_descr_out$n_ranked)
  n <- length(data_descr_out$mean_rank)
  cat("\n")
  cat("Sample size:", N, "\n")
  cat("N. of items:", n, "\n")
  cat("\n")
  cat("Frequency distribution of the number of ranked items:\n")
  print(data_descr_out$n_ranked_distr)
  cat("\n")
  cat("Number of missing positions for each item:\n")
  cat("\n")
  print(data_descr_out$n_ranks_by_item[1, ])
  cat("\n")
  mean_rank <- round(data_descr_out$mean_rank, 2)
  cat("Mean rank of each item:\n")
  cat("\n")
  print(mean_rank)
  cat("\n")
  cat("Borda ordering:\n")
  cat("\n")
  print(data_descr_out$borda_ordering)
  cat("\n")
  cat("First-order marginals:\n")
  cat("\n")
  print(data_descr_out$marginals)
  cat("\n")
  cat("Pairwise comparison matrix:\n")
  cat("\n")
  print(data_descr_out$pc)
}

# plot.data_descr ----
#' Plot descriptive statistics for partial rankings
#'
#' @description \code{plot} method for class \code{"data_descr"}.
#'
#' @details The plots of the marginals distributions and pairwise comparisons are constructed if the object \code{x} was obtained from the \code{data_description} routine with arguments \code{marg = TRUE} and \code{pc = TRUE}; otherwise, a \code{NULL} element in the output list is returned.
#'
#' @param x An object of class \code{"data_descr"} returned by \code{\link{data_description}}.
#' @param cex_text_mean Positive scalar: the magnification to be used for all the labels in the plot for the mean rank vector. Defaults to 1.
#' @param cex_symb_mean Positive scalar: the magnification to be used for the symbols in the pictogram of the mean rank vector. Defaults to 12.
#' @param marg_by Character indicating whether the marginal distributions must be reported by \code{"item"} or by \code{"rank"} in the heatmap. Defaults to \code{"item"}.
#' @param cex_text_pc Positive scalar: the magnification to be used for all the labels in the bubble plot of the paired comparison frequencies. Defaults to 3.
#' @param cex_range_pc Numeric vector indicating the range of values to be used on each axis in the bubble plot of the paired comparison frequencies. Defaults to \code{c(8,20)}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @return A list of 5 labelled plots displaying descriptive summaries of the partial ranking dataset, namely: i) \code{n_ranked_distr}: a barplot of the frequency distribution (%) of the number of items actually ranked in each partial sequence, ii) \code{picto_mean_rank}: a basic pictogram of the mean rank vector, iii) \code{marginals}: a heatmap of the marginal distributions (either by item or by rank), iv) \code{ecdf}: the ecdf of the marginal rank distributions and v) \code{pc}: a bubble plot of the pairwise comparison matrix.
#'
#' @references
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. ISBN 978-3-319-24277-4, \url{https://ggplot2.tidyverse.org}.
#'
#' @seealso \code{\link{data_description}}
#'
#' @examples
#'
#' ## Example 1. Plot the mean rank vector and marginal distributions for the Antifragility dataset.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' desc <- data_description(r_antifrag)
#' p_desc <- plot(desc)
#' p_desc$picto_mean_rank()
#' p_desc$marginals()
#'
#' ## Example 2. Plot the distribution of the number of ranked items and the
#' # pairwise comparison matrix for the Sports dataset.
#' r_sports <- ranks_sports[, 1:8]
#' desc <- data_description(rankings = r_sports, borda_ord = TRUE)
#' p_desc <- plot(desc, cex_text_mean = 1.2)
#' p_desc$n_ranked_distr()
#' p_desc$pc()
#'
#' ## Example 3. Plot the ecdf's for the marginal rank distributions for the Sports dataset by gender.
#' r_sports <- ranks_sports[, 1:8]
#' desc_f <- data_description(rankings = r_sports, subset = (ranks_sports$Gender == "Female"))
#' p_desc_f <- plot(desc_f, cex_text_mean = 1.2)
#' p_desc_f$ecdf()
#' desc_m <- data_description(rankings = r_sports, subset = (ranks_sports$Gender == "Male"))
#' p_desc_m <- plot(desc_m, cex_text_mean = 1.2)
#' p_desc_m$ecdf()
#'
#'
#' @export plot.data_descr
#' @export
#'
plot.data_descr <- function(x, cex_text_mean = 1, cex_symb_mean = 12, marg_by = "item", cex_text_pc = 3, cex_range_pc = c(8, 20), ...) {

  data_descr_out <- x

  if (!is(data_descr_out, "data_descr")) {
    stop("The function requires an object of S3 class 'data_descr' as its first argument.\n")
  }

  print(data_descr_out)

  oldpar <- par(mfrow = c(1, 1))
  on.exit(par(oldpar))

  mean_rank <- data_descr_out$mean_rank
  omr <- order(mean_rank)
  n_items <- length(mean_rank)
  item_names <- names(mean_rank)

  item_col = hue_pal()(n_items)

  df_n_ranked <- as.data.frame(round(100 * prop.table(data_descr_out$n_ranked_distr), 1))
  ggp_n_ranked_distr <- ggplot(data = df_n_ranked, aes(x = df_n_ranked$Var1, y = df_n_ranked$Freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = df_n_ranked$Freq), vjust = -0.3, size = 3.5) +
    labs(x = "Number of ranked items", y = "Percentage") +
    ylim(0, (max(df_n_ranked$Freq) %/% 10 + 1) * 10) +
    theme_minimal()

  plot_list <- list()
  plot_list$n_ranked_distr <- suppress_ggplot(ggp_n_ranked_distr)

  plot_list$picto_mean_rank <- function() {
    plot(x = rep(1, n_items + 2), y = 0:(n_items + 1), xlim = c(-1, 2), ylim = c(0, n_items + 1.25), type = "n", axes = FALSE, xlab = "", ylab = "")
    points(rep(1, n_items), n_items:1, pch = 18, col = item_col[omr], cex = cex_symb_mean * mean_rank[omr] / n_items)
    text(0.3, n_items:1, labels = item_names[omr], font = 2, cex = cex_text_mean, adj = 1)
    text(1.6, n_items:1, labels = round(mean_rank[omr], 2), font = 2, cex = cex_text_mean)
  }

  if (!is.null(data_descr_out$marginals)) {
    NN <- nrow(data_descr_out$marginals)
    colnames(data_descr_out$marginals)[-NN] <- item_names
    marg_melt <- quiet(melt(prop.table(data_descr_out$marginals[-NN, -NN], margin = (if (marg_by == "item") 2 else 1))))
    ggp_marg <- ggplot(data = marg_melt, aes(x = marg_melt$X1, y = marg_melt$X2, fill = marg_melt$value)) +
      geom_tile() +
      scale_fill_gradient(low = "mintcream", high = "slateblue3") +
      guides(fill = guide_legend("Prop.")) +
      ggtitle(paste("Marginal distributions by", (if (marg_by == "item") "item" else "rank"))) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      theme(axis.title = element_blank())

    ecdf_melt <- quiet(melt(as.data.frame(data_descr_out$rankings)))
    ggp_ecdf <- ggplot(data = ecdf_melt, aes(x = ecdf_melt$value, col = ecdf_melt$variable)) +
      stat_ecdf(size = 1, na.rm = TRUE) +
      ggtitle("Marginal rank distributions") +
      labs(color = "Item") +
      theme(axis.title = element_blank())

    plot_list$marginals <- suppress_ggplot(ggp_marg)
    plot_list$ecdf <- suppress_ggplot(ggp_ecdf)
  }else{
    plot_list$marginals <- function() NULL
    plot_list$ecdf <- function() NULL
  }

  if (!is.null(data_descr_out$pc)) {
    dimnames(data_descr_out$pc) <- list(item_names, item_names)
    pc_melt <- quiet(melt(t(data_descr_out$pc)))
    ggp_pc <- ggplot(data = pc_melt, aes(x = as.character(pc_melt$X1), y = as.character(pc_melt$X2), colour = pc_melt$X1, size = pc_melt$value)) +
      geom_point() +
      geom_text(aes(label = pc_melt$value), colour = "white", size = cex_text_pc) +
      labs(x = NULL, y = NULL) +
      scale_x_discrete(position = "top", limits = item_names) +
      scale_y_discrete(limits = rev(item_names)) +
      scale_size_continuous(range = cex_range_pc) +
      ggtitle("Paired comparisons") +
      theme(legend.position = "none", panel.background = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank())

    plot_list$pc <- suppress_ggplot(ggp_pc)
  }else{
    plot_list$pc <- function() NULL
  }

  return(invisible(plot_list))
}

# suppress_ggplot ----
#/' Utility for the exported plot.data_descr and plot.emMSmix
#/'
#/' Suppress ggplot2 warnings during plotting.
#/'
#/' This internal function suppresses warnings generated by ggplot2 when rendering a plot. It ensures the plot is evaluated before warning suppression.
#/'
#/' @keywords internal
#/' @param plot A ggplot2 object to be displayed.
#/'
#/' @return A function that prints the plot without emitting warnings.
#/'
suppress_ggplot <- function(plot) {
  force(plot)  # Ensures evaluation before suppression
  function() suppressWarnings(print(plot))
}

# rMSmix ----
#' Random samples from a mixture of Mallows models with Spearman distance
#'
#' @description Draw random samples of full rankings from a mixture of Mallows models with Spearman distance.
#'
#' @details
#' When \code{n_items > 10} or \code{mh = TRUE}, the random samples are obtained by using the Metropolis-Hastings algorithm, described in Vitelli et al. (2018) and implemented in the \code{sample_mallows} function of the package \code{BayesMallows} package.
#'
#' When \code{theta = NULL}, the concentration parameters are randomly generated from a uniform distribution on the interval \eqn{(1/n^{2},3/n^{1.5})} containing typical values for the precisions.
#'
#' When \code{uniform = FALSE}, the mixing weights are sampled from a symmetric Dirichlet distribution with shape parameters all equal to \eqn{2G}, to favor populated and balanced clusters, and
#' the consensus parameters are sampled to favor well-separated clusters, i. e.,  at least at Spearman distance equal to \eqn{\frac{2}{G}\binom{n+1}{3}} from each other.
#'
#' @param sample_size Number of full rankings to be sampled. Defaults to 1.
#' @param n_items Number of items.
#' @param n_clust Number of mixture components. Defaults to 1.
#' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row. Defaults to \code{NULL}, meaning that the consensus rankings are randomly generated according to the sampling scheme indicated by the \code{uniform} argument. See Details.
#' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters. Defaults to \code{NULL}, meaning that the concentrations are uniformly generated from an interval containing typical values for the precisions. See Details.
#' @param weights Numeric vector of \eqn{G} positive mixture weights (normalization is not necessary). Defaults to \code{NULL}, meaning that the mixture weights are randomly generated according to the sampling scheme indicated by the \code{uniform} argument. See Details.
#' @param uniform Logical: whether \code{rho} or \code{weights} have to be sampled uniformly on their support. When \code{uniform = FALSE} they are sampled, respectively, to ensure separation among mixture components and populated weights. Used when \eqn{G>1} and either \code{rho} or \code{weights} are \code{NULL} (see Details). Defaults to \code{FALSE}.
#' @param mh Logical: whether the samples must be drawn with the Metropolis-Hastings (MH) scheme implemented in the \code{BayesMallows} package, rather by direct sampling from the Mallows probability distribution. For \code{n_items > 10}, the MH is always applied to speed up the sampling procedure. Defaults to \code{TRUE}.
#'
#' @return A list of the following named components:
#'
#'  \item{\code{samples}}{Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with the \code{sample_size} simulated full rankings in each row.}
#'  \item{\code{rho}}{Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings used for the simulation in each row.}
#'  \item{\code{theta}}{Numeric vector of the \eqn{G} component-specific precision parameters used for the simulation.}
#'  \item{\code{weights}}{Numeric vector of the \eqn{G} mixture weights used for the simulation.}
#'  \item{\code{classification}}{Integer vector of the \code{sample_size} component membership labels.}
#'
#' @references
#' Vitelli V, Sørensen Ø, Crispino M, Frigessi A and Arjas E (2018). Probabilistic Preference Learning with the Mallows Rank Model. \emph{Journal of Machine Learning Research}, \bold{18}(158), pages 1--49, ISSN: 1532-4435, \href{https://jmlr.org/papers/v18/15-481.html}{https://jmlr.org/papers/v18/15-481.html}.
#'
#' Sørensen Ø, Crispino M, Liu Q and Vitelli V (2020). BayesMallows: An R Package for the Bayesian Mallows Model. \emph{The R Journal}, \bold{12}(1), pages 324--342, DOI: 10.32614/RJ-2020-026.
#'
#' Chenyang Zhong (2021). Mallows permutation model with L1 and L2 distances I: hit and run algorithms and mixing times. arXiv: 2112.13456.
#'
#' @examples
#'
#' ## Example 1. Drawing from a mixture with randomly generated parameters of separated clusters.
#' set.seed(12345)
#' rMSmix(sample_size = 50, n_items = 25, n_clust = 5)
#'
#'
#' ## Example 2. Drawing from a mixture with uniformly generated parameters.
#' set.seed(12345)
#' rMSmix(sample_size = 100, n_items = 9, n_clust = 3, uniform = TRUE)
#'
#'
#' ## Example 3.  Drawing from a mixture with customized parameters.
#' r_par <- rbind(1:5, c(4, 5, 2, 1, 3))
#' t_par <- c(0.01, 0.02)
#' w_par <- c(0.4, 0.6)
#' set.seed(12345)
#' rMSmix(sample_size = 50, n_items = 5, n_clust = 2, theta = t_par, rho = r_par, weights = w_par)
#'
#' @export
#'
rMSmix <- function(sample_size = 1, n_items, n_clust = 1, rho = NULL, theta = NULL, weights = NULL, uniform = FALSE, mh = TRUE){

  if (is.null(rho)) {
    if (uniform | n_clust == 1) {
      rho <- t(apply(matrix(1:n_items, nrow = n_items, ncol = n_clust), 2, sample))
    } else {
      dmax <- 2 * choose(n_items + 1, 3)
      rho <- sample(n_items)
      for (g in 2:n_clust) {
        d <- 0
        while (min(d) < dmax / (n_clust)) {
          r <- sample(n_items)
          d <- compute_rank_distance(rho, r, "spearman")
        }
        rho <- rbind(rho, r)
      }
      rownames(rho) <- NULL
    }
  } else {
    if (is.vector(rho)) {
      rho <- matrix(rho, nrow = 1)
    }
  }

  if (is.null(theta)) {
    theta <- runif(n_clust, 1/n_items^2, 3/n_items^(1.5))
  } else {
    if (any(theta < 0)) {
      stop("Precision parameters must be non-negative")
    }
  }

  if (is.null(weights)) {
    if (uniform) {
      weights <- runif(n_clust)
      weights <- prop.table(weights)
    } else {
      weights <- as.vector(rdirichlet(1, rep(n_clust * 2, n_clust)))
    }
  } else {
    if (any(weights <= 0)) {
      stop("Mixture weights must be positive")
    } else {
      if (sum(weights) != 1) {
        weights <- prop.table(weights)
      }
    }
  }

  class <- sample(1:n_clust, size = sample_size, replace = TRUE, prob = weights)
  table_class <- tabulate(class, nbins = n_clust)
  data_sim <- NULL

  class_temp <- NULL

  if ((n_items > 10) | mh) {
    message("Metropolis-Hastings Sampling")
    for (i in 1:n_clust) {

      if (table_class[i] > 0) {
        data_sim <- rbind(data_sim, sample_mallows(
          n_samples = table_class[i],
          rho0 = rho[i, ],
          alpha0 = theta[i] * n_items,
          metric = "spearman",
          burnin = n_items * 200,
          thinning = n_items * 10
        ))
        class_temp <- c(class_temp, rep(i, table_class[i]))
      }
    }
  } else {
    message("Exact Sampling")
    allp <- perm[[n_items]]
    for (i in 1:n_clust) {
      f <- exp(-theta[i]*compute_rank_distance(allp, rho[i, ], "spearman"))
      data_sim <- rbind(data_sim, allp[sample(1:nrow(allp), size = table_class[i], replace = TRUE, prob = f), ])
      class_temp <- c(class_temp, rep(i, table_class[i]))
    }
  }

  ord <- sample(sample_size)
  classification <- class_temp[ord]
  data_sim <- data_sim[ord, , drop = FALSE]

  return(list(samples = data_sim, rho = rho, theta = theta, weights = weights, classification = classification))
}

# arbitrary_cens ----
#/' Utility for the exported data_censoring
#/'
#/' Convert full rankings into partial rankings with missing data in arbitrary positions (not necessarily in the top ones).
#/'
#/' @keywords internal
#/' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with full rankings in each row.
#/' @param nranked Integer vector of length \eqn{N} with the desired number of positions to be retained in each partial sequence after censoring. If \code{nranked = NULL} (default), the number of positions are randomly generated according to the probabilities in the \code{probs} argument.
#/' @param probs Numeric vector of the \eqn{(n-1)} probabilities for the random generation of the number of positions to be retained in each partial sequence after censoring (normalization is not necessary). Used only if \code{nranked = NULL}. Defaults to equal probabilities.
#/'
#/' @return A list of two named objects:
#/'  \describe{
#/'  \item{\code{partialdata}}{Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with partial (censored) rankings in each row. Missing positions are coded as \code{NA}.}
#/'  \item{\code{nranked}}{Integer vector of length \eqn{N} with the actual number of items ranked in each partial sequence after censoring.}
#/'  }
#/'
arbitrary_cens <- function(rankings, nranked = NULL, probs = rep(1,ncol(rankings) - 1)){

  n_items <- ncol(rankings)
  N <- nrow(rankings)

  if (is.null(nranked)) {
    nranked <- sample(c(1:(n_items - 2), n_items), size = N, replace = TRUE,
                      prob = probs)
  }

  data_partial <- rankings
  for(j in 1:N){
    r <- rankings[j,]
    dove <- sample(n_items, n_items-nranked[j])
    qualiranks <- r[dove]
    data_partial[j,dove]<-NA
  }

  return(list(partialdata = data_partial, nranked = nranked))
}

# topk_cens ----
#/' Utility for the exported data_censoring
#/'
#/' Convert full rankings into top-k partial rankings with possibly different lengths.
#/'
#/' @keywords internal
#/' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with full rankings in each row.
#/' @param nranked Integer vector of length \eqn{N} with the desired number of top positions to be retained in each partial sequence after censoring. If \code{nranked = NULL} (default), the number of top positions are randomly generated according to the probabilities in the \code{probs} argument.
#/' @param probs Numeric vector of the \eqn{(n-1)} probabilities for the random generation of the number of top positions to be retained in each partial sequence after censoring (normalization is not necessary). Used only if \code{nranked = NULL}. Defaults to equal probabilities.
#/'
#/' @return A list of two named objects:
#/'  \describe{
#/'  \item{\code{partialdata}}{Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with partial (censored) rankings in each row. Missing positions are coded as \code{NA}.}
#/'  \item{\code{nranked}}{Integer vector of length \eqn{N} with the actual number of items ranked in each partial sequence after censoring.}
#/'  }
#/'
topk_cens <- function(rankings, nranked = NULL, probs = rep(1,ncol(rankings) - 1)){

  n_items <- ncol(rankings)
  N <- nrow(rankings)

  if (is.null(nranked)) {
    nranked <- sample(c(1:(n_items - 2), n_items), size = N, replace = TRUE,
                      prob = probs)
  }

  data_partial <- rankings
  for(j in 1:N){
    r <- rankings[j,]
    r[which(r>nranked[j])]<-NA
    data_partial[j,]<-r
  }

  return(list(partialdata = data_partial, nranked = nranked))
}

# data_censoring ----
#' Censoring of full rankings
#'
#' @description Convert full rankings into either top-k rankings or into partial rankings with missing data in arbitrary positions.
#'
#' @details
#' Both forms of partial rankings can be obtained into two ways: (i) by specifying, in the \code{nranked} argument, the number of positions to be retained in each partial ranking; (ii) by setting \code{nranked = NULL} (default) and specifying, in the \code{probs} argument, the probabilities of retaining respectively \eqn{1, 2, ..., (n-1)} positions in the partial rankings (recall that a partial sequence with \eqn{(n-1)} observed entries corresponds to a full ranking).
#'
#' When \code{topk = FALSE}, the exact positions that must be retained into the partial sequences after censoring are uniformly generated, regardless of the specification of the \code{nranked} argument.
#'
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with full rankings in each row.
#' @param topk Logical: whether the full rankings must be converted into top-k rankings (\code{TRUE}) or into partial rankings with missing data in arbitrary positions (\code{FALSE}). Defaults to \code{TRUE}.
#' @param nranked Integer vector of length \eqn{N} with the desired number of positions to be retained in each partial sequence after censoring. If \code{nranked = NULL} (default), the number of positions are randomly generated according to the probabilities in the \code{probs} argument.
#' @param probs Numeric vector of the \eqn{(n-1)} probabilities for the random generation of the number of positions to be retained in each partial sequence after censoring (normalization is not necessary). Used only if \code{nranked = NULL}. Defaults to equal probabilities.
#'
#' @return A list of two named objects:
#'  \describe{
#'  \item{\code{part_rankings}}{Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with partial (censored) rankings in each row. Missing positions are coded as \code{NA}.}
#'  \item{\code{nranked}}{Integer vector of length \eqn{N} with the actual number of items ranked in each partial sequence after censoring.}
#'  }
#'
#' @examples
#'
#' ## Example 1. Censoring the Antifragility dataset into partial top rankings
#' # Top-3 censoring (assigned number of top positions to be retained)
#' n <- 7
#' r_antifrag <- ranks_antifragility[, 1:n]
#' data_censoring(r_antifrag, topk = TRUE, nranked = rep(3,nrow(r_antifrag)))
#' # Random top-k censoring with assigned probabilities
#' set.seed(12345)
#' data_censoring(r_antifrag, topk = TRUE, probs = 1:(n-1))
#'
#' ## Example 2. Simulate full rankings from a basic Mallows model with Spearman distance
#' n <- 10
#' N <- 100
#' set.seed(12345)
#' rankings <- rMSmix(sample_size = N, n_items = n)$samples
#' # Censoring in arbitrary positions with assigned number of ranks to be retained
#' set.seed(12345)
#' nranked <- round(runif(N,0.5,1)*n)
#' set.seed(12345)
#' arbitr_ranks1 <- data_censoring(rankings, topk = FALSE, nranked = nranked)
#' arbitr_ranks1
#' identical(arbitr_ranks1$nranked, nranked)
#' # Censoring in arbitrary positions with random number of ranks to be retained
#' set.seed(12345)
#' probs <- runif(n-1, 0, 0.5)
#' set.seed(12345)
#' arbitr_ranks2 <- data_censoring(rankings, topk = FALSE, probs = probs)
#' arbitr_ranks2
#' prop.table(table(arbitr_ranks2$nranked))
#' round(prop.table(probs), 2)
#'
#' @export
#'
#'
data_censoring <- function(rankings, topk = TRUE, nranked = NULL, probs = rep(1,ncol(rankings) - 1)){

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }

  if (any(is.na(rankings))) {
    stop("The input 'rankings' must contain full rankings in each row.\n")
  }


  if(topk){
    out = topk_cens(rankings=rankings, nranked=nranked, probs=probs)
  }else{
    out = arbitrary_cens(rankings=rankings, nranked=nranked, probs=probs)
  }
  return(list(part_rankings=out$partialdata,nranked=out$nranked))
}



# spear_dist ----
#' Spearman distance
#'
#' @description Compute either the Spearman distance between each row of a full ranking matrix and a reference complete ranking, or the Spearman distance matrix between all pairs of full rankings.
#'
#' @details When \code{rho = NULL}, \code{spear_dist} recalls the \code{\link{dist}} function from the \code{base} package to compute the Spearman metric as squared Euclidian distance between all pairs of rows in \code{rankings}; otherwise, it recalls the \code{compute_rank_distance} routine of the \code{BayesMallows} package for the computation of the Spearman distance between each row in \code{rankings} and the full ranking \code{rho}.
#'
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with full rankings in each row.
#' @param rho An optional full ranking whose Spearman distance from each row in \code{rankings} must be computed. Defaults to \code{NULL}, meaning that the Spearman distance matrix between all pairs of rows in \code{rankings} must be computed.
#' @param subset Optional logical or integer vector specifying the subset of observations, i.e. rows of the \code{rankings}, to be kept. Missing values are taken as \code{FALSE}. Defaults to \code{NULL} meaning that all the rows are considered.
#' @param diag Logical: whether the diagonal of the Spearman distance matrix must be returned. Used when \code{rho = NULL}. Defaults to \code{FALSE}.
#' @param upper Logical: whether the upper triangle of the Spearman distance matrix must be printed. Used when \code{rho = NULL}. Defaults to \code{FALSE}.
#' @param plot_dist_mat Logical: whether the Spearman distance matrix must be plotted. Used when \code{rho = NULL}. Defaults to \code{FALSE}.
#'
#'
#' @return When \code{rho = NULL}, an object of class \code{"dist"} corresponding to the Spearman distance matrix; otherwise, a vector with the Spearman distances between each row in \code{rankings} and the full ranking \code{rho}.
#'
#' @references
#' Sørensen Ø, Crispino M, Liu Q and Vitelli V (2020). BayesMallows: An R Package for the Bayesian Mallows Model. \emph{The R Journal}, \bold{12}(1), pages 324--342, DOI: 10.32614/RJ-2020-026.
#'
#' @seealso  \code{\link{plot.dist}}
#'
#' @examples
#'
#' ## Example 1. Spearman distance between two full rankings.
#' spear_dist(rankings = c(4, 8, 6, 9, 2, 11, 3, 5, 1, 12, 7, 10), rho = 1:12)
#'
#' ## Example 2. Spearman distance between the Antifragility ranking dataset and the Borda ranking.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' borda <- rank(data_description(rankings = r_antifrag)$mean_rank)
#' spear_dist(rankings = r_antifrag, rho = borda)
#'
#' ## Example 3. Spearman distance matrix of the Sports ranking dataset.
#' r_sports <- ranks_sports[, 1:8]
#' dist_mat <- spear_dist(rankings = r_sports)
#' dist_mat
#' # Spearman distance matrix for the subsample of females.
#' dist_f <- spear_dist(rankings = r_sports, subset = (ranks_sports$Gender == "Female"))
#' dist_f
#'
#' @export
#'
spear_dist <- function(rankings, rho = NULL, subset = NULL, diag = FALSE, upper = FALSE, plot_dist_mat = FALSE) {

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }

  if (!is.null(subset)) {
    if(is.logical(subset)){
      rankings <- rankings[(subset & !is.na(subset)),, drop = FALSE]
    }else{
      rankings <- rankings[na.omit(subset),, drop = FALSE]
    }
  }

  if (any(is.na(rankings))) {
    stop("The input 'rankings' must contain full rankings in each row.\n")
  }

  if (is.null(rho)) {
    out <- dist(x = rankings, diag = diag, upper = upper)^2
    if (plot_dist_mat) {
      ### DO NOT DELETE THIS MESSAGE: print needed to allow that plot works inside the function!
      print(plot(out))
    } else {
      message("Use function plot() to visualize the object of class 'dist'.\n")
    }
  } else {

    if (any(is.na(rho))) {
      stop("The argument rho cannot contain NAs.\n")
    }
    out <- compute_rank_distance(rankings = rankings, rho = rho, metric = "spearman")
  }
  return(out)
}


# plot.dist ----
#' Plot the Spearman distance matrix
#'
#' @description \code{plot} method for class \code{"dist"}. It displays a heatmap which is useful to preliminarily explore the presence of patterns (groups) of similar preferences in the ranking dataset.
#'
#' @details \code{plot.dist} can visualize a distance matrix of any metric, provided that its class is \code{"dist"}. It can take a few seconds if the size of the distance matrix is large.
#'
#' The heatmap can be also obtained by setting the arguments \code{rho = NULL} and \code{plot_dist_mat = TRUE} when applying \code{\link{spear_dist}}.
#'
#' @param x An object of class \code{"dist"}, returned by \code{\link{spear_dist}} when setting the argument \code{rho = NULL}.
#' @param order Logical: whether the rows of the distance matrix must be ordered. Defaults to \code{TRUE}.
#' @param show_labels Logical: whether the labels must be displayed on the axes. Defaults to \code{TRUE}.
#' @param lab_size Positive scalar: the magnification of the labels on the axes. Defaults to 3.
#' @param gradient List of three elements with the colors for low, mid and high values of the distances in the heatmap. The element \code{mid} can take the value \code{NULL}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @return A heatmap of the Spearman distance matrix between all pairs of full rankings.
#'
#' @references
#'
#' Alboukadel K and Mundt F (2020). factoextra: Extract and Visualize the Results of Multivariate Data Analyses. R package version 1.0.7. \url{https://CRAN.R-project.org/package=factoextra}
#'
#'
#' @seealso \code{\link{spear_dist}}
#'
#' @examples
#'
#' ## Example 1. Plot the Spearman distance matrix of the Antifragility ranking dataset.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' dist_mat <- spear_dist(rankings = r_antifrag)
#' plot(dist_mat, show_labels = FALSE)
#'
#' ## Example 2. Plot the Spearman distance matrix of the Sports ranking dataset.
#' r_sports <- ranks_sports[, 1:8]
#' dist_mat <- spear_dist(rankings = r_sports)
#' plot(dist_mat, show_labels = FALSE)
#' # Plot the Spearman distance matrix for the subsample of males.
#' dist_m <- spear_dist(rankings = r_sports, subset = (ranks_sports$Gender == "Male"))
#' plot(dist_m)
#'
#'
#' @export plot.dist
#' @export
#'
plot.dist <- function(x, order = TRUE, show_labels = TRUE, lab_size = 3, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), ...) {

  dist_out <- x

  if (!is(dist_out, "dist")) {
    stop("The function requires an object of S3 class 'dist' as its first argument.\n")
  }

  oldpar <- par(mfrow = c(1, 1))
  on.exit(par(oldpar))

  fviz_dist(dist_out, order = order, show_labels = show_labels, lab_size = lab_size, gradient = gradient)

}



# spear_dist_distr ----
#' Spearman distance distribution under the uniform ranking model
#'
#' @description Provide (either the exact or the approximate) frequency distribution of the Spearman distance under the uniform (null) ranking model.
#'
#' @details
#' When \eqn{n\leq 20}, the exact distribution provided by OEIS Foundation Inc. (2023) is returned by relying on a call to the \code{get_cardinalities} routine of the \code{BayesMallows} package. When \eqn{n>20}, the approximate distribution introduced by Crispino et al. (2023) is returned. If \eqn{n>170}, the approximation is also restricted over a fixed grid of values for the Spearman distance to limit computational burden.
#'
#' @param n_items Number of items.
#' @param log Logical: whether the frequencies must be reported on the log scale. Defaults to \code{TRUE}.
#'
#' @return A list of two named objects:
#'
#'  \item{\code{distances}}{All the possible Spearman distance values (i.e., the support of the distribution).}
#'  \item{\code{logcard}}{(Log)-frequencies corresponding to each value in \code{distances}.}
#'
#' @references
#' OEIS Foundation Inc. (2023). The On-Line Encyclopedia of Integer Sequences, Published electronically at \url{https://oeis.org}.
#'
#' Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#' Sørensen Ø, Crispino M, Liu Q and Vitelli V (2020). BayesMallows: An R Package for the Bayesian Mallows Model. \emph{The R Journal}, \bold{12}(1), pages 324--342, DOI: 10.32614/RJ-2020-026.
#'
#' @seealso \code{\link{spear_dist}}, \code{\link{expected_spear_dist}}, \code{\link{partition_fun_spear}}
#'
#' @examples
#'
#' ## Example 1. Exact Spearman distance distribution for n=20 items.
#' distr <- spear_dist_distr(n_items = 20, log = FALSE)
#' plot(distr$distances,distr$logcard,type="l",ylab="Frequency",xlab="d",
#' main='Distribution of the Spearman distance\nunder the null model')
#'
#'
#' ## Example 2. Approximate Spearman distance distribution for n=50 items with log-frequencies.
#'distr <- spear_dist_distr(n_items = 50)
#'plot(distr$distances,distr$logcard,type="l",ylab="Log-frequency",xlab="d",
#'     main='Log-distribution of the Spearman distance\nunder the null model')
#'
#' @export
#'
spear_dist_distr <- function(n_items, log = TRUE) {
  if (n_items <= 20) {
    pfd <- get_cardinalities(n_items = n_items, metric = "spearman")
    distances <- pfd$distance
    card <- pfd$value
    return(list(distances=distances, logcard=(if (log) log(card) else card)))
  } else {
    if (n_items <= 170) {
      message("Approximate frequency distribution of the Spearman distance")
      pfd <- get_log_card_approx_spearman(n_items)
    } else {
      message("Approximate frequency distribution of the Spearman distance over a fixed grid of values")
      pfd <- get_log_card_approx_spearman_grid(n_items)
    }

    logcardest <- pfd$logcard
    distances <- pfd$dist
    return(list(distances=distances, logcard=(if (log) logcardest else exp(logcardest))))
  }

}


# log_partition_fun_spear ----
#/' Utility for the exported partition_fun_spear
#/'
#/' Compute (either the exact or the approximate) logarithm of the partition function of the Mallows model with Spearman distance.
#/'
#/' @keywords internal
#/' @param theta Non-negative precision parameter.
#/' @param n_items Number of items.
#/'
#/' @return The log-partition function of the Mallows models with Spearman distance.
#/'
log_partition_fun_spear <- function(theta, n_items) {
  out <- suppressMessages(spear_dist_distr(n_items))
  logcardest <- out$logcard
  distances <- out$distances
  tmp <- logcardest - theta * distances
  tmp2 <- max(tmp)
  out <- log(sum(exp(tmp - tmp2))) + tmp2

  return(out)
}

# partition_fun_spear ----
#' Partition function of the Mallows model with Spearman distance
#'
#' @description Compute (either the exact or the approximate) (log-)partition function of the Mallows model with Spearman distance.
#'
#' @details
#' When \eqn{n\leq 20}, the partition function is exactly computed by relying on the Spearman distance distribution provided by OEIS Foundation Inc. (2023). When \eqn{n>20}, it is approximated with the method introduced by Crispino et al. (2023) and, if \eqn{n>170}, the approximation is also restricted over a fixed grid of values for the Spearman distance to limit computational burden.
#'
#' The partition function is independent of the consensus ranking of the Mallows model with Spearman distance due to the right-invariance of the metric. When \eqn{\theta=0}, the partition function is equivalent to \eqn{n!}, which is the normalizing constant of the uniform (null) model.
#'
#' @param theta Non-negative precision parameter.
#' @param n_items Number of items.
#' @param log Logical: whether the partition function must be returned on the log scale. Defaults to \code{TRUE}.
#'
#' @return Either the exact or the approximate (log-)partition function of the Mallows model with Spearman distance.
#'
#' @references
#' Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#' OEIS Foundation Inc. (2023). The On-Line Encyclopedia of Integer Sequences, Published electronically at \url{https://oeis.org}.
#'
#' @seealso \code{\link{spear_dist_distr}}, \code{\link{expected_spear_dist}}
#'
#' @examples
#'
#' ## Example 1. Partition function of the uniform (null) model, coinciding with n!.
#' partition_fun_spear(theta = 0, n_items = 10, log = FALSE)
#' factorial(10)
#'
#' ## Example 2. Partition function of the Mallows model with Spearman distance.
#' partition_fun_spear(theta = 0.5, n_items = 10, log = FALSE)
#'
#' ## Example 3. Log-partition function of the Mallows model with Spearman distance
#' ## as a function of theta.
#' partition_fun_spear_vec <- Vectorize(partition_fun_spear, vectorize.args = "theta")
#' curve(partition_fun_spear_vec(x, n_items = 10), from = 0, to = 0.1, lwd = 2,
#'   xlab = expression(theta), ylab = expression(log(Z(theta))),
#'   main = "Log-partition function of the Mallows model with Spearman distance",
#'   ylim = c(7, log(factorial(10))))
#'
#' ## Example 4. Log-partition function of the Mallows model with Spearman distance
#' ## for varying number of items and values of the concentration parameter.
#' partition_fun_spear_vec <- Vectorize(partition_fun_spear, vectorize.args = "theta")
#' curve(partition_fun_spear_vec(x, n_items = 10),
#'   from = 0, to = 0.1, lwd = 2, col = 2,
#'   xlab = expression(theta), ylab = expression(log(Z(theta))),
#'   main = "Log-partition function of the Mallows model with Spearman distance",
#'   ylim = c(0, log(factorial(30))))
#' curve(partition_fun_spear_vec(x, n_items = 20), add = TRUE, col = 3, lwd = 2)
#' curve(partition_fun_spear_vec(x, n_items = 30), add = TRUE, col = 4, lwd = 2)
#' legend("topright", legend = c(expression(n == 10), expression(n == 20), expression(n == 30)),
#'   col = 2:4, lwd = 2, bty = "n")
#'
#' @export
#'
#'
partition_fun_spear <- function(theta, n_items, log = TRUE) {

  if(log){
    out <- log_partition_fun_spear(theta = theta, n_items = n_items)
  }else{
    out <- exp(log_partition_fun_spear(theta = theta, n_items = n_items))
  }

  return(out)

}


# log_expect_spear_dist ----
#/' Utility for the exported expected_spear_dist
#/'
#/' Compute (either the exact or the approximate) log-expectation of the Spearman distance under the Mallows model with Spearman distance.
#/'
#/' @keywords internal
#/' @param theta Non-negative precision parameter.
#/' @param n_items Number of items.
#/'
#/' @return The log-expected value of the Spearman distance under the Mallows model with Spearman distance.
#/'
log_expect_spear_dist <- function(theta, n_items) {

  out <- suppressMessages(spear_dist_distr(n_items))
  logcardest <- out$logcard
  distances <- out$distances

  tmp <- max(logcardest + log(distances) - theta * distances)
  tmp2 <- max(logcardest - theta * distances)
  out <- log(sum(exp(logcardest + log(distances) - theta * distances - tmp))) - log(sum(exp(logcardest - theta * distances - tmp2))) + tmp - tmp2

  return(out)
}

# expected_spear_dist ----
#' Expectation of the Spearman distance
#'
#' @description Compute (either the exact or the approximate) (log-)expectation of the Spearman distance under the Mallows model with Spearman distance.
#'
#' @details
#' When \eqn{n\leq 20}, the expectation is exactly computed by relying on the Spearman distance distribution provided by OEIS Foundation Inc. (2023). When \eqn{n>20}, it is approximated with the method introduced by Crispino et al. (2023) and, if \eqn{n>170}, the approximation is also restricted over a fixed grid of values for the Spearman distance to limit computational burden.
#'
#' The expected Spearman distance is independent of the consensus ranking of the Mallows model with Spearman distance due to the right-invariance of the metric. When \eqn{\theta=0}, this is equal to \eqn{\frac{n^3-n}{6}}, which is the expectation of the Spearman distance under the uniform (null) model.
#'
#' @param theta Non-negative precision parameter.
#' @param n_items Number of items.
#' @param log Logical: whether the expected Spearman distance on the log scale must be returned. Defaults to \code{TRUE}.
#'
#' @return Either the exact or the approximate (log-)expected value of the Spearman distance under the Mallows model with Spearman distance.
#'
#' @references
#' Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#' OEIS Foundation Inc. (2023). The On-Line Encyclopedia of Integer Sequences, Published electronically at \url{https://oeis.org}.
#'
#' Kendall MG (1970). Rank correlation methods. 4th ed. Griffin London.
#'
#' @seealso \code{\link{spear_dist_distr}}, \code{\link{partition_fun_spear}}
#'
#' @examples
#'
#' ## Example 1. Expected Spearman distance under the uniform (null) model,
#' ## coinciding with (n^3-n)/6.
#' n_items <- 10
#' expected_spear_dist(theta = 0, n_items = n_items, log = FALSE)
#' (n_items^3-n_items)/6
#'
#' ## Example 2. Expected Spearman distance.
#' expected_spear_dist(theta = 0.5, n_items = 10, log = FALSE)
#'
#' ## Example 3. Log-expected Spearman distance as a function of theta.
#' expected_spear_dist_vec <- Vectorize(expected_spear_dist, vectorize.args = "theta")
#' curve(expected_spear_dist_vec(x, n_items = 10),
#'   from = 0, to = 0.1, lwd = 2, col = 2, ylim = c(3, 5.5),
#'   xlab = expression(theta), ylab = expression(log(E[theta](D))),
#'   main = "Log-expected Spearman distance")
#'
#' ## Example 4. Log-expected Spearman distance for varying number of items
#' # and values of the concentration parameter.
#' expected_spear_dist_vec <- Vectorize(expected_spear_dist, vectorize.args = "theta")
#' curve(expected_spear_dist_vec(x, n_items = 10),
#'   from = 0, to = 0.1, lwd = 2, col = 2, ylim = c(3, 9),
#'   xlab = expression(theta), ylab = expression(log(E[theta](D))),
#'   main = "Log-expected Spearman distance")
#' curve(expected_spear_dist_vec(x, n_items = 20), add = TRUE, col = 3, lwd = 2)
#' curve(expected_spear_dist_vec(x, n_items = 30), add = TRUE, col = 4, lwd = 2)
#' legend("topright", legend = c(expression(n == 10), expression(n == 20), expression(n == 30)),
#'   col = 2:4, lwd = 2, bty = "n")
#'
#' @export
#'
expected_spear_dist <- function(theta, n_items, log = TRUE) {

  if(log){
    out <- log_expect_spear_dist(theta = theta, n_items = n_items)
  }else{
    out <- exp(log_expect_spear_dist(theta = theta, n_items = n_items))
  }

  return(out)

}



# var_spear_dist ----
#' Variance of the Spearman distance
#'
#' @description Compute (either the exact or the approximate) (log-)variance of the Spearman distance under the Mallows model with Spearman distance.
#'
#' @details
#' When \eqn{n\leq 20}, the variance is exactly computed by relying on the Spearman distance distribution provided by OEIS Foundation Inc. (2023). When \eqn{n>20}, it is approximated with the method introduced by Crispino et al. (2023) and, if \eqn{n>170}, the approximation is also restricted over a fixed grid of values for the Spearman distance to limit computational burden.
#'
#' The variance of the Spearman distance is independent of the consensus ranking of the Mallows model with Spearman distance due to the right-invariance of the metric. When \eqn{\theta=0}, this is equal to \eqn{\frac{n^2(n-1)(n+1)^2}{36}}, which is the variance of the Spearman distance under the uniform (null) model.
#'
#' @param theta Non-negative precision parameter.
#' @param n_items Number of items.
#' @param log Logical: whether the expected Spearman distance on the log scale must be returned. Defaults to \code{TRUE}.
#'
#' @return Either the exact or the approximate (log-)variance of the Spearman distance under the Mallows model with Spearman distance.
#'
#' @references
#' Crispino M., Mollica C., Astuti V. and Tardella L. (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#' OEIS Foundation Inc. (2023). The On-Line Encyclopedia of Integer Sequences, Published electronically at \url{https://oeis.org}
#'
#' Kendall, M. G. (1970). Rank correlation methods. 4th ed. Griffin London.
#'
#' @examples
#'
#' ## Example 1. Variance of the Spearman distance under the uniform (null) model,
#' ## coinciding with n^2(n-1)(n+1)^2/36.
#' n_items <- 10
#' var_spear_dist(theta = 0, n_items= n_items, log = FALSE)
#' n_items^2*(n_items-1)*(n_items+1)^2/36
#'
#' ## Example 2. Variance of the Spearman distance.
#' var_spear_dist(theta = 0.5, n_items = 10, log = FALSE)
#'
#' ## Example 3. Log-variance of the Spearman distance as a function of theta.
#' var_spear_dist_vec <- Vectorize(var_spear_dist, vectorize.args = "theta")
#' curve(var_spear_dist_vec(x, n_items = 10),
#'   from = 0, to = 0.1, lwd = 2, col = 2,
#'   xlab = expression(theta), ylab = expression(log(V[theta](D))),
#'   main = "Log-variance of the Spearman distance")
#'
#' ## Example 4. Log-variance of the Spearman distance for varying number of items
#' # and values of the concentration parameter.
#' var_spear_dist_vec <- Vectorize(var_spear_dist, vectorize.args = "theta")
#' curve(var_spear_dist_vec(x, n_items = 10),
#'   from = 0, to = 0.1, lwd = 2, col = 2, ylim = c(5, 14),
#'   xlab = expression(theta), ylab = expression(log(V[theta](D))),
#'   main = "Log-variance of the Spearman distance")
#' curve(var_spear_dist_vec(x, n_items = 20), add = TRUE, col = 3, lwd = 2)
#' curve(var_spear_dist_vec(x, n_items = 30), add = TRUE, col = 4, lwd = 2)
#' legend("topright", legend = c(expression(n == 10), expression(n == 20), expression(n == 30)),
#'   col = 2:4, lwd = 2, bty = "n")
#'
#' @export
#'
var_spear_dist <- function(theta,n_items,log=TRUE){

  if(theta==0){
    ed2 = n_items^2*(n_items-1)*(n_items+1)^2/36
  }else{

    out <- suppressMessages(spear_dist_distr(n_items))
    logcardest <- out$logcard
    distances <- out$distances

    tmp <- max(logcardest + 2*log(distances) - theta * distances)
    tmp2 <- max(logcardest - theta * distances)
    out <- log(sum(exp(logcardest + 2*log(distances) - theta * distances - tmp))) -
      log(sum(exp(logcardest - theta * distances - tmp2))) + tmp - tmp2

    ed2 = exp(out) - expected_spear_dist(theta,n_items,log=FALSE)^2

  }
  return(if (log) log(ed2) else ed2)
}

# paired_comparisonsMSmix ----
#/' Utility for the exported data_description
#/'
#/' Construct the paired comparison matrix for a partial ranking dataset.
#/'
#/' @keywords internal
#/' @param rank_data Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with partial rankings in each row.
#/'
#/' @return Integer \eqn{n}\eqn{\times}{x}\eqn{n} paired comparison matrix: the \eqn{(i,i')}-th entry indicates the number of sample units that preferred item \eqn{i} to item \eqn{i'}.
#/'
paired_comparisonsMSmix <- function(rank_data) {

  N <- nrow(rank_data)
  n_items <- ncol(rank_data)

  ord_data <- data_conversion(rank_data)
  pc <- tau(pi_inv = ord_data)
  return(pc)
} # n_items*n_items matrix


# itemwise_rank_marginals ----
#/' Utility for the exported plot.bootMSmix
#/'
#/' Compute the first-order marginals (marginal rank distributions of each item) for the bootstrap samples of a component-specific consensus ranking.
#/'
#/' Differently from the matrix \code{marginals} returned by the \code{data_description} function and reporting the absolute counts, the relative frequency distributions are computed.
#/'
#/' @keywords internal
#/' @param rankings Integer matrix of bootstrap samples for a component-specific consensus ranking parameter or, more generally, a ranking matrix.
#/' @param item_names Character vector with the names to be used for the items. Defaults to \code{NULL}, meaning that \code{colnames(rankings)} is used and, if not available, \code{item_names} is set equal to \code{"Item1","Item2",...}.
#/' @param borda_ord Logical: whether the items (columns of the first-order marginal matrix) must be ordered according to the Borda ranking (i.e., mean rank vector). Defaults to \code{FALSE}.
#/' @param subset Optional logical or integer vector specifying the subset of rows of \code{rankings} to be kept. Missing values are taken as \code{FALSE}. Defaults to \code{NULL} meaning that all the rows are considered.
#/'
#/' @return Numeric \eqn{n}\eqn{\times}{x}\eqn{n} matrix of the (relative) first-order marginals in each column: the \eqn{(j,i)}-th entry indicates the relative frequency of times that item \eqn{i} is ranked in position \eqn{j}.
#/'
itemwise_rank_marginals <- function(rankings, item_names = NULL, borda_ord = TRUE, subset = NULL) {

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }

  if (!is.null(subset)) {
    if(is.logical(subset)){
      rankings <- rankings[(subset & !is.na(subset)),, drop = FALSE]
    }else{
      rankings <- rankings[na.omit(subset),, drop = FALSE]
    }
  }

  rankings <- suppressWarnings(fill_single_entries_new(data = rankings))
  N <- nrow(rankings)
  n_items <- ncol(rankings)

  marginals <- apply(rankings, 2, tabulate, nbins = n_items)
  marginals_rel <- prop.table(marginals, margin = 2)
  rownames(marginals_rel) <- paste0("Rank", 1:n_items)

  if (is.null(item_names)) {
    item_names <- colnames(marginals_rel) <- colnames(rankings)
    if (is.null(item_names)) {
      colnames(marginals_rel) <- paste0("Item", 1:n_items)
    }
  } else {
    colnames(marginals_rel) <- item_names
  }


  if (borda_ord) {
    marginals_rel <- marginals_rel[, order(colMeans(rankings, na.rm = TRUE)), drop = FALSE]
  }

  return(marginals_rel)
}


# itemwise_rank_hdi ----
#/' Utility for the exported bootstrapMSmix
#/'
#/' Compute the highest density interval of each item for the bootstrap samples of a component-specific consensus ranking.
#/'
#/' @keywords internal
#/' @param rankings Integer matrix of bootstrap samples for a component-specific consensus ranking parameter or, more generally, a ranking matrix.
#/' @param item_names Character vector with the names to be used for the items. Defaults to \code{NULL}, meaning that \code{colnames(rankings)} is used and, if not available, \code{item_names} is set equal to \code{"Item1","Item2",...}.
#/' @param prob_level Numeric: value in the interval (0,1] indicating the desired probability level of the highest density sets. Defaults to 0.95.
#/' @param subset Optional logical or integer vector specifying the subset of rows of \code{rankings} to be kept. Missing values are taken as \code{FALSE}. Defaults to \code{NULL} meaning that all the rows are considered.
#/'
#/' @return A data frame with columns containing the highest density interval of each item.
#/'
itemwise_rank_hdi <- function(rankings, item_names = NULL, prob_level = 0.95, subset = NULL) {

  if(prob_level <= 0 | prob_level > 1){
    stop("The argument 'prob_level' must a value in the interval (0,1].")
  }

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }


  if (!is.null(subset)) {
    if(is.logical(subset)){
      rankings <- rankings[(subset & !is.na(subset)),, drop = FALSE]
    }else{
      rankings <- rankings[na.omit(subset),, drop = FALSE]
    }
  }

  rankings <- suppressWarnings(fill_single_entries_new(data = rankings))
  n_items <- ncol(rankings)
  out <- apply(rankings, 2, rank_hdi, n_items = n_items, prob_level = prob_level)

  if (is.null(item_names)) {
    item_names <- names(out) <- colnames(rankings)
    if (is.null(item_names)) {
      names(out) <- paste0("Item", 1:n_items)
    }
  } else {
    names(out) <- item_names
  }


  out <- data.frame(HPD_set = out)

  return(out)
}


# rank_hdi ----
#/' Utility for the internal itemwise_rank_hdi
#/'
#/' Compute the highest density interval of an item from its marginal rank distribution.
#/'
#/' @keywords internal
#/' @param ranks Integer vector containing the ranks of a given item.
#/' @param n_items Number of items.
#/' @param prob_level Numeric: value in the interval (0,1] indicating the desired probability level of the highest density sets. Defaults to 0.95.
#/'
#/' @return String indicating the highest density interval of a given item.
#/'
rank_hdi <- function(ranks, n_items, prob_level) {

  prop <- prop.table(table(factor(ranks, levels = 1:n_items)))
  cum <- cumsum(sort(prop))
  alpha <- 1 - prob_level


  ranks_to_drop <- as.numeric(names(cum)[which(cum < alpha)])


  ranks_to_keep_tmp = setdiff(1:n_items,ranks_to_drop)
  tied_prop_min=(prop==min(prop[ranks_to_keep_tmp]))

  if(any(tied_prop_min)){
    tied_ranks=as.numeric(names(prop)[tied_prop_min])
    ranks_to_keep=sort(union(ranks_to_keep_tmp,tied_ranks))
  }else{
    ranks_to_keep=ranks_to_keep_tmp
  }

  hdi <- paste0("{", paste0(ranks_to_keep, sep = ",", collapse = ""), "}")


  hdi <- gsub(",}", "}", hdi)

  return(hdi)
}


# f1_estn ----
#/' Utility for the internal estn
#/'
#/' Compute the estimated probabilities of the distinct FULL ranking compatible with a given partial sequence in \code{rankings_part} for the EM algorithm based on \code{data_augmentation} in the homogeneous case (\eqn{G=1}).
#/'
#/' @keywords internal
#/' @param elem Integer matrix of full rankings compatible with a given partial sequence in \code{rankings_part}.
#/' @param freq_part Frequency of a given partial sequence in \code{rankings_part}.
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the current value of the component-specific consensus rankings in each row.
#/' @param theta Numeric vector with the current value of the \eqn{G} component-specific precision parameters.
#/'
#/' @return Numeric vector with the estimated probabilities of the distinct full ranking compatible with a given partial sequence in \code{rankings_part}.
#/'
f1_estn <- function(elem,freq_part,rho,theta){
  tmp <- -theta * compute_rank_distance(elem, rho, "spearman")
  gr <- exp((tmp - max(tmp)))
  gr <- prop.table(gr)
  nx <- freq_part * gr
  return(nx)
} # case G=1: vector of length M=number of complete rankings
# compatible with a given partial ranking, whose generic entry is the term of the
# sum in eq. (9) of paper published on SC (2023)


# f2_estn ----
#/' Utility for the internal estn
#/'
#/' Compute the estimated probabilities of the distinct FULL ranking compatible with a given partial sequence in \code{rankings_part} for the EM algorithm based on \code{data_augmentation} in the heterogeneous case (\eqn{G>1}).
#/'
#/' @keywords internal
#/' @param elem Integer matrix of full rankings compatible with a given partial sequence in \code{rankings_part}.
#/' @param freq_part Frequency of a given partial sequence in \code{rankings_part}.
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the current value of the component-specific consensus rankings in each row.
#/' @param theta Numeric vector with the current value of the \eqn{G} component-specific precision parameters.
#/' @param weights Numeric vector with the current value of the \eqn{G} mixture weights.
#/' @param logZ Numeric vector with the current value of the partition function in the \eqn{G} component-specific precision parameters.
#/'
#/' @return Numeric vector with the estimated probabilities of the distinct full ranking compatible with a given partial sequence in \code{rankings_part}.
#/'
f2_estn <- function(elem,freq_part,rho,theta,weights,logZ){

  if (nrow(elem) == 1) {
    tmp2dist <- matrix(apply(rho, 1, compute_rank_distance, rankings = elem, metric = "spearman"), ncol = 1)
  } else { # 1*M matrix
    tmp2dist <- t(apply(rho, 1, compute_rank_distance, rankings = elem, metric = "spearman"))
  } # G*M matrix

  tmp <- -theta * tmp2dist + log(weights) - logZ # G*M matrix
  gr <- exp(tmp - max(tmp))
  gr <- colSums(gr) # vector of length M
  gr <- prop.table(gr)
  nx <- freq_part * gr
  return(nx)
} # case G>1: vector of length M=number of complete rankings
# compatible with a given partial ranking, whose generic entry is the term of the
# sum in eq. (9) of paper published on SC (2023)

# estn ----
#/' Utility for the internal em_db_mix and the exported fitMSmix
#/'
#/' Compute the estimated frequencies of each \eqn{M} distinct FULL ranking for the EM algorithm based on \code{data_augmentation}.
#/'
#/' @keywords internal
#/' @param theta Numeric vector with the current value of the \eqn{G} component-specific precision parameters.
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the current value of the component-specific consensus rankings in each row.
#/' @param weights Numeric vector with the current value of the \eqn{G} mixture weights.
#/' @param aug_list A list with elements corresponding to the matrices of full rankings compatible with the partial sequences in \code{rankings_part}.
#/' @param aug_mat Integer matrix of full rankings with rows given by the stacked matrices in \code{aug_list}.
#/' @param aug_mat_vec Character vector with elements corresponding to the rows of \code{aug_mat} converted into strings.
#/' @param freq_part Integer vector with the frequencies of partial sequences in \code{rankings_part}.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/'
#/' @return Numeric vector with the \eqn{M} estimated frequencies of each distinct full ranking compatible with the partial sequences in \code{rankings_part}.
#/'
estn <- function(theta, rho, weights,
                 aug_list, aug_mat, aug_mat_vec,
                 freq_part, cardinalities) {

  N <- length(aug_list)
  n_clust <- length(theta)
  n_items <- ncol(aug_mat)

  if (n_clust == 1) {
    nhat_tmp <- unlist(lapply(1:N, function(x) f1_estn(aug_list[[x]], freq_part[x],rho,theta)))
  } else {
    distances <- cardinalities$distances
    logcardest <- cardinalities$logcard
    logZ <- sapply(theta, log_part_funct_spear_hide, distances = distances, logcardest = logcardest)
    nhat_tmp <- unlist(lapply(1:N, function(x) f2_estn(aug_list[[x]], freq_part[x],rho,theta,weights,logZ)))
  }

  aug_data_table <- data.table(amv=aug_mat_vec, nhat_tmp = nhat_tmp)
  freq_compl_est <- aug_data_table[, .(nhat = sum(nhat_tmp)), by = amv]
  names(freq_compl_est)[1 + 1] <- "nhat"
  freq_compl_est <- freq_compl_est$nhat

  return(freq_compl_est)
}

# get_log_card_approx_spearman ----
#/' Utility for the exported spear_dist_distr
#/'
#/' Compute the approximate log-frequency distribution of the Spearman distance in the case when \eqn{n>20} and \eqn{n<=170}.
#/'
#/' @keywords internal
#/' @param n_items Number of items.
#/'
#/' @return A list of two named objects: 1) \code{logcard}: numeric vector of log-frequencies corresponding to each value in \code{dist}; 2) \code{dist}: numeric vector of all the possible Spearman distance values.
#/'
get_log_card_approx_spearman <- function (n_items) {
  log_n <- lgamma(n_items + 1)
  a0 <- n_items * (-0.24/sqrt(n_items))
  a1 <- n_items * (1/3 - 0.1784/sqrt(n_items))
  a2 <- n_items * (log(2) * 8/3 - 5.5241/sqrt(n_items))
  maxd <- 2 * choose(n_items + 1, 3)
  distances <- seq(0, maxd, 2)
  n_distances <- length(distances)
  logcardest <- rep(NA, n_distances)
  if (n_items > 3) {
    logcardest[1] <- logcardest[n_distances] <- 0
    logcardest[2] <- logcardest[n_distances - 1] <- log(n_items - 1)
    logcardest[3] <- logcardest[n_distances - 2] <- log(choose(n_items -  2, 2))
    logcardest[4] <- logcardest[n_distances - 3] <- log(((n_items - 2)^3)/6 - (n_items - 2)^2 + 23 * (n_items - 2)/6 - 1)
    dchosen <- distances[5:(n_distances - 4)]

    x_n <- (dchosen + 1)/(maxd + 2)
    tmp <- exp(log_n) - sum(exp(logcardest), na.rm = TRUE)
    tmp2 <- log_n + a0 + a1 * log(x_n * (1 - x_n)) + a2 * x_n * (1 - x_n)

    logcardest[5:(n_distances - 4)] <- log(tmp) + tmp2 - log(sum(exp(tmp2)))

  }
  else if (n_items == 2) {
    logcardest[1] <- logcardest[2] <- 0
  }
  else if (n_items == 3) {
    logcardest[1] <- logcardest[5] <- 0
    logcardest[2] <- logcardest[4] <- log(n_items - 1)
    logcardest[3] <- log(choose(n_items - 2, 2))
  }
  return(list(logcard = logcardest, dist = distances))
}

# get_log_card_approx_spearman_grid ----
#/' Utility for the exported spear_dist_distr
#/'
#/' Compute the approximate log-frequency distribution of the Spearman distance over a fixed grid of \eqn{10^6} distance values. This is applied when \eqn{n>170}, to limit the computational burden and memory space needed when \eqn{n} is very high.
#/'
#/' @keywords internal
#/' @param n_items Number of items.
#/'
#/' @return A list of two named objects: 1) \code{logcard}: numeric vector of log-frequencies corresponding to each value in \code{dist}; 2) \code{dist}: numeric vector of all the possible Spearman distance values.
#/'
get_log_card_approx_spearman_grid <- function (n_items) {
  n_fac <- factorialZ(n_items)
  a0 <- n_items * (-0.24/sqrt(n_items))
  a1 <- n_items * (1/3 - 0.1784/sqrt(n_items))
  a2 <- n_items * (log(2) * 8/3 - 5.5241/sqrt(n_items))
  maxd <- 2 * choose(n_items + 1, 3)
  n_distances <- 10^6

  logcardest <- rep(NA, n_distances)
  logcardest[1] <- logcardest[n_distances] <- 0
  logcardest[2] <- logcardest[n_distances - 1] <- log(n_items - 1)
  logcardest[3] <- logcardest[n_distances - 2] <- log(choose(n_items -  2, 2))
  logcardest[4] <- logcardest[n_distances - 3] <- log(((n_items - 2)^3)/6 - (n_items - 2)^2 + 23 * (n_items - 2)/6 - 1)

  distances <- round(seq(4, (maxd-8)/2, length.out = n_distances-8))*2

  x_n <- (distances + 1)/(maxd + 2)
  tmp <- n_fac - sum(exp(logcardest), na.rm = TRUE)
  tmp2 <- log(n_fac) + a0 + a1 * log(x_n * (1 - x_n)) + a2 * x_n * (1 - x_n)

  logcardest[5:(n_distances-4)]  <-  log(tmp) + tmp2 - log(sum(exp(tmp2-max(tmp2))))-max(tmp2)

  return(list(logcard = logcardest, dist = c(0,2,4,6,distances,maxd-6,maxd-4,maxd-2,maxd)))

}



# log_part_funct_spear_hide ----
#/' Utility for the internal log_lik_inter_spearman and estn
#/'
#/' Compute (either the exact or the approximate) logarithm of the partition function of the Mallows model with Spearman distance.
#/'
#/' Although this internal routine parallels the utility \code{log_partition_fun_spear}, it allows to speed up the iterative log-likelihood evaluation and E-step in the EM algorithm.
#/'
#/' @keywords internal
#/' @param theta Non-negative precision parameter.
#/' @param distances Numeric vector of all the possible Spearman distance values.
#/' @param logcardest Numeric vector of log-frequencies corresponding to each value in \code{distances}.
#/'
#/' @return The log-partition function of the Mallows models with Spearman distance.
#/'
log_part_funct_spear_hide <- function(theta, distances, logcardest) {
  tmp <- logcardest - theta * distances
  tmp2 <- max(tmp)
  z <- log(sum(exp(tmp - tmp2))) + tmp2
  return(z)
} # scalar


# log_lik_inter_spearman ----
#/' Utility for several likelihood-related internal functions
#/'
#/' Compute the log-likelihood values for each distinct COMPLETE ranking under each component-specific (basic) Mallows model with Spearman distance.
#/'
#/' This routine allows to speed up the iterative log-likelihood evaluation in the EM algorithm.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#/' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#/' @param rankings Integer matrix with full rankings in each row, whose content depends on the type of originally observed ranking data (full or partial) and the selected EM algorithm.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/'
#/' @return Numeric matrix of log-likelihood values of each distinct full ranking under each mixture component.
#/'
log_lik_inter_spearman <- function(rho, theta, rankings, cardinalities) {
  n_items <- ncol(rankings)
  cn <- n_items * (n_items + 1) * (2 * n_items + 1) / 6

  distances <- cardinalities$distances
  logcardest <- cardinalities$logcard
  logZ <- sapply(theta, log_part_funct_spear_hide, distances = distances,
                 logcardest = logcardest)

  temp <- -2 * theta * (cn - rho %*% t(rankings)) - logZ
  return(temp)
} # n_cluster*nrow(rankings) matrix


# log_lik_db_mix ----
#/' Utility for the internal em_db_mix
#/'
#/' Compute the log-likelihood value for the parameters of a mixture of Mallows models with Spearman distance on COMPLETE rankings ONLY.
#/'
#/' This routine allows to speed up the iterative log-likelihood evaluation in the EM algorithm.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#/' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#/' @param weights Numeric vector of \eqn{G} positive mixture weights (normalization is not necessary).
#/' @param rankings Integer matrix with full rankings in each row, whose content depends on the type of originally observed ranking data (full or partial) and the selected EM algorithm.
#/' @param freq_compl Integer vector with the (observed or estimated) frequencies of each distinct full ranking.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/'
#/' @return The log-likelihood value.
#/'
log_lik_db_mix <- function(rho, theta, weights, rankings, freq_compl, cardinalities) {
  temp <- log_lik_inter_spearman(
    rho = rho, theta = theta, rankings = rankings,
    cardinalities = cardinalities
  ) # n_cluster*L matrix
  tmp <- max(temp)
  log_lik <- c(freq_compl %*% t(tmp + log(weights %*% exp(temp - tmp))))
  return(log_lik)
} # scalar

# log_lik_db_mix_partial ----
#/' Utility for the internal lik_partialMSmix and the log-likelihood evaluation in the EM algorithm.
#/'
#/' Compute the log-likelihood value for the parameters of a mixture of Mallows models with Spearman distance on PARTIAL rankings ONLY. The partial observations are augmented with the set of all possible compatible full rankings to allow the computation of the marginal likelihood.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#/' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#/' @param weights Numeric vector of \eqn{G} positive mixture weights.
#/' @param aug_list A list with elements corresponding to the matrices of full rankings compatible with each distinct partial sequence.
#/' @param freq_part Integer vector with the \eqn{L} observed frequencies of each distinct partial ranking.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/'
#/' @return The log-likelihood value.
#/'
log_lik_db_mix_partial <- function(rho,
                                   theta,
                                   weights,
                                   aug_list,
                                   freq_part,
                                   cardinalities) {
  temp <- lapply(aug_list, function(x) {
    weights %*% exp(log_lik_inter_spearman(
      rho = rho, theta = theta, rankings = x,
      cardinalities = cardinalities
    ))
  })

  log_lik_partial <- c(freq_part %*% log(sapply(temp, sum)))
  return(log_lik_partial)
} # scalar

# lik_partialMSmix ----
#/' Utility for the exported likMSmix
#/'
#/' Compute the (log-)likelihood value for the parameters of a mixture of Mallows models with Spearman distance on PARTIAL rankings ONLY. The partial observations are augmented with the set of all possible compatible full rankings to allow the computation of the marginal likelihood.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#/' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#/' @param weights Numeric vector of \eqn{G} positive mixture weights.
#/' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with the individual partial sequences in each row.
#/' @param log Logical: whether the log-likelihood must be returned.
#/'
#/' @return The (log)-likelihood value.
#/'
lik_partialMSmix <- function(rho, theta, weights, rankings, log = TRUE) {
  n_items <- ncol(rankings)
  uniranks <- frequence(rankings)
  freq_part <- uniranks[, n_items + 1]
  rankings_part <- uniranks[, 1:n_items, drop = FALSE]
  aug_list <- try(data_augmentation(rankings_part, fill_single_na = FALSE), silent = TRUE)

  if (inherits(aug_list, "try-error")) {
    stop("Data augmentation not carried out because some partial rankings have too many missing entries.\n")
  }

  cardinalities <- suppressMessages(spear_dist_distr(n_items))

  if (log) {
    out <- log_lik_db_mix_partial(
      rho = rho, theta = theta, weights = weights,
      aug_list = aug_list,
      freq_part = freq_part,
      cardinalities = cardinalities
    )
  } else {
    out <- exp(log_lik_db_mix_partial(
      rho = rho, theta = theta, weights = weights,
      aug_list = aug_list,
      freq_part = freq_part,
      cardinalities = cardinalities
    ))
  }

  return(out)
} # scalar

# LoglikInterMSmix ----
#/' Utility for the internal LoglikMSmix
#/'
#/' Compute the log-likelihood values for each distinct COMPLETE ranking under each component-specific (basic) Mallows model with Spearman distance.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#/' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#/' @param rankings Integer matrix with the distinct full sequences in each row.
#/'
#/' @return Numeric matrix of log-likelihood values of each distinct full ranking under each mixture component.
#/'
LoglikInterMSmix <- function(rho, theta, rankings) {
  n_items <- ncol(rankings)
  cn <- n_items * (n_items + 1) * (2 * n_items + 1) / 6
  logZ <- sapply(theta, partition_fun_spear, n_items = n_items)
  temp <- -2 * theta * (cn - rho %*% t(rankings)) - logZ
  return(temp)
} # n_cluster*nrow(rankings) matrix


# LoglikMSmix ----
#/' Utility for the internal lik_completeMSmix
#/'
#/' Compute the log-likelihood value for the parameters of a mixture of Mallows models with Spearman distance on COMPLETE rankings ONLY.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#/' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#/' @param weights Numeric vector of \eqn{G} positive mixture weights.
#/' @param rankings Integer matrix with the distinct full sequences in each row.
#/' @param obs_freq Integer vector with the observed frequencies of each distinct full ranking.
#/'
#/' @return The log-likelihood value.
#/'
LoglikMSmix <- function(rho, theta, weights, rankings, obs_freq) {
  temp <- LoglikInterMSmix(rho = rho, theta = theta, rankings = rankings) # n_cluster*nrow(rankings) matrix
  tmp <- max(temp)
  log_lik <- c(obs_freq %*% t(tmp + log(weights %*% exp(temp - tmp))))
  return(log_lik)
} # scalar

# lik_completeMSmix ----
#/' Utility for the exported likMSmix
#/'
#/' Compute the (log-)likelihood for the parameters of a mixture of Mallows models with Spearman distance on COMPLETE rankings ONLY.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#/' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#/' @param weights Numeric vector of \eqn{G} positive mixture weights.
#/' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix of the individual full sequences in each row.
#/' @param log Logical: whether the log-likelihood must be returned.
#/'
#/' @return The (log)-likelihood value.
#/'
lik_completeMSmix <- function(rho, theta, weights, rankings, log = TRUE) {
  n_items <- ncol(rankings)
  uniranks <- frequence(rankings)
  obs_freq <- uniranks[, n_items + 1]
  rankings <- uniranks[, 1:n_items, drop = FALSE]

  if (log) {
    out <- LoglikMSmix(
      rho = rho, theta = theta, weights = weights,
      rankings = rankings, obs_freq = obs_freq
    )
  } else {
    out <- exp(LoglikMSmix(
      rho = rho, theta = theta, weights = weights,
      rankings = rankings, obs_freq = obs_freq
    ))
  }

  return(out)
} # scalar

# likMSmix ----
#' (Log-)likelihood for mixtures of Mallows models with Spearman distance
#'
#' @description Compute the (log-)likelihood for the parameters of a mixture of Mallows models with Spearman distance on partial rankings.
#' Partial rankings with missing data in arbitrary positions are supported.
#'
#' @details
#' The (log-)likelihood evaluation is performed by augmenting the partial rankings with the set of all compatible full rankings (see \code{\link{data_augmentation}}), and then the marginal likelihood is computed.
#'
#' When \eqn{n\leq 20}, the (log-)likelihood is exactly computed. When \eqn{n>20}, the model normalizing constant is not available and is approximated with the method introduced by Crispino et al. (2023). If \eqn{n>170}, the approximation is also restricted over a fixed grid of values for the Spearman distance to limit computational burden.
#'
#' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#' @param weights Numeric vector of \eqn{G} positive mixture weights (normalization is not necessary).
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#' @param log Logical: whether the log-likelihood must be returned. Defaults to \code{TRUE}.
#'
#' @return The (log)-likelihood value.
#'
#' @references
#' Crispino M, Mollica C and Modugno L (2025+). MSmix: An R Package for clustering partial rankings via mixtures of Mallows Models with Spearman distance. \emph{(submitted)}
#'
#' Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#' @seealso \code{\link{bicMSmix}}, \code{\link{aicMSmix}}, \code{\link{data_augmentation}}
#'
#' @examples
#'
#' ## Example 1. Likelihood of a full ranking of n=5 items under the uniform (null) model.
#' likMSmix(rho = 1:5, theta = 0, weights = 1, rankings = c(3,5,2,1,4), log = FALSE)
#' # corresponds to...
#' 1/factorial(5)
#'
#' ## Example 2. Simulate rankings from a 2-component mixture of Mallows models
#' ## with Spearman distance.
#' set.seed(12345)
#' d_sim <- rMSmix(sample_size = 75, n_items = 8, n_clust = 2)
#' str(d_sim)
#' # Fit the true model.
#' rankings <- d_sim$samples
#' fit <- fitMSmix(rankings = rankings, n_clust = 2, n_start = 10)
#' # Compare log-likelihood values of the true parameter values and the MLE.
#' likMSmix(rho = d_sim$rho, theta = d_sim$theta, weights = d_sim$weights,
#'        rankings = d_sim$samples)
#' likMSmix(rho = fit$mod$rho, theta = fit$mod$theta, weights = fit$mod$weights,
#'        rankings = d_sim$samples)
#'
#' ## Example 3. Simulate rankings from a basic Mallows model with Spearman distance.
#' set.seed(12345)
#' d_sim <- rMSmix(sample_size = 25, n_items = 6)
#' str(d_sim)
#' # Censor data to be partial top-3 rankings.
#' rankings <- d_sim$samples
#' rankings[rankings>3] <- NA
#' # Fit the true model with data augmentation.
#' set.seed(12345)
#' fit <- fitMSmix(rankings = rankings, n_clust = 1, n_start = 10)
#' # Compare log-likelihood values of the true parameter values and the MLEs.
#' likMSmix(rho = d_sim$rho, theta = d_sim$theta, weights = d_sim$weights,
#'        rankings = d_sim$samples)
#' likMSmix(rho = fit$mod$rho, theta = fit$mod$theta, weights = fit$mod$weights,
#'        rankings = d_sim$samples)
#'
#' @export
#'
likMSmix <- function(rho, theta, weights=(if(length(theta)==1) NULL),
                     rankings, log = TRUE) {
  if (is.vector(rho)) {
    rho <- matrix(rho, nrow = 1)
  }

  if (any(theta < 0)) {
    stop("Precision parameters must be non-negative")
  }

  if (any(weights <= 0)) {
    stop("Mixture weights must be positive")
  } else {
    if (sum(weights) != 1) {
      weights <- prop.table(weights)
    }
  }

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }

  if (any(is.na(rankings))) {
    rankings <- fill_single_entries_new(data = rankings)
  }

  check_na <- is.na(rankings)

  if (any(rowSums(!check_na) == 0)) {
    stop("Some rankings have all NA entries and should be removed before performing the analysis.\n")
  }

  partial <- any(check_na)

  if (partial) {
    out <- lik_partialMSmix(rho = rho, theta = theta, weights = weights, rankings = rankings, log = log)
  } else {
    out <- lik_completeMSmix(rho = rho, theta = theta, weights = weights, rankings = rankings, log = log)
  }

  return(out)
} # scalar

# bicMSmix ----
#' BIC and AIC for mixtures of Mallows models with Spearman distance
#'
#' @description \code{bicMSmix} and \code{aicMSmix} compute, respectively, the Bayesian Information Criterion (BIC) and the Akaike Information Criterion (AIC) for a mixture of Mallows models with Spearman distance fitted on partial rankings.
#'
#' @details
#' The (log-)likelihood evaluation is performed by augmenting the partial rankings with the set of all compatible full rankings (see \code{\link{data_augmentation}}), and then the marginal likelihood is computed.
#'
#' When \eqn{n\leq 20}, the (log-)likelihood is exactly computed, otherwise it is approximated with the method introduced by Crispino et al. (2023). If \eqn{n>170}, the approximation is also restricted over a fixed grid of values for the Spearman distance to limit computational burden.
#'
#' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#' @param weights Numeric vector of \eqn{G} positive mixture weights (normalization is not necessary).
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#'
#' @return The BIC or AIC value.
#'
#'
#' @references
#' Crispino M, Mollica C and Modugno L (2025+). MSmix: An R Package for clustering partial rankings via mixtures of Mallows Models with Spearman distance. \emph{(submitted)}
#'
#' Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#' Schwarz G (1978). Estimating the dimension of a model. \emph{The Annals of Statistics}, \bold{6}(2), pages 461–464, DOI: 10.1002/sim.6224.
#'
#' @seealso \code{\link{likMSmix}}, \code{\link{data_augmentation}}
#'
#' @examples
#'
#' ## Example 1. Simulate rankings from a 2-component mixture of Mallows models
#' ## with Spearman distance.
#' set.seed(12345)
#' rank_sim <- rMSmix(sample_size = 50, n_items = 12, n_clust = 2)
#' str(rank_sim)
#' rankings <- rank_sim$samples
#' # Fit the true model.
#' set.seed(12345)
#' fit <- fitMSmix(rankings = rankings, n_clust = 2, n_start = 10)
#' # Comparing the BIC at the true parameter values and at the MLE.
#' bicMSmix(rho = rank_sim$rho, theta = rank_sim$theta, weights = rank_sim$weights,
#'        rankings = rank_sim$samples)
#' bicMSmix(rho = fit$mod$rho, theta = fit$mod$theta, weights = fit$mod$weights,
#'        rankings = rank_sim$samples)
#' aicMSmix(rho = rank_sim$rho, theta = rank_sim$theta, weights = rank_sim$weights,
#'        rankings = rank_sim$samples)
#' aicMSmix(rho = fit$mod$rho, theta = fit$mod$theta, weights = fit$mod$weights,
#'        rankings = rank_sim$samples)
#'
#'
#' ## Example 2. Simulate rankings from a basic Mallows model with Spearman distance.
#' set.seed(54321)
#' rank_sim <- rMSmix(sample_size = 50, n_items = 8, n_clust = 1)
#' str(rank_sim)
#' # Let us censor the observations to be top-5 rankings.
#' rank_sim$samples[rank_sim$samples > 5] <- NA
#' rankings <- rank_sim$samples
#' # Fit the true model with the two EM algorithms.
#' set.seed(54321)
#' fit_em <- fitMSmix(rankings = rankings, n_clust = 1, n_start = 10)
#' set.seed(54321)
#' fit_mcem <- fitMSmix(rankings = rankings, n_clust = 1, n_start = 10, mc_em = TRUE)
#' # Compare the BIC at the true parameter values and at the MLEs.
#' bicMSmix(rho = rank_sim$rho, theta = rank_sim$theta, weights = rank_sim$weights,
#'        rankings = rank_sim$samples)
#' bicMSmix(rho = fit_em$mod$rho, theta = fit_em$mod$theta, weights = fit_em$mod$weights,
#'        rankings = rank_sim$samples)
#' bicMSmix(rho = fit_mcem$mod$rho, theta = fit_mcem$mod$theta, weights = fit_mcem$mod$weights,
#'        rankings = rank_sim$samples)
#' aicMSmix(rho = rank_sim$rho, theta = rank_sim$theta, weights = rank_sim$weights,
#'        rankings = rank_sim$samples)
#' aicMSmix(rho = fit_em$mod$rho, theta = fit_em$mod$theta, weights = fit_em$mod$weights,
#'        rankings = rank_sim$samples)
#' aicMSmix(rho = fit_mcem$mod$rho, theta = fit_mcem$mod$theta, weights = fit_mcem$mod$weights,
#'        rankings = rank_sim$samples)
#'
#' @export
#'
bicMSmix <- function(rho, theta, weights, rankings) {

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }

  N <- nrow(rankings)
  n_clust <- length(weights)

  max_log_lik <- likMSmix(rho = rho, theta = theta, weights = weights, rankings = rankings)
  bic <- -2 * max_log_lik + (2 * n_clust + (n_clust - 1)) * log(N)

  return(bic)
}# scalar

# aicMSmix ----
#' AIC for mixtures of Mallows models with Spearman distance
#'
#' @description NULL
#'
#'
#' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row.
#' @param theta Numeric vector of \eqn{G} non-negative component-specific precision parameters.
#' @param weights Numeric vector of \eqn{G} positive mixture weights (normalization is not necessary).
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#'
#'
#' @references
#' Sakamoto Y, Ishiguro M, and Kitagawa G (1986). \emph{Akaike Information Criterion Statistics}. Dordrecht, The Netherlands: D. Reidel Publishing Company.
#'
#' @rdname bicMSmix
#'
#' @export
#'
aicMSmix <- function(rho, theta, weights, rankings) {
  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }

  N <- nrow(rankings)
  n_clust <- length(weights)
  max_log_lik <- likMSmix(rho = rho, theta = theta, weights = weights, rankings = rankings)
  aic <- -2 * max_log_lik + (2 * n_clust + (n_clust - 1)) * 2

  return(aic)
}# scalar

# e_step ----
#/' Utility for the internal em_db_mix
#/'
#/' Compute the E-step of the EM algorithm for the estimation of the component membership probabilities.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the current value of the component-specific consensus rankings in each row.
#/' @param theta Numeric vector with the current value of the \eqn{G} component-specific precision parameters.
#/' @param weights Numeric vector with the current value of the \eqn{G} mixture weights.
#/' @param rankings Integer matrix with full rankings in each row, whose content depends on the type of originally observed ranking data (full or partial) and the selected EM algorithm.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/'
#/' @return Numeric matrix with the current estimate of posterior component membership probabilities in each row.
#/'
e_step <- function(rho, theta, weights, rankings, cardinalities) {
  if (!is.matrix(rankings)) {
    rankings <- matrix(rankings, nrow = 1)
  }

  if (is.vector(rho)) {
    rho <- matrix(rho, nrow = 1)
  }

  temp <- log_lik_inter_spearman(rho = rho, theta = theta, rankings = rankings,
                                 cardinalities = cardinalities)

  z <- t(log(weights) + temp)
  z <- exp(z - apply(z, 1, max))
  z <- z / rowSums(z)
  z <- z + 10^(-10)
  return(z)
} # L*n_clust matrix


# Mstep_weights ----
#/' Utility for the internal em_db_mix
#/'
#/' Compute the M-step of the EM algorithm for the estimation of the mixture weights.
#/'
#/' @keywords internal
#/' @param N Sample size.
#/' @param freq_hat Numeric vector with the currently estimated frequencies of rankings belonging to the \eqn{G} groups.
#/'
#/' @return Numeric vector with the current estimate of the \eqn{G} mixture weights.
#/'
Mstep_weights <- function(N, freq_hat) {
  weights <- freq_hat / N
  return(weights)
} # vector of length n_clust


# average_mean_ranks ----
#/' Utility for the internal em_db_mix
#/'
#/' Compute the mean rank vectors needed for the estimation of the component-specific consensus rankings and precision parameters.
#/'
#/' @keywords internal
#/' @param rankings Integer matrix with full rankings in each row, whose content depends on the type of originally observed ranking data (full or partial) and the selected EM algorithm.
#/' @param temp_prod Numeric matrix of posterior component membership probabilities multiplied by the frequencies of the distinct full rankings.
#/' @param freq_hat Numeric vector with the currently estimated frequencies of rankings belonging to the \eqn{G} groups.
#/'
#/' @return Numeric \eqn{n}\eqn{\times}{x}\eqn{G} matrix with the current value of the component-specific mean rank vectors in each column.
#/'
average_mean_ranks <- function(rankings, temp_prod, freq_hat) {
  num <- t(temp_prod) %*% (rankings)
  amr <- t(num / freq_hat)
  return(amr)
} # n_items*n_clust matrix


# Mstep_rho ----
#/' Utility for the internal em_db_mix
#/'
#/' Compute the M-step of the EM algorithm for the estimation of the component-specific consensus rankings.
#/'
#/' @keywords internal
#/' @param amr Numeric \eqn{n}\eqn{\times}{x}\eqn{G} matrix with the current value of the component-specific mean rank vectors in each column.
#/'
#/' @return Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the current estimate of the component-specific consensus rankings.
#/'
Mstep_rho <- function(amr) {
  rho <- t(apply(amr, 2, rank, ties.method = "random"))

  if (is.vector(rho)) {
    rho <- matrix(rho, nrow = 1)
  }

  return(rho)
} # n_clust*n_items matrix

# Rhs ----
#/' Utility for the internal em_db_mix
#/'
#/' Compute the right-hand side of the estimation equation component-specific precision parameters.
#/'
#/' @keywords internal
#/' @param rho Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the current value of the component-specific consensus rankings in each row.
#/' @param amr Numeric \eqn{n}\eqn{\times}{x}\eqn{G} matrix with the current value of the component-specific mean rank vectors in each column.
#/'
#/' @return Numeric vector with the \eqn{G} sample average Spearman distances from the component-specific consensus rankings.
#/'
Rhs <- function(rho, amr) {
  if (is.vector(rho)) {
    rho <- matrix(rho, nrow = 1)
  }

  n_items <- ncol(rho)
  cn <- n_items * (n_items + 1) * (2 * n_items + 1) / 6
  rhs <- 2 * (cn - diag(rho %*% amr))
  return(rhs)
} # vector of length n_clust



# log_expect_spear_dist_hide ----
#/' Utility for the internal Equation_theta
#/'
#/' Compute the log-expectation of the Spearman distance for the estimation of the component-specific precision parameters.
#/'
#/' Although this internal routine parallels the utility \code{log_expect_spear_dist}, it allows to speed up the computations of the iterative M-step for theta.
#/'
#/' @keywords internal
#/' @param theta Non-negative precision parameter.
#/' @param distances Numeric vector of all the possible Spearman distance values.
#/' @param logcardest Numeric vector of log-frequencies corresponding to each value in \code{distances}.
#/'
#/' @return Either the exact or the approximate log-expected value of the Spearman distance.
#/'
log_expect_spear_dist_hide <- function(theta, distances, logcardest) {
  tmp <- max(logcardest + log(distances) - theta * distances)
  tmp2 <- max(logcardest - theta * distances)
  logedist <- log(sum(exp(logcardest + log(distances) - theta * distances - tmp))) -
    log(sum(exp(logcardest - theta * distances - tmp2))) + tmp - tmp2
  return(logedist)
}


# Equation_theta ----
#/' Utility for the internal Mstep_theta
#/'
#/' Compute the estimation equation for the component-specific precision parameters on the log scale.
#/'
#/' @keywords internal
#/' @param theta Non-negative precision parameter.
#/' @param n_items Number of items.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/' @param rhs Right-hand side of the estimation equation of the component-specific precision parameters.
#/'
#/' @return Numeric vector with the \eqn{G} differences between the (log-) expected and sample average Spearman distances from the component-specific consensus rankings.
#/'
Equation_theta <- function(theta, n_items, cardinalities, rhs) {
  logcardest <- cardinalities$logcard
  distances <- cardinalities$distances

  out <- log_expect_spear_dist_hide(
    theta = theta,
    distances = distances,
    logcardest = logcardest
  ) - log(rhs)

  return(out)
}


# Mstep_theta ----
#/' Utility for the internal em_db_mix, homo_bootstrapMSmix and hetero_bootstrapMSmix
#/'
#/' Compute the M-step of the EM algorithm for the estimation of the precision parameters.
#/'
#/' @keywords internal
#/' @param theta_max Positive upper bound for the precision parameters.
#/' @param n_items Number of items.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/' @param rhs Right-hand side of the estimation equation of the component-specific precision parameters.
#/' @param theta_tol Positive convergence tolerance for the M-step of the precision parameters.
#/'
#/' @return Numeric vector with the current estimate of the \eqn{G} component-specific precision parameters.
#/'
Mstep_theta <- function(theta_max, n_items, cardinalities, rhs, theta_tol) {
  n_clust <- length(rhs)
  f_low <- Equation_theta(theta = 0, n_items = n_items, cardinalities = cardinalities, rhs = rhs)
  f_upp <- Equation_theta(theta = theta_max, n_items = n_items, cardinalities = cardinalities,
                          rhs = rhs)

  theta_vec <- rep(NA, n_clust)

  for (g in 1:n_clust) {
    if (sign(f_low[g]) != sign(f_upp[g])) {
      theta_vec[g] <- uniroot(Equation_theta,
                              n_items = n_items,
                              cardinalities = cardinalities,
                              rhs = rhs[g], interval = c(0, theta_max),
                              f.lower = f_low[g], f.upper = f_upp[g], tol = theta_tol
      )$root
    } else {
      theta_vec[g] <- theta_max
    }
  }
  return(theta_vec)
} # vector of length n_clust


# em_db_mix ----
#/' Utility for the exported fitMSmix
#/'
#/' Perform the MLE of mixtures of Mallows model with Spearman distance on full and partial rankings via EM algorithms launched from a SINGLE starting point.
#/' Partial rankings with missing data in arbitrary positions are supported.
#/'
#/' @keywords internal
#/' @param rankings_orig Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#/' @param rankings Integer matrix with full rankings in each row, whose content depends on the type of originally observed ranking data (full or partial) and the selected EM algorithm.
#/' @param item_names Character vector with the names to be used for the items.
#/' @param freq_compl Integer vector with the (observed or estimated) frequencies of each distinct full ranking.
#/' @param partial Logical: whether the \code{rankings_orig} matrix contains any partial sequence.
#/' @param rankings_part If \code{partial = TRUE}, the (aggregated or non-aggregated) observed matrix of partial rankings, otherwise \code{NULL}.
#/' @param freq_part If \code{partial = TRUE}, an integer vector with the frequencies of partial sequences in \code{rankings_part}, otherwise \code{NULL}.
#/' @param N_partial_rows If \code{partial = TRUE} and \code{mc_em = TRUE}, the number of partial sequences in the \code{rankings_orig} matrix, otherwise \code{NULL}.
#/' @param partial_rows If \code{partial = TRUE} and \code{mc_em = TRUE}, a logical vector indicating the rows of the \code{rankings_orig} matrix corresponding to a partial sequence, otherwise \code{NULL}.
#/' @param missing_entries If \code{partial = TRUE} and \code{mc_em = TRUE}, a list of length \code{N_partial_rows} with labels of the unranked items in the partial sequences of the \code{rankings_orig} matrix, otherwise \code{NULL}.
#/' @param N Sample size, corresponding to the number of rows of \code{rankings_orig}.
#/' @param n_items Number of items.
#/' @param n_clust Number of mixture components.
#/' @param n_iter Maximum number of EM iterations. Defaults to 200.
#/' @param theta_max Positive upper bound for the precision parameters. Defaults to 3.
#/' @param init List with the starting values of the parameters to initialize the EM algorithm. This must contain three named objects, namely: 1) \code{rho}: integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row; 2) \code{theta}: numeric vector of \eqn{G} non-negative component-specific precision parameters; 3) \code{weights}: numeric vector of \eqn{G} positive mixture weights.
#/' @param cardinalities A list of two named objects: 1) \code{distances}: vector of all the possible Spearman distance values; 2) \code{logcard}: vector of log-frequencies corresponding to each value in \code{distances}.
#/' @param eps Positive tolerance value for the convergence of the EM algorithm. Defaults to \eqn{10^{-6}}.
#/' @param plot_log_lik Logical: whether the iterative log-likelihood values (based on full or augmented rankings) must be plotted.
#/' @param plot_log_lik_part Logical: whether the iterative observed-data log-likelihood values (based on partial rankings) must be plotted.
#/' @param aug_list If \code{partial = TRUE}, a list with elements corresponding to the matrices of full rankings compatible with the partial sequences in \code{rankings_part}, otherwise \code{NULL}.
#/' @param aug_mat If \code{partial = TRUE}, an integer matrix of full rankings with rows given by the stacked matrices in \code{aug_list}, otherwise \code{NULL}.
#/' @param aug_mat_vec If \code{partial = TRUE}, a character vector with elements corresponding to the rows of \code{aug_mat} converted into strings, otherwise \code{NULL}.
#/' @param mc_em Logical: whether the Monte Carlo EM algorithm must be used for MLE on partial rankings completion. Ignored when \code{rankings} does not contain any partial sequence.
#/' @param theta_tune Positive tuning constant affecting the precision parameters in the Monte Carlo step. Ignored when \code{rankings} does not contain any partial sequence or \code{mc_em = FALSE}.
#/' @param theta_tol Positive convergence tolerance for the M-step of the precision parameters. Defaults to \eqn{10^{-5}}.
#/'
#/' @return
#/' The \code{mod} sublist contains the following named objects:
#/' \describe{
#/'  \item{\code{rho}}{Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the estimates of the component-specific consensus rankings in each row.}
#/'  \item{\code{theta}}{Numeric vector with the estimates of the \eqn{G} component-specific precision parameters.}
#/'  \item{\code{weights}}{Numeric vector with the estimates of the \eqn{G} mixture weights.}
#/'  \item{\code{z_hat}}{Numeric \eqn{N}\eqn{\times}{x}\eqn{G} matrix of the estimated posterior component membership probabilities for the rows of the \code{rankings} matrix. Returned when \code{n_clust > 1}, otherwise \code{NULL}.}
#/'  \item{\code{map_classification}}{\code{NULL}.}
#/'  \item{\code{freq_compl}}{Integer vector with the (observed or estimated) frequencies of each distinct full ranking. Returned when \code{n_clust > 1}, otherwise \code{NULL}.}
#/'  \item{\code{log_lik}}{Numeric vector of the log-likelihood values (based on full or augmented rankings) at each iteration.}
#/'  \item{\code{best_log_lik}}{Maximized log-likelihood value (based on full or augmented rankings) of the fitted model.}
#/'  \item{\code{bic}}{BIC value of the fitted model based on \code{best_log_lik}.}
#/'  \item{\code{log_lik_part}}{Numeric vector of the observed-data log-likelihood values (based on partial rankings) at each iteration. Returned when \code{rankings_orig} contains some partial sequences that can be completed with \code{data_augmentation} and \code{plot_log_lik_part = TRUE}, otherwise \code{NULL}.}
#/'  \item{\code{best_log_lik_part}}{\code{NULL}.}
#/'  \item{\code{bic_part}}{\code{NULL}.}
#/'  \item{\code{conv}}{Binary convergence indicator: 1 = convergence has been achieved, 0 = otherwise.}
#/'  \item{\code{augmented_rankings}}{Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with rankings completed through the Monte Carlo step in each row. Returned when \code{partial = TRUE} and \code{mc_em = TRUE}, otherwise \code{NULL}.}
#/'  }
#/'
em_db_mix <- function(rankings_orig,
                      rankings,
                      item_names,
                      freq_compl,
                      partial,
                      rankings_part,
                      freq_part,
                      N_partial_rows,
                      partial_rows,
                      missing_entries,
                      N,
                      n_items,
                      n_clust,
                      n_iter = 200,
                      theta_max = 3,
                      init,
                      cardinalities,
                      eps = 10^(-6),
                      plot_log_lik = FALSE,
                      plot_log_lik_part = FALSE,
                      aug_list,
                      aug_mat,
                      aug_mat_vec,
                      mc_em,
                      theta_tune,
                      theta_tol = 1e-05) {
  rho <- init$rho
  weights <- init$weights
  theta <- init$theta

  log_lik <- rep(NA, n_iter)
  if (plot_log_lik_part & partial & !inherits(aug_list, "try-error")) {
    log_lik_partial <- rep(NA, n_iter)
  }

  conv <- 0
  l <- 1

  if (n_clust == 1) {
    z_hat <- matrix(1, nrow = length(freq_compl), ncol = n_clust)
  }

  while (l <= n_iter) {
    if (l %% 50 == 0) {
      print(paste("EM iteration", l))
    }

    if (n_clust > 1) {
      z_hat <- e_step(rho = rho, theta = theta, weights = weights, rankings = rankings,
                      cardinalities = cardinalities)
    }
    temp_prod <- z_hat * freq_compl
    freq_hat <- colSums(temp_prod)

    if (n_clust == 1) {
      weights <- 1
    } else {
      weights <- Mstep_weights(N = N, freq_hat = freq_hat)
      if (max(weights) > (1 - 10^(-7)) | min(weights) < 10^(-7)) {
        weights <- weights + 10^(-8)
        weights <- prop.table(weights)
      }
    }

    amr <- average_mean_ranks(rankings = rankings, temp_prod = temp_prod, freq_hat = freq_hat)

    rho <- Mstep_rho(amr = amr)
    rhs <- Rhs(rho = rho, amr = amr)
    theta <- Mstep_theta(theta_max = theta_max, n_items = n_items, rhs = rhs, cardinalities = cardinalities, theta_tol = theta_tol)

    if (partial) {
      if (mc_em) {
        if (n_clust == 1) {
          rho_star <- suppressMessages(rMSmix(
            sample_size = N_partial_rows, n_items = n_items,
            n_clust = n_clust, rho = rho,
            theta = theta_tune*theta
          )$samples)
        } else {
          rho_star <- matrix(NA, nrow = N_partial_rows, ncol = n_items)
          z_hat_part <- z_hat[partial_rows, , drop = FALSE]
          c_tmp <- apply(z_hat_part, 1, sample, size = 1, x = 1:n_clust, replace = TRUE)
          for (cc in 1:n_clust) {
            quanti <- sum(c_tmp == cc)
            quali <- which(c_tmp == cc)
            rho_star[quali, ] <- suppressMessages(rMSmix(
              sample_size = quanti, n_items = n_items,
              n_clust = 1, rho = rho[cc, ],
              theta = theta_tune*theta[cc])$samples)
          }
        }

        rankings[partial_rows, ] <- t(sapply(1:N_partial_rows,
                                             function(x)ranking_completion_hide(
                                               part_ranking = rankings_part[partial_rows[x], ],
                                               rho = rho_star[x, ],
                                               items_unranked = missing_entries[[x]],
                                               n_items = n_items)))

        if (plot_log_lik_part & !inherits(aug_list, "try-error")) {
          log_lik_partial[l] <- log_lik_db_mix_partial(
            rho = rho, theta = theta, weights = weights,
            aug_list = aug_list, freq_part = freq_part,
            cardinalities = cardinalities
          )
        }
      } else {
        freq_compl <- estn(
          theta = theta, rho = rho, weights = weights,
          aug_list = aug_list, aug_mat = aug_mat,
          aug_mat_vec = aug_mat_vec,
          freq_part = freq_part,
          cardinalities = cardinalities
        )

        if (plot_log_lik_part){
          log_lik_partial[l] <- log_lik_db_mix_partial(
            rho = rho, theta = theta, weights = weights, aug_list = aug_list, freq_part = freq_part,
            cardinalities = cardinalities
          )
        }
      }
    }

    log_lik[l] <- log_lik_db_mix(
      rho = rho, theta = theta, weights = weights, rankings = rankings, freq_compl = freq_compl,
      cardinalities = cardinalities
    )

    if (l >= 2) {
      rat=(log_lik[l] - log_lik[l - 1])/abs(log_lik[l - 1])
      if (is.nan(rat) | rat<eps) {
        conv <- 1
        l <- n_iter + 1
      }
    }
    l <- l + 1
  } # end while

  log_lik <- log_lik[!(is.na(log_lik))]
  best_log_lik <- log_lik[length(log_lik)]
  bic <- -2 * best_log_lik + (2 * n_clust + (n_clust - 1)) * log(N)

  if (plot_log_lik) {
    plot(log_lik,
         ylab = "Log-likelihood for complete data", xlab = "Iteration",
         main = paste0(n_clust, "-component mixture of Mallows models\nwith the Spearman distance"),
         type = "l"
    )


  }

  if (plot_log_lik_part & partial & !inherits(aug_list, "try-error")) {
    log_lik_partial <- log_lik_partial[!(is.na(log_lik_partial))]
    plot(log_lik_partial,
         ylab = "Log-likelihood for partial data", xlab = "Iteration",
         main = paste0(n_clust, "-component mixture of Mallows models\nwith the Spearman distance"),
         type = "l"
    )
  }

  colnames(rho) <- item_names

  return(list(
    rho = rho,
    theta = theta,
    weights = (if (n_clust > 1) weights else 1),
    z_hat = (if (n_clust > 1) z_hat else NULL),
    map_classification = NULL,
    freq_compl = (if (n_clust > 1) freq_compl else NULL),
    log_lik = log_lik,
    best_log_lik = best_log_lik,
    bic = bic,
    log_lik_part = (if (plot_log_lik_part & partial & !inherits(aug_list, "try-error")) log_lik_partial else NULL),
    best_log_lik_part = NULL,
    bic_part = NULL,
    conv = conv,
    augmented_rankings = (if (partial & mc_em) rankings else NULL)
  ))
}

# fitMSmix ----
#' MLE of mixtures of Mallows models with Spearman distance via EM algorithms
#'
#' @description
#' Perform the MLE of mixtures of Mallows model with Spearman distance on full and partial rankings via EM algorithms.
#' Partial rankings with missing data in arbitrary positions are supported.
#'
#' @details
#' The EM algorithms are launched from \code{n_start} initializations and the best solution in terms of maximized
#' log-likelihood value (based on full or augmented rankings) is returned.
#'
#' When \code{mc_em = FALSE}, the scheme introduced by Crispino et al. (2023) is performed, where partial
#' rankings are augmented with all compatible full rankings. This type of data augmentation is
#' supported up to 10 missing positions in the partial rankings.
#'
#' When \code{mc_em = TRUE}, the - computationally more efficient - Monte Carlo EM algorithm
#' introduced by Crispino et al. (2025+) is implemented. In the case of a large number
#' of censored positions and sample sizes, the \code{mc_em = TRUE} must be preferred.
#'
#' Regardless of the fitting method adopted for inference on partial rankings, note that
#' setting the argument \code{comp_log_lik_part = TRUE} for the computation of the
#' observed-data log-likelihood values (based on partial rankings)
#' can slow down the procedure in the case of a large number of censored positions and sample sizes.
#'
#' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#' @param n_clust Number of mixture components. Defaults to 1.
#' @param n_start Number of starting points. Defaults to 1.
#' @param n_iter Maximum number of EM iterations. Defaults to 200.
#' @param mc_em Logical: whether the Monte Carlo EM algorithm must be used for MLE on partial rankings completion, see Details. Ignored when \code{rankings} does not contain any partial sequence. Defaults to \code{FALSE}.
#' @param eps Positive tolerance value for the convergence of the EM algorithm. Defaults to \eqn{10^{-6}}.
#' @param init List of \code{n_start} lists with the starting values of the parameters to initialize the EM algorithm. Each list must contain three named objects, namely: 1) \code{rho}: integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the component-specific consensus rankings in each row; 2) \code{theta}: numeric vector of \eqn{G} non-negative component-specific precision parameters; 3) \code{weights}: numeric vector of \eqn{G} positive mixture weights. Defaults to \code{NULL}, meaning that the starting points are automatically generated from the uniform distribution.
#' @param plot_log_lik Logical: whether the iterative log-likelihood values (based on full or augmented rankings) must be plotted. Defaults to \code{FALSE}.
#' @param comp_log_lik_part Logical: whether the maximized observed-data log-likelihood value (based on partial rankings) must be returned. Ignored when \code{rankings} does not contain any partial sequence or \code{\link{data_augmentation}} cannot be applied. See Details. Defaults to \code{FALSE}.
#' @param plot_log_lik_part Logical: whether the iterative observed-data log-likelihood values (based on partial rankings) must be plotted. Ignored when \code{rankings} does not contain any partial sequence. In the presence of partial rankings, this argument is ignored when \code{comp_log_lik_part = FALSE} or \code{\link{data_augmentation}} cannot be applied. Defaults to \code{FALSE}.
#' @param parallel Logical: whether parallelization over multiple initializations must be used. Defaults to \code{FALSE}.
#' @param theta_max Positive upper bound for the precision parameters. Defaults to 3.
#' @param theta_tol Positive convergence tolerance for the M-step of the precision parameters. Defaults to \eqn{10^{-5}}.
#' @param theta_tune Positive tuning constant affecting the precision parameters in the Monte Carlo step. Ignored when \code{rankings} does not contain any partial sequence or \code{mc_em = FALSE}. Defaults to 1.
#' @param subset Optional logical or integer vector specifying the subset of observations, i.e. rows of the \code{rankings}, to be kept. Missing values are taken as \code{FALSE}. Defaults to \code{NULL} meaning that all the rows are considered.
#' @param item_names Character vector with the names to be used for the items. Defaults to \code{NULL}, meaning that \code{colnames(rankings)} is used and, if not available, \code{item_names} is set equal to \code{"Item1","Item2",...}.
#'
#' @return
#' An object of class \code{"emMSmix"}, namely a list with the following named components:
#'    \describe{
#'  \item{\code{mod}}{List of named objects describing the best fitted model in terms of maximized log-likelihood over the \code{n_start} initializations. See Details.}
#'  \item{\code{max_log_lik}}{Maximized log-likelihood values for each initialization.}
#'  \item{\code{partial_data}}{Logical: whether the dataset includes some partially-ranked sequences.}
#'  \item{\code{convergence}}{Binary convergence indicators of the EM algorithm for each initialization: 1 = convergence has been achieved, 0 = otherwise.}
#'  \item{\code{record}}{Best log-likelihood values sequentially achieved over the \code{n_start} initializations.}
#'  \item{\code{em_settings}}{List of settings used to fit the model.}
#'  \item{\code{call}}{The matched call.}
#' }
#'
#' The \code{mod} sublist contains the following named objects:
#' \describe{
#'  \item{\code{rho}}{Integer \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the MLEs of the component-specific consensus rankings in each row.}
#'  \item{\code{theta}}{Numeric vector with the MLEs of the \eqn{G} component-specific precision parameters.}
#'  \item{\code{weights}}{Numeric vector with the MLEs of the \eqn{G} mixture weights.}
#'  \item{\code{z_hat}}{Numeric \eqn{N}\eqn{\times}{x}\eqn{G} matrix of the estimated posterior component membership probabilities. Returned when \code{n_clust > 1}, otherwise \code{NULL}.}
#'  \item{\code{map_classification}}{Integer vector of \eqn{N} mixture component memberships based on the MAP allocation from the \code{z_hat} matrix. Returned when \code{n_clust > 1}, otherwise \code{NULL}.}
#'  \item{\code{log_lik}}{Numeric vector of the log-likelihood values (based on full or augmented rankings) at each iteration.}
#'  \item{\code{best_log_lik}}{Maximized log-likelihood value (based on full or augmented rankings) of the fitted model.}
#'  \item{\code{bic}}{BIC value of the fitted model based on \code{best_log_lik}.}
#'  \item{\code{log_lik_part}}{Numeric vector of the observed-data log-likelihood values (based on partial rankings) at each iteration. Returned when \code{rankings} contains some partial sequences that can be completed with \code{data_augmentation} and \code{plot_log_lik_part = TRUE}, otherwise \code{NULL}. See Details.}
#'  \item{\code{best_log_lik_part}}{Maximized observed-data log-likelihood value (based on partial rankings) of the fitted model. Returned when \code{rankings} contains some partial sequences that can be completed with \code{data_augmentation}, otherwise \code{NULL}. See Details.}
#'  \item{\code{bic_part}}{BIC value of the fitted model based on \code{best_log_lik_part}. Returned when \code{rankings} contains some partial sequences that can be completed with \code{\link{data_augmentation}}, otherwise \code{NULL}. See Details.}
#'  \item{\code{conv}}{Binary convergence indicator of the best fitted model: 1 = convergence has been achieved, 0 = otherwise.}
#'  \item{\code{augmented_rankings}}{Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix with rankings completed through the Monte Carlo step in each row. Returned when \code{rankings} contains some partial sequences and \code{mc_em = TRUE}, otherwise \code{NULL}.}
#'  }
#'
#' @references
#' Crispino M, Mollica C and Modugno L (2025+). MSmix: An R Package for clustering partial rankings via mixtures of Mallows Models with Spearman distance. \emph{(submitted)}
#'
#' Crispino M, Mollica C, Astuti V and Tardella L (2023). Efficient and accurate inference for mixtures of Mallows models with Spearman distance. \emph{Statistics and Computing}, \bold{33}(98), DOI: 10.1007/s11222-023-10266-8.
#'
#' Sørensen Ø, Crispino M, Liu Q and Vitelli V (2020). BayesMallows: An R Package for the Bayesian Mallows Model. \emph{The R Journal}, \bold{12}(1), pages 324--342, DOI: 10.32614/RJ-2020-026.
#'
#' Beckett LA (1993). Maximum likelihood estimation in Mallows’s model using partially ranked data. In \emph{Probability models and statistical analyses for ranking data}, pages 92--107. Springer New York.
#'
#'
#' @seealso \code{\link{summary.emMSmix}}, \code{\link{plot.emMSmix}}
#'
#' @examples
#' ## Example 1. Fit the 3-component mixture of Mallows models with Spearman distance
#' ## to the Antifragility dataset.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' set.seed(123)
#' mms_fit <- fitMSmix(rankings = r_antifrag, n_clust = 3, n_start = 10)
#' mms_fit$mod$rho; mms_fit$mod$theta; mms_fit$mod$weights
#'
#' ## Example 2. Fit the Mallows model with Spearman distance
#' ## to simulated partial rankings through data augmentation.
#' rank_data <- rbind(c(NA, 4, NA, 1, NA), c(NA, NA, NA, NA, 1), c(2, NA, 1, NA, 3),
#'                    c(4, 2, 3, 5, 1), c(NA, 4, 1, 3, 2))
#' mms_fit <- fitMSmix(rankings = rank_data, n_start = 10)
#' mms_fit$mod$rho; mms_fit$mod$theta
#'
#' ## Example 3. Fit the Mallows model with Spearman distance
#' ## to the Reading genres dataset through Monte Carlo EM.
#' top5_read <- ranks_read_genres[, 1:11]
#' mms_fit <- fitMSmix(rankings = top5_read, n_start = 10, mc_em = TRUE)
#' mms_fit$mod$rho; mms_fit$mod$theta
#'
#' @export
#'
fitMSmix <- function(rankings,
                     n_clust = 1,
                     n_start = 1,
                     n_iter = 200,
                     mc_em = FALSE,
                     eps = 10^(-6),
                     init = list(list(rho = NULL, theta = NULL, weights = NULL))[rep(1, n_start)],
                     plot_log_lik = FALSE,
                     comp_log_lik_part = FALSE,
                     plot_log_lik_part = FALSE,
                     parallel = FALSE,
                     theta_max = 3,
                     theta_tol = 1e-05,
                     theta_tune = 1,
                     subset = NULL,
                     item_names = NULL) {


  cl <- match.call()

  if (!is.matrix(rankings)) {
    if (is.vector(rankings)) {
      rankings <- matrix(rankings, nrow = 1)
    } else {
      rankings <- as.matrix(rankings)
    }
  }


  if (!is.null(subset)) {
    if(is.logical(subset)){
      rankings <- rankings[(subset & !is.na(subset)),, drop = FALSE]
    }else{
      rankings <- rankings[na.omit(subset),, drop = FALSE]
    }
  }

  n_items <- ncol(rankings)

  if (is.null(item_names)) {
    item_names <- colnames(rankings)
    if (is.null(item_names)) {
      colnames(rankings) <- item_names <- paste0("Item", 1:n_items)
    }
  } else {
    colnames(rankings) <- item_names
  }

  aug_list <- aug_mat <- freq_part <- rankings_part <- N_partial_rows <- partial_rows <- missing_entries <- NULL

  aug_mat_vec = NULL

  if (any(is.na(rankings))) {
    rankings <- suppressWarnings(fill_single_entries_new(data = rankings))
  }

  N <- nrow(rankings)
  rankings_orig <- rankings
  check_na <- is.na(rankings)

  if (any(rowSums(!check_na) == 0)) {
    stop("Some rankings have all NA entries and should be removed before performing the analysis.\n")
  }

  partial <- any(check_na)


  if (partial) {

    if(!comp_log_lik_part){
      plot_log_lik_part = FALSE
    }

    if (mc_em) {
      message("The dataset includes partial rankings. Estimation method ------> MONTE CARLO EM.\n")
      rankings_part <- rankings
      freq_part <- freq_compl <- rep(1, N)


      if(comp_log_lik_part){
        aug_list <- try(data_augmentation(rankings_part, fill_single_na = FALSE), silent = TRUE)
      }

      partial_rows <- which(apply(check_na, 1, any))
      N_partial_rows <- length(partial_rows)
      missing_entries <- apply(check_na[partial_rows,, drop = FALSE], 1, which)
      if (is.matrix(missing_entries)) {
        missing_entries <- as.data.frame(missing_entries)
      }
    } else {
      message("The dataset includes partial rankings. Estimation method ------> EM ALGORITHM ON AUGMENTED RANKINGS proposed by Crispino et al. (2023).\n")

      quanti_na <- rowSums(check_na)

      if (any(quanti_na > 10)) {
        stop("Data augmentation cannot be performed because some partial rankings have more than 10 missing positions.\n")
      }else{
        if ((n_items > 11)&(any(quanti_na > 6))) {
          message("Generating all possible full rankings compatible with the partial observations.\n Please, be aware that this may be slow and allocate a lot of memory.\n Alternatively, stop the fitting routine and rerun with mc_em = TRUE.\n")
        }
      }

      uniranks <- frequence(rankings)
      freq_part <- uniranks[, n_items + 1]
      rankings_part <- uniranks[, 1:n_items, drop = FALSE]
      aug_list <- try(quiet(data_augmentation(rankings_part, fill_single_na = FALSE)), silent = TRUE)
      aug_mat <- do.call(rbind, aug_list)
      aug_mat_vec = apply(aug_mat,1,paste0,sep="-",collapse="")
      rankings <- unique(aug_mat)
    }
  } else {
    message("The dataset includes only full rankings. Estimation method ------> EM ALGORITHM.\n")

    uniranks <- frequence(rankings)
    freq_compl <- uniranks[, n_items + 1]
    rankings <- uniranks[, 1:n_items, drop = FALSE]
  }


  for (i in 1:n_start) {
    if (is.null(init[[i]]$rho)) {
      init[[i]]$rho <- t(apply(matrix(1:n_items, nrow = n_items, ncol = n_clust), 2, sample))
    } else {
      rho <- init[[i]]$rho
      if (is.vector(rho)) {
        init[[i]]$rho <- matrix(rho, nrow = 1)
      }
    }

    if (is.null(init[[i]]$weights)) {
      init[[i]]$weights <- as.vector(rdirichlet(1, rep(n_clust * 2, n_clust)))
    } else {
      weights <- init[[i]]$weights
      if (any(weights <= 0)) {
        stop("Mixture weights must be positive")
      } else {
        if (sum(weights) != 1) {
          weights <- prop.table(weights)
          warning("Mixture weights have been normalized to sum up to one")
        }
      }
    }

    if (is.null(init[[i]]$theta)) {
      init[[i]]$theta <- runif(n = n_clust, min = 0, max = 1)
    } else {
      theta <- init[[i]]$theta
      if (any(theta < 0)) {
        stop("Precision parameters must be non-negative")
      }
    }
  }

  cardinalities <- suppressMessages(spear_dist_distr(n_items))

  if (!parallel) {
    mod <- vector(mode = "list", length = n_start)
    max_log_lik <- rep(NA, n_start)
    convergence <- rep(NA, n_start)
    record <- rep(NA, n_start)

    l <- 0

    for (i in 1:n_start) {
      l <- l + 1
      print(paste("INITIALIZATION", l))

      if (partial) {
        if (mc_em) {
          rho_star <- suppressMessages(rMSmix(
            sample_size = N_partial_rows, n_items = n_items,
            n_clust = n_clust, rho = init[[i]]$rho,
            theta = init[[i]]$theta, weights = init[[i]]$weights
          )$samples)


          rankings[partial_rows, ] <- t(sapply(1:N_partial_rows,
                                               function(x)ranking_completion_hide(
                                                 part_ranking = rankings_part[partial_rows[x], ],
                                                 rho = rho_star[x, ],
                                                 items_unranked = missing_entries[[x]],
                                                 n_items = n_items)))

        } else {
          freq_compl <- estn(
            theta = init[[i]]$theta, rho = init[[i]]$rho, weights = init[[i]]$weights,
            aug_list = aug_list, aug_mat = aug_mat,
            aug_mat_vec = aug_mat_vec,
            freq_part = freq_part,
            cardinalities = cardinalities
          )
        }
      }

      mod[[l]] <- em_db_mix(
        rankings_orig = rankings_orig,
        rankings = rankings,
        item_names = item_names,
        freq_compl = freq_compl,
        partial = partial,
        rankings_part = rankings_part,
        freq_part = freq_part,
        N_partial_rows = N_partial_rows,
        partial_rows = partial_rows,
        missing_entries = missing_entries,
        N = N,
        n_items = n_items,
        n_clust = n_clust,
        n_iter = n_iter,
        theta_max = theta_max,
        init = init[[i]],
        cardinalities = cardinalities,
        eps = eps,
        plot_log_lik = plot_log_lik,
        plot_log_lik_part = plot_log_lik_part,
        aug_list = aug_list,
        aug_mat = aug_mat,
        aug_mat_vec = aug_mat_vec,
        mc_em = mc_em,
        theta_tune = theta_tune,
        theta_tol = theta_tol)

      max_log_lik[l] <- max(mod[[l]]$log_lik)
      convergence[l] <- mod[[l]]$conv
      record[l] <- cummax(max_log_lik)[l]
      print(paste("Starting value #", l, " => best log-likelihood so far =", record[l]))
    }
  } else {


    if(!("doParallel"%in%names(sessionInfo()$otherPkgs))){
      stop("For parallelization, load package 'doParallel' and set the number of cores with registerDoParallel().\n")
    }else{
      if(!getDoParRegistered() | getDoParWorkers() == 1){
        stop("For parallelization, the 'cores' argument in registerDoParallel() must be set greater than 1.\n")
      }
    }

    mod <- foreach(i = 1:n_start) %dopar% {

      if (partial) {
        if (mc_em) {
          rho_star <- suppressMessages(rMSmix(
            sample_size = N_partial_rows, n_items = n_items,
            n_clust = n_clust, rho = init[[i]]$rho,
            theta = init[[i]]$theta, weights = init[[i]]$weights
          )$samples)

          rankings[partial_rows, ] <- t(sapply(1:N_partial_rows,
                                               function(x)ranking_completion_hide(
                                                 part_ranking = rankings_part[partial_rows[x], ],
                                                 rho = rho_star[x, ],
                                                 items_unranked = missing_entries[[x]],
                                                 n_items = n_items)))

        } else {
          freq_compl <- estn(
            theta = init[[i]]$theta, rho = init[[i]]$rho, weights = init[[i]]$weights,
            aug_list = aug_list, aug_mat = aug_mat,
            aug_mat_vec = aug_mat_vec,
            freq_part = freq_part,
            cardinalities = cardinalities
          )
        }
      }

      tempmod <- suppressMessages(em_db_mix(
        rankings_orig = rankings_orig,
        rankings = rankings,
        item_names = item_names,
        freq_compl = freq_compl,
        partial = partial,
        rankings_part = rankings_part,
        freq_part = freq_part,
        N_partial_rows = N_partial_rows,
        partial_rows = partial_rows,
        missing_entries = missing_entries,
        N = N,
        n_items = n_items,
        n_clust = n_clust,
        n_iter = n_iter,
        theta_max = theta_max,
        init = init[[i]],
        cardinalities = cardinalities,
        eps = eps,
        plot_log_lik = plot_log_lik,
        plot_log_lik_part = plot_log_lik_part,
        aug_list = aug_list,
        aug_mat = aug_mat,
        aug_mat_vec = aug_mat_vec,
        mc_em = mc_em,
        theta_tune = theta_tune,
        theta_tol = theta_tol))

    }

    max_log_lik <- sapply(mod, function(x) max(x$log_lik))
    record <- cummax(max_log_lik)
    convergence <- sapply(mod, "[[", "conv")

  }

  mod <- mod[[which.max(max_log_lik)]]

  if (n_clust > 1) {
    if (!partial) {
      mod$z_hat <- assign_cluster_full(rankings_orig = rankings_orig, z_hat = mod$z_hat)
    } else {
      if (!mc_em) {
        mod$z_hat <- assign_cluster_partial(rankings_part_orig = rankings_orig,
                                            aug_list=aug_list,
                                            aug_mat=aug_mat,
                                            z_hat = mod$z_hat, freq_compl = mod$freq_compl)
      }
    }
  }

  mod$freq_compl=NULL


  if (n_clust > 1){
    mod$map_classification = apply(mod$z_hat, 1, which.max)
  }

  if (partial & comp_log_lik_part & !inherits(aug_list, "try-error")) {
    mod$best_log_lik_part <- log_lik_db_mix_partial(
      rho = mod$rho, theta = mod$theta, weights = mod$weights,
      aug_list = aug_list, freq_part = freq_part,
      cardinalities = cardinalities
    )
    mod$bic_part <- -2 * mod$best_log_lik_part + (2 * n_clust + (n_clust - 1)) * log(N)
  }

  if (!is.null(mod$augmented_rankings)){
    dimnames(mod$augmented_rankings) = list(NULL, item_names)
  }


  em_settings <- list(rankings = rankings_orig,
                      n_clust = n_clust,
                      n_iter = n_iter,
                      mc_em = mc_em,
                      eps = eps,
                      theta_tol = theta_tol,
                      theta_max = theta_max,
                      theta_tune = theta_tune)

  out <- list(mod = mod, max_log_lik = max_log_lik, partial_data = partial,
              convergence = convergence, record = record,
              em_settings = em_settings, call = cl)

  class(out) <- "emMSmix"

  out
  message("Use functions summary() and plot() to summarize and visualize the object of class 'emMSmix'.\n")


  return(out)
}

# print.emMSmix ----
#' Print of the EM algorithm for the mixture of Mallows models with Spearman distance
#'
#' @description \code{print} method for class \code{"emMSmix"}.
#'
#'
#' @param x An object of class \code{"emMSmix"} returned by \code{\link{fitMSmix}}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @rdname fitMSmix
#'
#' @export print.emMSmix
#' @export
#'

print.emMSmix <- function(x, ...) {
  emMSmix_out <- x

  if (!is(emMSmix_out, "emMSmix")) {
    stop("The function requires an object of S3 class 'emMSmix' as its first argument.\n")
  }

  cat("\nCall:\n", paste(deparse(emMSmix_out$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = ""
  )
  n_items <- ncol(emMSmix_out$mod$rho)
  n_clust <- length(emMSmix_out$mod$weights)
  cat("N. of items:", n_items, "\n")
  cat("N. of clusters:", n_clust, "\n")
  cat("\n")
  cat("N. of starting points:", length(emMSmix_out$max_log_lik), "\n")
  cat("Convergence achieved:", (emMSmix_out$mod$conv == 1), "\n")
  cat("N. of iterations:", length(emMSmix_out$mod$log_lik), "\n")
  cat("\n")
  cat("BIC:", round(emMSmix_out$mod$bic,digits=2), "(based on full or augmented rankings)\n")
  if(emMSmix_out$partial_data){
    cat("Presence of partially-ranked sequences in the dataset:",emMSmix_out$partial_data,"\n")
  }
  if (!is.null(emMSmix_out$mod$bic_part)) {

    cat("BIC_part:", round(emMSmix_out$mod$bic_part,digits=2), "(based on partial rankings)\n")
  }
  invisible(x)

}


# summary.emMSmix ----
#' Summary of the fitted mixture of Mallows models with Spearman distance
#'
#' @description \code{summary} method for class \code{"emMSmix"}.
#'
#' @param object An object of class \code{"emMSmix"} returned by \code{\link{fitMSmix}}.
#' @param digits Integer: decimal places for rounding the numerical summaries. Defaults to 3.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @return A list with the following named components:
#'
#'  \item{\code{modal_rankings}}{Integer matrix with the MLEs of the \eqn{G} component-specific consensus rankings in each row.}
#'  \item{\code{modal_orderings}}{Character matrix with the MLEs of the \eqn{G} component-specific consensus orderings in each row.}
#'  \item{\code{theta}}{Numeric vector of the MLEs of the \eqn{G} precisions.}
#'  \item{\code{weights}}{Numeric vector of the MLEs of the \eqn{G} mixture weights.}
#'  \item{\code{MAP_distr}}{Percentage distribution of the component memberships based on the MAP allocation. Returned when \code{n_clust > 1}, otherwise \code{NULL}}
#'  \item{\code{conv_perc}}{Percentage of convergence of the EM algorithm over the multiple starting points.}
#'  \item{\code{BIC}}{BIC value (based on full or augmented rankings).}
#'  \item{\code{BIC_part}}{BIC value (based on partial rankings).}
#'  \item{\code{call}}{The matched call.}
#'
#' @seealso \code{\link{fitMSmix}}, \code{\link{plot.emMSmix}}
#'
#' @examples
#' ## Example 1. Fit and summary of a 3-component mixture of Mallows models with Spearman distance
#' ## for the Antifragility dataset.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' set.seed(123)
#' mms_fit <- fitMSmix(rankings = r_antifrag, n_clust = 3, n_start = 10)
#' summary(mms_fit)
#'
#'
#' @export summary.emMSmix
#' @export
summary.emMSmix <- function(object, digits = 3, ...) {
  emMSmix_out <- object

  if (!is(emMSmix_out, "emMSmix")) {
    stop("The function requires an object of S3 class 'emMSmix' as its first argument.\n")
  }

  cl <- emMSmix_out$call
  n_clust <- length(emMSmix_out$mod$weights)
  n_items <- ncol(emMSmix_out$mod$rho)
  item_names <- colnames(emMSmix_out$mod$rho)
  out <- list(
    modal_rankings = emMSmix_out$mod$rho,
    modal_orderings = t(matrix(item_names[apply(emMSmix_out$mod$rho, 1, order)], nrow = n_items, ncol = n_clust)),
    theta = emMSmix_out$mod$theta,
    weights = emMSmix_out$mod$weights,
    MAP_distr = (if (n_clust > 1) round(100*prop.table(table(factor(emMSmix_out$mod$map_classification, levels = 1:n_clust))), digits = digits) else NULL),
    conv_perc = 100 * mean(emMSmix_out$convergence),
    BIC = emMSmix_out$mod$bic,
    BIC_part = (if (!is.null(emMSmix_out$mod$bic_part)) round(emMSmix_out$mod$bic_part,digits=digits) else emMSmix_out$mod$bic_part),
    call = cl
  )

  out[c(3:4,6:7)] <- lapply(out[c(3:4,6:7)], round, digits = digits)

  names(out$weights) <- names(out$theta) <- paste0("Group", 1:n_clust)
  dimnames(out$modal_rankings) <- list(paste0("Group", 1:n_clust), item_names)
  dimnames(out$modal_orderings) <- list(paste0("Group", 1:n_clust), paste0("Rank", 1:n_items))

  class(out) <- "summary.emMSmix"
  out
}



# print.summary.emMSmix ----
#' Print of the summary of fitted mixture of Mallows models with Spearman distance
#'
#' \code{print} method for class \code{"summary.emMSmix"}.
#'
#'
#' @param x An object of class \code{"summary.emMSmix"} returned by \code{\link{summary.emMSmix}}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#'
#' @rdname summary.emMSmix
#'
#' @export print.summary.emMSmix
#' @export
#'
print.summary.emMSmix <- function(x, ...) {
  summary.emMSmix_out <- x

  if (!is(summary.emMSmix_out, "summary.emMSmix")) {
    stop("The function requires an object of S3 class 'summary.emMSmix' as its first argument.\n")
  }

  cat("\nCall:\n", paste(deparse(summary.emMSmix_out$call), sep = "\n", collapse = "\n"),
      "\n\n",
      sep = ""
  )
  cat("-----------------------------\n")
  cat("--- MLE of the parameters ---\n")
  cat("-----------------------------\n")
  cat("\n")
  cat("Component-specific consensus rankings:\n")
  print(summary.emMSmix_out$modal_rankings)
  cat("\n")
  cat("Component-specific consensus orderings:\n")
  print(summary.emMSmix_out$modal_orderings)
  cat("\n")
  cat("Component-specific precisions:\n")
  print(summary.emMSmix_out$theta)
  cat("\n")
  cat("Mixture weights:\n")
  print(summary.emMSmix_out$weights)
  cat("\n")
  invisible(x)
}


# plot.emMSmix ----
#' Plot the MLEs for the fitted mixture of Mallows models with Spearman distance
#'
#' @description \code{plot} method for class \code{"emMSmix"}.
#'
#'
#' @param x An object of class \code{"emMSmix"} returned by \code{\link{fitMSmix}}.
#' @param max_scale_w Positive scalar: maximum magnification of the dots in the bump plot, set proportional to the MLEs of the weights. Defaults to 20.
#' @param mar_lr Numeric: margin for the left and right side of the plot. Defaults to 0.4.
#' @param mar_tb Numeric: margin for the bottom and top side of the plot. Defaults to 0.2.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @return A list of 2 labelled plots, namely: i) \code{bump_plot}: a bump plot comparing the component-specific consensus rankings of the fitted mixture of Mallows models with Spearman distance (the size of the dots of each consensus ranking is proportional to the weight of the corresponding component); and ii) \code{est_clust_prob}: a heatmap of the estimated component membership probabilities is returned when \code{n_clust > 1}, otherwise \code{NULL}.
#'
#' @references
#'
#' Sjoberg D (2020). ggbump: Bump Chart and Sigmoid Curves. R package version 0.1.10. \url{https://CRAN.R-project.org/package=ggbump}.
#'
#' Wickham H et al. (2019). Welcome to the tidyverse. \emph{Journal of Open Source Software}, \bold{4}(43), 1686, DOI: 10.21105/joss.01686.
#'
#' Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. ISBN 978-3-319-24277-4, \url{https://ggplot2.tidyverse.org}.
#'
#'
#' @seealso \code{\link{fitMSmix}}, \code{\link{summary.emMSmix}}
#'
#' @examples
#' ## Example 1. Fit and plot a 3-component mixture of Mallows models with Spearman distance
#' ## to the Antifragility dataset.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' set.seed(123)
#' mms_fit <- fitMSmix(rankings = r_antifrag, n_clust = 3, n_start = 10)
#' p_mms_fit <- plot(mms_fit)
#' p_mms_fit$bump_plot()
#' p_mms_fit$est_clust_prob()
#'
#' @export plot.emMSmix
#' @export
#'
plot.emMSmix <- function(x, max_scale_w = 20, mar_lr = 0.4, mar_tb = 0.2, ...) {
  emMSmix_out <- x

  if (!is(emMSmix_out, "emMSmix")) {
    stop("The function requires an object of S3 class 'emMSmix' as its first argument.\n")
  }

  n_clust <- length(emMSmix_out$mod$weights)
  clusters <- 1:n_clust
  positions <- c(emMSmix_out$mod$rho)
  n_items <- ncol(emMSmix_out$mod$rho)
  item_names <- colnames(emMSmix_out$mod$rho)
  items <- rep(item_names, each = n_clust)

  df_bump <- data.frame(x = clusters, y = positions, group = items)
  df_bump_min <- df_bump %>% filter(x == min(x))
  df_bump_max <- df_bump %>% filter(x == max(x))

  plot_list <- list()

  ggp_bump_plot <- ggplot(df_bump, aes(x = x, y = .data$y, color = .data$group)) +
    geom_bump(linewidth = 1) +
    geom_point(size = rep(max_scale_w * emMSmix_out$mod$weights, n_items)) +
    geom_text(
      data = df_bump_min, aes(x = x - mar_tb, label = .data$group, y = .data$y, color = .data$group),
      size = 3, hjust = 1
    ) +
    geom_text(
      data = df_bump_max, aes(x = x + mar_tb, label = .data$group, y = .data$y, color = .data$group),
      size = 3, hjust = 0
    ) +
    scale_x_continuous(
      limits = c(1 - mar_lr, n_clust + mar_lr),
      breaks = seq(1, n_clust, 1)
    ) +
    labs(y = "Rank", x = "Group") +
    scale_y_reverse(
      limits = c(n_items + mar_tb, 1 - mar_tb),
      breaks = seq(1, n_items, 1),
      labels = seq(1, n_items, 1)
    ) +
    ggtitle(paste("Modal orderings of the", n_clust, "groups")) +
    theme(legend.position = "none")

  plot_list$bump_plot <- suppress_ggplot(ggp_bump_plot)

  if (n_clust > 1) {
    plot_list$est_clust_prob <- function() {
      colors_image <- colorRampPalette(brewer.pal(9, "GnBu"))(50)
      chiplotto <- emMSmix_out$mod$z_hat[sort(emMSmix_out$mod$map_classification, index.return = TRUE)$ix, ]
      N <- ncol(chiplotto)
      n <- nrow(chiplotto)
      oldpar <- par(mar = c(3.1, 4.1, 2.1, 8.1))
      on.exit(par(oldpar))
      image(chiplotto, axes = FALSE, main = "Estimated probabilities of cluster membership",
            col = colors_image)
      mtext(text = paste0("Group", 1:N), side = 2, line = 0.6,
            at = seq(0, 1, 1 / (N - 1)), cex = 0.8, las = 2)
      if (n <= 100) {
        ini <- 1 / (n - 1)
        atSeq <- seq(0, 1, ini)
        mtext(text = sort(emMSmix_out$mod$map_classification, index.return = TRUE)$ix,
              side = 1, line = 0.3, at = atSeq, cex = 0.5, las = 2)
      }
      mtext(paste0("Sample units"), side = 1, line = 1.5, cex = 0.8)
      oldpar2 <- par(mar = c(3.1, 4.1, 2.1, 0))
      on.exit(par(oldpar2))
      image.plot(chiplotto, col = colors_image, legend.only = TRUE, horizontal = FALSE)
    }
  }else{
    plot_list$est_clust_prob <- function() NULL
  }

  return(invisible(plot_list))
}

# bootstrapMSmix ----
#' Bootstrap confidence intervals for the fitted mixture of Mallows models with Spearman distance
#'
#' @description Return the bootstrap confidence intervals for the parameters of a mixture of Mallows models with Spearman distance fitted on partial rankings.
#'
#' @details
#' When \code{n_clust = 1}, two types of bootstrap are available: 1) \code{type = "non-parametric"} (default);
#' \code{type = "parametric"}, where the latter supports full rankings only.
#'
#' When \code{n_clust > 1}, two types of bootstrap are available: 1) \code{type = "soft"} (default), which is
#' the soft-separated bootstrap (Crispino et al., 2025+) and returns confidence intervals for all
#' the parameters of the mixture of Mallows models with Spearman distance; 2) \code{type = "separated"}, which is the separated bootstrap
#' (Taushanov and Berchtold, 2019) and returns bootstrap samples for the component-specific
#' consensus rankings and precisions.
#'
#' @param object An object of class \code{"emMSmix"} returned by \code{\link{fitMSmix}}.
#' @param n_boot Number of desired bootstrap samples. Defaults to 50.
#' @param type Character indicating which bootstrap method must be used. Available options are: \code{"non-parametric"} or \code{"parametric"} for the \eqn{G=1} case, and \code{"soft"} or \code{"separated"} for the \eqn{G>1} case. Defaults to \code{"non-parametric"} when \code{n_clust = 1} and to \code{"soft"} when \code{n_clust > 1}. See Details.
#' @param conf_level Numeric: value in the interval (0,1] indicating the desired confidence level of the interval estimates. Defaults to 0.95.
#' @param all Logical: whether the bootstrap samples of the MLEs for all the parameters must be returned. Defaults to \code{FALSE}.
#' @param n_start Number of starting points for the MLE on each bootstrap sample. Defaults to 10.
#' @param parallel Logical: whether parallelization over multiple initializations of the EM algorithm must be used. Used when \code{rankings} contains some partial rankings. Defaults to \code{FALSE}.
#'
#' @return
#' An object of class \code{"bootMSmix"}, namely a list with the following named components:
#'    \describe{
#'      \item{\code{itemwise_ci_rho}}{Character \eqn{G}\eqn{\times}{x}\eqn{n} matrix with the bootstrap itemwise confidence intervals for the component-specific consensus rankings.}
#'      \item{\code{ci_boot_theta}}{Numeric \eqn{G}\eqn{\times}{x}\eqn{2} matrix with the bootstrap confidence intervals for the component-specific precisions.}
#'      \item{\code{ci_boot_weights}}{Numeric \eqn{G}\eqn{\times}{x}\eqn{2} matrix with the bootstrap confidence intervals for the mixture weights. Returned when \code{n_clust > 1} and \code{type = "soft"}, otherwise \code{NULL}.}
#'      \item{\code{boot}}{List containing all the \code{n_boot} bootstrap MLEs. Returned when \code{all = TRUE}, otherwise \code{NULL}.}
#'    }
#'
#' The \code{boot} sublist contains the following named components:
#'      \describe{
#'      \item{\code{rho_boot}}{List of length \code{n_clust}. Each element is an integer \code{n_boot} \eqn{\times}{x} \code{n_items} matrix with rows containing the bootstrap MLEs of a component-specific consensus ranking.}
#'      \item{\code{theta_boot}}{Numeric \code{n_boot}\eqn{\times}{x} \code{n_clust} matrix with the bootstrap MLEs of the component-specific precision parameters in each row.}
#'      \item{\code{weights_boot}}{Numeric \code{n_boot}\eqn{\times}{x} \code{n_clust} matrix with the bootstrap MLEs of the mixture weights in each row. Returned when \code{n_clust > 1} and \code{type = "soft"}, otherwise \code{NULL}.}
#'      }
#'
#'
#' @references
#'
#' Crispino M, Mollica C and Modugno L (2025+). MSmix: An R Package for clustering partial rankings via mixtures of Mallows Models with Spearman distance. \emph{(submitted)}
#'
#' Taushanov Z and Berchtold A (2019). Bootstrap validation of the estimated parameters in mixture models used for clustering. \emph{Journal de la société française de statistique}, \bold{160}(1).
#'
#' Efron B (1982). The Jackknife, the Bootstrap, and Other Resampling Plans. Philadelphia, \emph{Pa. :Society for Industrial and Applied Mathematics}.
#'
#'
#' @examples
#'
#' ## Example 1. Compute the bootstrap 95% confidence intervals for the Antifragility dataset.
#' # Let us assume no clusters.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' set.seed(12345)
#' fit <- fitMSmix(rankings = r_antifrag, n_clust = 1, n_start = 1)
#' # Apply non-parametric bootstrap procedure.
#' set.seed(12345)
#' boot_np <- bootstrapMSmix(object = fit, n_boot = 200)
#' print(boot_np)
#' # Apply parametric bootstrap procedure and set all = TRUE
#' # to return the bootstrap MLEs of the consensus ranking.
#' set.seed(12345)
#' boot_p <- bootstrapMSmix(object = fit, n_boot = 200,
#'                        type = "parametric", all = TRUE)
#' print(boot_p)
#' # Plot the bootstrap estimates.
#' p_boot_p <- plot(boot_p)
#' p_boot_p$rho_heatmap()
#' p_boot_p$theta_density()
#'
#' ## Example 2. Compute the bootstrap 95% confidence intervals for the Antifragility dataset.
#' # Let us assume two clusters.
#' r_antifrag <- ranks_antifragility[, 1:7]
#' set.seed(12345)
#' fit <- fitMSmix(rankings = r_antifrag, n_clust = 2, n_start = 20)
#' # Apply soft bootstrap procedure and set all = TRUE
#' # to return the bootstrap MLEs of the consensus ranking.
#' set.seed(12345)
#' boot_soft <- bootstrapMSmix(object = fit, n_boot = 500,
#'                       n_start = 20, all = TRUE)
#' print(boot_soft)
#' # Plot the bootstrap estimates.
#' p_boot_soft <- plot(boot_soft)
#' p_boot_soft$rho_heatmap[[1]]()
#' p_boot_soft$rho_heatmap[[2]]()
#' p_boot_soft$theta_density()
#' p_boot_soft$weights_density()
#' # Apply separated bootstrap and compare results.
#' set.seed(12345)
#' boot_sep <- bootstrapMSmix(object = fit, n_boot = 500,
#'                      n_start = 20, type = "separated", all = TRUE)
#' print(boot_sep)
#' p_boot_sep <- plot(boot_sep)
#' p_boot_sep$rho_heatmap[[1]]()
#' p_boot_sep$rho_heatmap[[2]]()
#' p_boot_sep$theta_density()
#' print(boot_soft)
#' print(boot_sep)
#'
#'
#' @export
#'
bootstrapMSmix <- function(object,
                           n_boot = 50,
                           type = (if(object$em_settings$n_clust == 1) "non-parametric" else "soft"),
                           conf_level = 0.95,
                           all = FALSE,
                           n_start = 10,
                           parallel = FALSE) {

  emMSmix_out <- object

  if (!is(emMSmix_out, "emMSmix")) {
    stop("The function requires an object of S3 class 'emMSmix' as its first argument.\n")
  }

  if(conf_level <= 0 | conf_level > 1){
    stop("The argument 'conf_level' must a value in the interval (0,1].")
  }

  rankings <- emMSmix_out$em_settings$rankings
  item_names <- colnames(rankings)
  n_items <- ncol(rankings)

  mc_em <- emMSmix_out$em_settings$mc_em
  n_clust <- emMSmix_out$em_settings$n_clust
  n_iter <- emMSmix_out$em_settings$n_iter
  theta_max <-  emMSmix_out$em_settings$theta_max
  theta_tune <-  emMSmix_out$em_settings$theta_tune
  eps <-  emMSmix_out$em_settings$eps
  theta_tol <-  emMSmix_out$em_settings$theta_tol


  if (n_clust == 1) {
    out <- homo_bootstrapMSmix(
      rankings = rankings,
      n_boot = n_boot,
      type = type,
      rho_mle = emMSmix_out$mod$rho,
      theta_mle = emMSmix_out$mod$theta,
      n_start = n_start,
      mc_em = mc_em,
      item_names = item_names,
      parallel = parallel,
      theta_max = theta_max,
      theta_tune = theta_tune,
      n_iter = n_iter,
      eps = eps,
      theta_tol = theta_tol
    )

  } else {
    out <- hetero_bootstrapMSmix(
      rankings = rankings,
      n_boot = n_boot,
      type = type,
      z_hat = emMSmix_out$mod$z_hat,
      classification = emMSmix_out$mod$map_classification,
      n_start = n_start,
      mc_em = mc_em,
      item_names = item_names,
      parallel = parallel,
      theta_max = theta_max,
      theta_tune = theta_tune,
      n_iter = n_iter,
      eps = eps,
      theta_tol = theta_tol
    )
  }

  # BUILD CONFIDENCE INTERVALS
  # RHO
  ci_rho <- matrix(NA, ncol = n_items, nrow = n_clust)
  for (g in 1:n_clust) ci_rho[g, ] <- itemwise_rank_hdi(out$rho_boot[[g]], prob_level = conf_level)$HPD_set

  colnames(ci_rho) <- item_names
  rownames(ci_rho) <- paste0("Group", 1:n_clust)

  # THETA
  ci_theta <- matrix(NA, ncol = 2, nrow = n_clust)

  alpha <- 1 - conf_level

  for (g in 1:n_clust) {
    theta_ord_sorted <- sort(out$theta_boot[, g])
    ci_theta[g, ] <- c(
      theta_ord_sorted[ceiling(n_boot * alpha/2)],
      theta_ord_sorted[ceiling(n_boot * (1 - alpha/2))]
    )
  }
  colnames(ci_theta) <- c("lower", "upper")
  rownames(ci_theta) <- paste0("Group", 1:n_clust)

  # WEIGHTS
  if ((n_clust > 1) & (type == "soft")) {
    ci_weights <- matrix(NA, ncol = 2, nrow = n_clust)
    for (g in 1:n_clust) {
      weights_ord_sorted <- sort(out$weights_boot[, g])
      ci_weights[g, ] <- c(
        weights_ord_sorted[ceiling(n_boot * alpha/2)],
        weights_ord_sorted[ceiling(n_boot * (1 - alpha/2))]
      )
    }
    colnames(ci_weights) <- c("lower", "upper")
    rownames(ci_weights) <- paste0("Group", 1:n_clust)
  } else {
    ci_weights <- NULL
  }

  out_boot <- list(itemwise_ci_rho = ci_rho,
                   ci_boot_theta = ci_theta,
                   ci_boot_weights = ci_weights,
                   conf_level = conf_level,
                   boot = (if (all) out else NULL))

  class(out_boot) <- "bootMSmix"

  message("Use function plot() to visualize the object of class 'bootMSmix'.\n")

  return(out_boot)

}

# homo_bootstrapMSmix ----
#/' Utility for the exported bootstrapMSmix
#/'
#/' Return the bootstrap MLEs for the parameters of a Mallows model with Spearman distance fitted on partial rankings (homogeneous case \eqn{G=1}).
#/'
#/' Two types of bootstrap are available: 1) \code{type = "non-parametric"} (default); \code{type = "parametric"}, where the latter supports full rankings only.
#/'
#/' @keywords internal
#/' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#/' @param n_boot Number of desired bootstrap samples.
#/' @param type Character indicating which bootstrap method must be used. Available options are: \code{"non-parametric"} (default) or \code{"parametric"}.
#/' @param rho_mle Integer \eqn{1}\eqn{\times}{x}\eqn{n} matrix with the MLE of the consensus ranking.
#/' @param theta_mle Numeric: the MLE of the precision parameter.
#/' @param n_start Number of starting points for the MLE on each bootstrap sample.
#/' @param mc_em Logical: whether the Monte Carlo EM algorithm must be used for MLE on partial rankings completion. Ignored when \code{rankings} does not contain any partial sequence.
#/' @param item_names Character vector with the names to be used for the items.
#/' @param parallel Logical: whether parallelization over multiple initializations of the EM algorithm must be used. Used when \code{rankings} contains some partial rankings.
#/' @param theta_max Positive upper bound for the precision parameters.
#/' @param theta_tune Positive tuning constant affecting the precision parameters in the Monte Carlo step.
#/' @param n_iter Maximum number of EM iterations.
#/' @param eps Positive tolerance value for the convergence of the EM algorithm.
#/' @param theta_tol Positive convergence tolerance for the M-step of the precision parameters.
#/'
#/' @return
#/' A list with the following named components:
#/'      \describe{
#/'      \item{\code{rho_boot}}{List of length 1 with the bootstrap MLEs of the consensus ranking collected in the rows of an integer \code{n_boot} \eqn{\times}{x} \code{n_items} matrix.}
#/'      \item{\code{theta_boot}}{Numeric \code{n_boot}\eqn{\times}{x} \code{1} matrix with the bootstrap MLEs of the precision parameter in each row.}
#/'      }
#/'
homo_bootstrapMSmix <- function(rankings,
                                n_boot,
                                type,
                                rho_mle,
                                theta_mle,
                                n_start,
                                mc_em,
                                item_names,
                                parallel,
                                theta_max,
                                theta_tune,
                                n_iter,
                                eps,
                                theta_tol) {


  if (!(type%in%c("parametric","non-parametric"))){
    stop("Only parametric and non-parametric bootstrap types are available with one mixture component.\n")
  }

  n_items <- ncol(rankings)
  N <- nrow(rankings)

  check_na <- is.na(rankings)
  partial <- any(check_na)

  if(partial){
    message("The dataset contains partial rankings. Therefore, the bootstrap procedure may be slow.\n")
  }

  cardinalities <- suppressMessages(spear_dist_distr(n_items))

  rho_boot <- list()
  rho_boot[[1]] <- matrix(NA, ncol = n_items, nrow = n_boot)
  theta_boot <- rep(NA, n_boot)

  for (h in 1:n_boot) {
    if (h %% 50 == 0) {
      print(paste("Bootstrap iteration", h))
    }

    if (type != "parametric") {
      Rstar <- rankings[sample(x = N, size = N, replace = TRUE), , drop = FALSE]

      if (partial) {

        FIT <- quiet(fitMSmix(
          rankings = Rstar,
          n_clust = 1,
          n_start = n_start,
          mc_em = mc_em,
          parallel = parallel,
          theta_max = theta_max,
          theta_tune = theta_tune,
          n_iter = n_iter,
          eps = eps,
          theta_tol = theta_tol
        ))

        rho_boot[[1]][h, ] <- FIT$mod$rho
        theta_boot[h] <- FIT$mod$theta
      } else {
        rho_boot[[1]][h, ] <- rank(colMeans(Rstar), ties.method = "random")
        rhs <- mean(compute_rank_distance(Rstar, rho_boot[[1]][h, ], "spearman"))
        theta_boot[h] <- Mstep_theta(theta_max = theta_max, n_items = n_items,
                                     cardinalities = cardinalities,
                                     rhs = rhs, theta_tol = theta_tol)
      }
    } else {
      if (partial) {
        stop("Only non-parametric bootstrap is available for partial rankings.\n")
      } else {

        Rstar <- quiet(rMSmix(
          sample_size = N,
          n_items = n_items,
          n_clust = 1,
          rho = rho_mle,
          theta = theta_mle,
          mh = T
        )$samples)

        rho_boot[[1]][h, ] <- rank(colMeans(Rstar), ties.method = "random")
        rhs <- mean(compute_rank_distance(Rstar, rho_boot[[1]][h, ], "spearman"))
        theta_boot[h] <- Mstep_theta(theta_max = theta_max, n_items = n_items,
                                     cardinalities = cardinalities, rhs = rhs,
                                     theta_tol = theta_tol)
      }
    }
  }

  colnames(rho_boot[[1]]) <- item_names
  names(rho_boot) <- paste0("Group", 1)

  theta_boot <- matrix(theta_boot, ncol = 1)

  out <- list(rho_boot = rho_boot, theta_boot = theta_boot)

  return(out)
}


# hetero_bootstrapMSmix ----
#/' Utility for the exported bootstrapMSmix
#/'
#/' Return the bootstrap MLEs for the parameters of a mixture of Mallows model with Spearman distance fitted on partial rankings (heterogeneous case \eqn{G>1}).
#/'
#/' Two types of bootstrap are available: 1) \code{type = "soft"} (default), which is
#/' the soft-separated bootstrap (Crispino et al., 2025+) and returns confidence intervals for all
#/' the parameters of the mixture of Mallows models with Spearman distance; 2) \code{type = "separated"}, which is the separated bootstrap
#/' (Taushanov and Berchtold, 2019) and returns bootstrap samples for the component-specific
#/' consensus rankings and precisions.
#/'
#/' @keywords internal
#/' @param rankings Integer \eqn{N}\eqn{\times}{x}\eqn{n} matrix or data frame with partial rankings in each row. Missing positions must be coded as \code{NA}.
#/' @param n_boot Number of desired bootstrap samples.
#/' @param type Character indicating which bootstrap method must be used. Available options are: \code{"soft"} (default) or \code{"separated"}.
#/' @param z_hat Numeric \eqn{N}\eqn{\times}{x}\eqn{G} matrix of the estimated posterior component membership probabilities.
#/' @param classification Integer vector of \eqn{N} mixture component memberships based on the MAP allocation from the \code{z_hat} matrix.
#/' @param n_start Number of starting points for the MLE on each bootstrap sample.
#/' @param mc_em Logical: whether the Monte Carlo EM algorithm must be used for MLE on partial rankings completion. Ignored when \code{rankings} does not contain any partial sequence.
#/' @param item_names Character vector with the names to be used for the items.
#/' @param parallel Logical: whether parallelization over multiple initializations of the EM algorithm must be used. Used when \code{rankings} contains some partial rankings.
#/' @param theta_max Positive upper bound for the precision parameters.
#/' @param theta_tune Positive tuning constant affecting the precision parameters in the Monte Carlo step.
#/' @param n_iter Maximum number of EM iterations.
#/' @param eps Positive tolerance value for the convergence of the EM algorithm.
#/' @param theta_tol Positive convergence tolerance for the M-step of the precision parameters.
#/'
#/' @return
#/' A list with the following named components:
#/'      \describe{
#/'      \item{\code{rho_boot}}{List of length \code{n_clust}. Each element is an integer \code{n_boot} \eqn{\times}{x} \code{n_items} matrix with rows containing the bootstrap MLEs of a component-specific consensus ranking.}
#/'      \item{\code{theta_boot}}{Numeric \code{n_boot}\eqn{\times}{x} \code{n_clust} matrix with the bootstrap MLEs of the component-specific precision parameters in each row.}
#/'      \item{\code{weights_boot}}{Numeric \code{n_boot}\eqn{\times}{x} \code{n_clust} matrix with the bootstrap MLEs of the mixture weights in each row. Returned when \code{type = "soft"}, otherwise \code{NULL}.}
#/'      }
#/'
hetero_bootstrapMSmix <- function(rankings,
                                  n_boot,
                                  type,
                                  z_hat,
                                  classification,
                                  n_start,
                                  mc_em,
                                  item_names,
                                  parallel,
                                  theta_max,
                                  theta_tune,
                                  n_iter,
                                  eps,
                                  theta_tol) {


  if (!(type%in%c("soft","separated"))){
    stop("Only soft and separated bootstrap types are available with multiple mixture components.\n")
  }


  n_items <- ncol(rankings)
  N <- nrow(rankings)
  n_clust <- ncol(z_hat)

  check_na <- is.na(rankings)
  partial <- any(check_na)

  if(partial){
    message("The dataset contains partial rankings. Therefore, the bootstrap procedure may be slow.\n")
  }


  cardinalities <- suppressMessages(spear_dist_distr(n_items))

  rho_boot <- rep(list(matrix(NA, nrow = n_boot, ncol = n_items,
                              dimnames = list(NULL, item_names))), n_clust)
  names(rho_boot) <- paste0("Group", 1:n_clust)

  theta_boot <- matrix(NA, nrow = n_boot, ncol = n_clust)

  if (type != "soft") {
    Rg <- split(x = as.data.frame(rankings), f = classification)
    Rg <- lapply(Rg, as.matrix)
    freq <- tabulate(classification, nbins = n_clust)
  } else {
    weights_boot <- matrix(NA, nrow = n_boot, ncol = n_clust)
  }


  for (h in 1:n_boot) {
    if (h %% 50 == 0) {
      print(paste("Bootstrap iteration", h))
    }

    if (type != "soft") {
      for (g in 1:n_clust) {
        Rstar <- Rg[[g]][sample(x = freq[g], size = freq[g], replace = TRUE), , drop = FALSE]

        if (partial) {
          FIT <- quiet(fitMSmix(
            rankings = Rstar,
            n_clust = 1,
            n_start = n_start,
            mc_em = mc_em,
            parallel = parallel,
            theta_max = theta_max,
            theta_tune = theta_tune,
            n_iter = n_iter,
            eps = eps,
            theta_tol = theta_tol
          ))
          rho_boot[[g]][h, ] <- FIT$mod$rho
          theta_boot[h, g] <- FIT$mod$theta
        } else {
          rho_boot[[g]][h, ] <- rank(colMeans(Rstar), ties.method = "random")
          rhs <- mean(compute_rank_distance(Rstar, rho_boot[[g]][h, ], "spearman"))
          theta_boot[h, g] <- Mstep_theta(theta_max = theta_max,
                                          n_items = n_items,
                                          cardinalities = cardinalities,
                                          rhs = rhs, theta_tol = theta_tol)
        }
      }
    } else {
      classification <- apply(z_hat, 1, sample, size = 1, x = 1:n_clust, replace = TRUE)
      Rg <- split(x = as.data.frame(rankings), f = classification)
      Rg <- lapply(Rg, as.matrix)
      freq <- tabulate(classification, nbins = n_clust)
      weights_boot[h, ] <- freq / N

      for (g in 1:n_clust) {
        Rstar <- Rg[[g]][sample(x = freq[g], size = freq[g], replace = TRUE), , drop = FALSE]

        if (partial) {
          FIT <- quiet(fitMSmix(
            rankings = Rstar,
            n_clust = 1,
            n_start = n_start,
            mc_em = mc_em,
            parallel = parallel,
            theta_max = theta_max,
            theta_tune = theta_tune,
            n_iter = n_iter,
            eps = eps,
            theta_tol = theta_tol
          ))
          rho_boot[[g]][h, ] <- FIT$mod$rho
          theta_boot[h, g] <- FIT$mod$theta
        } else {
          rho_boot[[g]][h, ] <- rank(colMeans(Rstar), ties.method = "random")
          rhs <- mean(compute_rank_distance(Rstar, rho_boot[[g]][h, ], "spearman"))
          theta_boot[h, g] <- Mstep_theta(theta_max = theta_max,
                                          n_items = n_items, cardinalities = cardinalities,
                                          rhs = rhs, theta_tol = theta_tol)
        }
      }
    }
  }

  out <- list(rho_boot = rho_boot, theta_boot = theta_boot,
              weights_boot = (if (type == "soft") weights_boot else NULL))

  return(out)
}

# print.bootMSmix ----
#' Print of the bootstrap confidence intervals for the fitted mixture of Mallows models with Spearman distance
#'
#' @description \code{print} method for class \code{"bootMSmix"}.
#'
#'
#' @param x An object of class \code{"bootMSmix"} returned by \code{\link{bootstrapMSmix}}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @rdname bootMSmix
#'
#' @export print.bootMSmix
#' @export
#'
print.bootMSmix <- function(x, ...) {

  bootMSmix_out <- x

  if (!is(bootMSmix_out, "bootMSmix")) {
    stop("The function requires an object of S3 class 'bootMSmix' as its first argument.\n")
  }

  cat(paste0("Bootstrap itemwise ", 100*bootMSmix_out$conf_level, "%CIs for the consensus rankings:\n"))
  cat("\n")
  print(bootMSmix_out$itemwise_ci_rho)
  cat("\n")
  cat("\n")
  cat(paste0("Bootstrap ", 100*bootMSmix_out$conf_level, "%CIs for the precisions:\n"))
  cat("\n")
  print(round(bootMSmix_out$ci_boot_theta,3))
  cat("\n")
  if(!is.null(bootMSmix_out$ci_boot_weights)){
  cat("\n")
  cat(paste0("Bootstrap ", 100*bootMSmix_out$conf_level, "%CIs for the mixture weights:\n"))
  cat("\n")
  print(round(bootMSmix_out$ci_boot_weights,3))
  cat("\n")
  }
  invisible(x)

}

# plot.bootMSmix ----
#' Plot the bootstrap estimates for the fitted mixture of Mallows models with Spearman distance
#'
#' @description \code{plot} method for class \code{"bootMSmix"}.
#'
#'
#' @param x An object of class \code{"bootMSmix"} returned by \code{\link{bootstrapMSmix}}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @return
#' A list of 3 labelled plots, namely: i) \code{rho_heatmap}: a heatmap for the component-specific bootstrap consensus ranking estimates (when \code{n_clust > 1}, this is in turn a list of heatmaps for each consensus ranking estimate); ii) \code{theta_density}: a kernel density plot for the component-specific bootstrap precision estimates; iii) \code{weights_density}: a kernel density plot for the bootstrap mixture weight estimates is returned when \code{n_clust > 1} and the object \code{x} was obtained from the \code{bootstrapMSmix} routine with the argument \code{type = "soft"}, otherwise \code{NULL}.
#'
#' @rdname bootstrapMSmix
#'
#' @export plot.bootMSmix
#' @export
#'

plot.bootMSmix <- function(x, ...) {
  bootMSmix_out <- x

  if (!is(bootMSmix_out, "bootMSmix")) {
    stop("The function requires an object of S3 class 'bootMSmix' as its first argument.\n")
  }

  if (is.null(bootMSmix_out$boot)) {
    stop("Run the 'bootstrapMSmix' function with argument all = TRUE before plotting.\n")
  }

  n_clust <- nrow(bootMSmix_out$ci_boot_theta)
  plot_list <- list()

  if (n_clust == 1) {
    plot_list$rho_heatmap <- function() {
      colors_image <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)
      chiplotto <- itemwise_rank_marginals(rankings = bootMSmix_out$boot$rho_boot[[1]])
      N <- dim(chiplotto)[2]
      n <- dim(chiplotto)[1]
      oldpar <- par(mar = c(3.1, 9.1, 2.1, 8.1))
      on.exit(par(oldpar))
      image(chiplotto, axes = FALSE, main = "Bootstrap MLEs of the consensus ranking", col = colors_image)
      mtext(text = colnames(chiplotto), side = 2, line = 0.6, at = seq(0, 1, 1 / (N - 1)), cex = 0.8, las = 2)
      mtext(text = rownames(chiplotto), side = 1, line = 0.3, at = seq(0, 1, 1 / (n - 1)), cex = 0.8, las = 2)
      image.plot(chiplotto, col = colors_image, legend.only = TRUE, horizontal = FALSE)
    }

    plot_list$theta_density <- function() {
      den <- density(bootMSmix_out$boot$theta_boot[,1])
      oldpar4 <- par(mar=rep(4,4))
      on.exit(par(oldpar4))
      plot(den, lwd = 2,
           col = "darkblue",
           main = "Bootstrap MLEs of the precision parameter",
           xlab = expression(theta), ylab = "Density")
      polygon(den, col = adjustcolor("darkblue",0.5))
      legend('topright',legend=(1:n_clust),title='Component',
             fill=adjustcolor("darkblue",0.5),bty='n')
    }

    plot_list$weights_density <- function() NULL

  } else {

    plot_list$rho_heatmap <- list()

    for (g in 1:n_clust) {
      # Heatmap function for each cluster
      plot_list$rho_heatmap[[g]] <- local({
        g_fixed <- g  # Create a local variable to store `g`
        force(g_fixed) # Ensure `g_fixed` is captured at function creation

        function() {
          colors_image <- colorRampPalette(brewer.pal(9, "YlOrRd"))(50)
          chiplotto <- itemwise_rank_marginals(rankings = bootMSmix_out$boot$rho_boot[[g_fixed]])
          N <- dim(chiplotto)[2]
          n <- dim(chiplotto)[1]
          oldpar <- par(mar = c(3.1, 9.1, 2.1, 8.1))
          on.exit(par(oldpar))
          image(chiplotto, axes = FALSE, main = paste("Bootstrap MLEs of consensus ranking for component", g_fixed), col = colors_image)
          mtext(text = colnames(chiplotto), side = 2, line = 0.6, at = seq(0, 1, 1 / (N - 1)), cex = 0.8, las = 2)
          mtext(text = rownames(chiplotto), side = 1, line = 0.3, at = seq(0, 1, 1 / (n - 1)), cex = 0.8, las = 2)
          image.plot(chiplotto, col = colors_image, legend.only = TRUE, horizontal = FALSE)
        }
      })

    }

    # Precisions density plot
    plot_list$theta_density <- function() {
      den <- list()
      dx <- dy <- NULL

      for(g in 1:n_clust){
        den[[g]] <- density(bootMSmix_out$boot$theta_boot[,g])
        dx <-c(dx,den[[g]]$x)
        dy <-c(dy,den[[g]]$y)
      }
      oldpar4 <- par(mar=rep(4,4))
      on.exit(par(oldpar4))
      ramp <- colorRamp(c("darkblue","darkgreen","yellow"))
      ramp2 <- rgb( ramp(seq(0, 1, length.out = n_clust)), maxColorValue = 255)
      plot(den[[1]], lwd = 2,
           col = ramp2[1],ylim=range(dy),xlim=range(dx),
           main = "Bootstrap MLEs of the precision parameters",
           xlab = expression(theta), ylab = "Density")
      polygon(den[[1]], col = adjustcolor(ramp2[1],0.5))
      for(g in 2:n_clust){
        lines(den[[g]], lwd = 2, col = ramp2[g])
        polygon(den[[g]], col = adjustcolor(ramp2[g],0.5))
      }
      legend('topright',legend=(1:n_clust),title='Component',
             fill=adjustcolor(ramp2,0.5),bty='n')
    }

    # Mixture weights density plot
    if (!is.null(bootMSmix_out$boot$weights_boot)) {
      plot_list$weights_density <- function() {
        den2 <- list()
        dx2 <- dy2 <- NULL

        for(g in 1:n_clust){
          den2[[g]] <- density(bootMSmix_out$boot$weights_boot[,g])
          dx2 <-c(dx2,den2[[g]]$x)
          dy2 <-c(dy2,den2[[g]]$y)
        }

        oldpar5 <- par(mar=rep(4,4))
        on.exit(par(oldpar5))
        ramp <- colorRamp(c("darkblue","darkgreen","yellow"))
        ramp2 <- rgb( ramp(seq(0, 1, length.out = n_clust)), maxColorValue = 255)
        plot(den2[[1]], lwd = 2,
             col = ramp2[1],ylim=range(dy2),xlim=range(dx2),
             main = "Bootstrap MLEs of the mixture weights",
             xlab = "weights", ylab = "Density")
        polygon(den2[[1]], col = adjustcolor(ramp2[1],0.5))
        for(g in 2:n_clust){
          lines(den2[[g]], lwd = 2, col = ramp2[g])
          polygon(den2[[g]], col = adjustcolor(ramp2[g],0.5))
        }
        legend('topright',legend=(1:n_clust),title='Component',
               fill=adjustcolor(ramp2,0.5),bty='n')
      }

    }else{
      plot_list$weights_density <- function() NULL
    }

  }

  return(invisible(plot_list))
}

# seMSmix ----
#/' Utility for the exported confintMSmix
#/'
#/' Return the (asymptotic) standard errors of the continuous parameters (component-specific precisions and weights) of a mixture of Mallows models with Spearman distance fitted to full rankings.
#/'
#/' The current implementation of the standard errors assumes that the observed rankings are complete.
#/'
#/' @keywords internal
#/' @param object An object of class \code{"emMSmix"} returned by \code{\link{fitMSmix}}.
#/'
#/' @return A list with the following named components:
#/' \describe{
#/'    \item{\code{se_theta}}{Numeric vector with the \eqn{G} standard errors of the component-specific precision parameters.}
#/'    \item{\code{se_weights}}{Numeric vector with the standard errors for the mixture weights (when \eqn{G>1}), otherwise \code{NULL}.}
#/' }
#/'
seMSmix <- function(object){
  emMSmix_out <- object
  if (!is(emMSmix_out, "emMSmix")) {
    stop("The function requires an object of S3 class 'emMSmix' as its first argument.\n")
  }
  if (emMSmix_out$partial_data) {
    stop("The function assumes that the fitted dataset is composed of full rankings only.\n")
  }

  rankings <- emMSmix_out$em_settings$rankings
  N <- nrow(rankings)

  rho_mle <- emMSmix_out$mod$rho
  n_items <- ncol(rho_mle)
  dist_mat <- apply(rho_mle,1,spear_dist,rankings=rankings) # N vector (when n_clust=1) or N*n_clust matrix (when n_clust>1)

  theta_mle <- emMSmix_out$mod$theta
  n_clust <- length(theta_mle)
  expect_dist <- sapply(theta_mle,expected_spear_dist, n_items=n_items, log = FALSE) # n_clust vector

  if (n_clust == 1){
    # Score function for theta
    score_theta <- score_funct <- -(dist_mat-expect_dist) # N vector

  }else{
    z_hat_mle <- emMSmix_out$mod$z_hat
    # Score function for theta
    score_theta <- -z_hat_mle*(dist_mat-matrix(expect_dist, nrow=N, ncol=n_clust, byrow=TRUE)) # N*n_clust matrix
    # Score function for weights
    weights_mle <- emMSmix_out$mod$weights
    score_weights <- t(t(z_hat_mle)/weights_mle)[,-n_clust,drop=FALSE] # N*(n_clust-1) matrix
    score_funct <- cbind(score_theta, score_weights) # N times 2*n_clust-1 matrix

  }

  # Approximate empirical information matrix
  approx_emp_info <- t(score_funct)%*%score_funct # 1 times 1 matrix (when n_clust=1) 2*n_clust-1 times 2*n_clust-1 matrix (when n_clust>1)

  # SE
  var_cov_mat <- solve(approx_emp_info)
  var_vec <- diag(var_cov_mat)
  approx_se <- sqrt(var_vec)
  se_theta <- approx_se[1:n_clust]

  if (n_clust > 1){
    if (n_clust == 2){
      se_weights <- rep(approx_se[-c(1:n_clust)],n_clust)
    }else{
      var_cov_weights_mat <- var_cov_mat[-c(1:n_clust),-c(1:n_clust)]
      cov_weights <- var_cov_mat[lower.tri(var_cov_weights_mat)]
      se_weights <- c(approx_se[-c(1:n_clust)],
                      sqrt(sum(diag(var_cov_weights_mat))+2*sum(cov_weights)))
    }
    }

  out <- list(se_theta = se_theta,
              se_weights = (if (n_clust > 1) se_weights else NULL))

  return(out)

}
# list with: a positive scalar and a NULL element (when n_clust=1) or
#            two vectors of length n_clust (when n_clust>1)


# confintMSmix ----
#' Asymptotic confidence intervals for the fitted mixture of Mallows models with Spearman distance
#'
#' @description Return the asymptotic confidence intervals of the continuous parameters (component-specific precisions and weights) of a mixture of Mallows models with Spearman distance fitted to full rankings.
#'
#' @details The current implementation of the asymptotic confidence intervals assumes that the observed rankings are complete.
#'
#' @param object An object of class \code{"emMSmix"} returned by \code{\link{fitMSmix}}.
#' @param conf_level Numeric: value in the interval (0,1] indicating the desired confidence level of the interval estimates. Defaults to 0.95.
#'
#' @return An object of class \code{"ciMSmix"}, namely a list with the following named components:
#' \describe{
#' \item{\code{ci_theta}}{Numeric \eqn{G}\eqn{\times}{x}\eqn{2} matrix with the confidence intervals of the component-specific precision parameters in each row.}
#' \item{\code{ci_weights}}{Numeric \eqn{G}\eqn{\times}{x}\eqn{2} matrix with the confidence intervals of the mixture weights in each row (when \eqn{G>1}), otherwise \code{NULL}.}
#' }
#'
#' @references
#' Crispino M, Mollica C and Modugno L (2025+). MSmix: An R Package for clustering partial rankings via mixtures of Mallows Models with Spearman distance. \emph{(submitted)}
#'
#' Marden JI (1995). Analyzing and modeling rank data. \emph{Monographs on Statistics and Applied Probability} (64). Chapman & Hall, ISSN: 0-412-99521-2. London.
#'
#' McLachlan G and Peel D (2000). Finite Mixture Models. \emph{Wiley Series in Probability and Statistics}, John Wiley & Sons.
#'
#'
#' @examples
#'
#' ## Example 1. Simulate rankings from a 2-component mixture of Mallows models
#' ## with Spearman distance.
#' set.seed(123)
#' d_sim <- rMSmix(sample_size = 75, n_items = 8, n_clust = 2)
#' rankings <- d_sim$samples
#' # Fit the basic Mallows model with Spearman distance.
#' set.seed(123)
#' fit1 <- fitMSmix(rankings = rankings, n_clust = 1, n_start = 10)
#' # Compute the asymptotic confidence intervals for the MLEs of the precision.
#' ci95_fit1 <- confintMSmix(object = fit1)
#' print(ci95_fit1)
#' # Fit the true model.
#' set.seed(123)
#' fit2 <- fitMSmix(rankings = rankings, n_clust = 2, n_start = 10)
#' # Compute the asymptotic confidence intervals for the MLEs of the weights and precisions.
#' ci95_fit2 <- confintMSmix(object = fit2)
#' print(ci95_fit2)
#'
#' @export
#'
confintMSmix <- function(object, conf_level = 0.95){

  emMSmix_out <- object
  if (!is(emMSmix_out, "emMSmix")) {
    stop("The function requires an object of S3 class 'emMSmix' as its first argument.\n")
  }
  if (emMSmix_out$partial_data) {
    stop("The function assumes that the fitted dataset is composed of full rankings only.\n")
  }

  theta_mle <- emMSmix_out$mod$theta
  n_clust <- length(theta_mle)

  SE <- seMSmix(object=object)

  z_level <- qnorm((1 - conf_level)/2, lower.tail = FALSE)

  # CI for theta
  ci_t_l <- theta_mle - z_level * SE$se_theta
  if(any(ci_t_l<0)){
    #    warning(paste0("Negative lower bound of the ",100 * conf_level ,"%CI for some precisions"))
    neg_ci_t_l <- which(ci_t_l<0)
    ci_t_l[neg_ci_t_l] <- 0
  }

  ci_t_u <- theta_mle + z_level * SE$se_theta

  ci_theta <- cbind(ci_t_l, ci_t_u)
  colnames(ci_theta) <- c("lower", "upper")
  rownames(ci_theta) <- paste0("Group", 1:n_clust)

  # CI for weights
  if (n_clust > 1) {
    weights_mle <- emMSmix_out$mod$weights
    ci_w_l <- weights_mle - z_level * SE$se_weights
    ci_w_u <- weights_mle + z_level * SE$se_weights
    ci_weights <- cbind(ci_w_l, ci_w_u)
    ci_weights[ci_weights < 0] <- 0
    ci_weights[ci_weights > 1] <- 1
    colnames(ci_weights) <- c("lower", "upper")
    rownames(ci_weights) <- paste0("Group", 1:n_clust)
  }

  out <- list(ci_theta = ci_theta,
              ci_weights = (if (n_clust > 1) ci_weights else NULL),
              conf_level = conf_level)

  class(out) <- "ciMSmix"

  return(out)

}# list with: a 1*2 matrix and a NULL element (when n_clust=1) or
#            two n_clust*2 matrices (when n_clust>1)


# print.ciMSmix ----
#' Print of the asymptotic confidence intervals for the fitted mixture of Mallows models with Spearman distance
#'
#' @description \code{print} method for class \code{"ciMSmix"}.
#'
#'
#' @param x An object of class \code{"ciMSmix"} returned by \code{\link{confintMSmix}}.
#' @param ... Further arguments passed to or from other methods (not used).
#'
#' @rdname confintMSmix
#'
#' @export print.ciMSmix
#' @export
#'
print.ciMSmix <- function(x, ...) {

  confintMSmix_out <- x

  if (!is(confintMSmix_out, "ciMSmix")) {
    stop("The function requires an object of S3 class 'ciMSmix' as its first argument.\n")
  }

  cat(paste0("Asymptotic ", 100*confintMSmix_out$conf_level, "%CIs for the precisions:\n"))
  cat("\n")
  print(round(confintMSmix_out$ci_theta,3))
  cat("\n")

  if(!is.null(confintMSmix_out$ci_weights)){
  cat("\n")
  cat(paste0("Asymptotic ", 100*confintMSmix_out$conf_level, "%CIs for the mixture weights:\n"))
  cat("\n")
  print(round(confintMSmix_out$ci_weights,3))
  cat("\n")}
  invisible(x)

}

# rearrange_output_mix ----
#' Arrange the output of MLE of mixtures of Mallows models with Spearman distance via EM algorithms
#'
#' Arrange the output of the object of class \code{"emMSmix"} according to a given relabelling of the mixture component labels.
#'
#' @param output An object of class \code{"emMSmix"} returned by \code{\link{fitMSmix}}.
#' @param ord Integer vector of length \code{n_clust} with the desired relabelling of the mixture component labels.
#'
#' @export rearrange_output_mix
#' @export
#'


rearrange_output_mix <- function(output, ord){


  if (!is(output, "emMSmix")) {
    stop("The function requires an object of S3 class 'emMSmix' as its first argument.\n")
  }

  output_switched <- output
  output_switched$mod$rho <- output$mod$rho[ord,]
  output_switched$mod$theta <- output$mod$theta[ord]
  output_switched$mod$weights <- output$mod$weights[ord]
  output_switched$mod$z_hat <- output$mod$z_hat[,ord]

  class<-output$mod$map_classification
  n_clust <- output$em_settings$n_clust
  for(k in 1:n_clust) class[which(output$mod$map_classification==ord[k])] <- k

  output_switched$mod$map_classification <- class

  invisible(output_switched)
}
